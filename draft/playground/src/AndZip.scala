package hermes
import chisel3._
import chisel3.experimental._
import chisel3.util._
import hermes._

import scala.language.postfixOps

/**
  * AndZip two sparse dimensions.
  * @param inputChannelL the input channel which specifies the left operand of the matching process
  * @param inputChannelR the input channel which specifies the right operand of the matching process
  * @param passthroughDims set of index which specifies the channel which the andZip address generator does nothing on it.
  * @param needPositionDims set of index which specifies the channel which the andZip address generator outputs position info.
  */
class AndZip(
  selfSize:         Int,
  numInputs:        Int,
  passthroughDims:  Set[Int] = Set(),
  needPositionDims: Set[Int] = Set(),
  inputChannelL:    Int,
  inputChannelR:    Int
)(
  implicit val p: Parameters)
    extends MultiIOModule
    with HasParent {
  if (
    inputChannelL == inputChannelR || inputChannelL < 0 || inputChannelL >= numInputs ||
    inputChannelR < 0 || inputChannelR >= numInputs
  )
    throw new RuntimeException("invalid input channel specification.")
  val inputL = IO(Flipped(Decoupled(ID))).suggestName(f"selfID_${inputChannelL}(inputL)")
  val inputR = IO(Flipped(Decoupled(ID))).suggestName(f"selfID_${inputChannelR}(inputR)")

  val positionL              = IO(Flipped(new RegFileBundle()))
  val indexL                 = IO(Flipped(new RegFileBundle()))
  val positionR              = IO(Flipped(new RegFileBundle()))
  val indexR                 = IO(Flipped(new RegFileBundle()))
  private val finalIssueLReg = RegInit(ID, 0.U)
  private val finalIssueRReg = RegInit(ID, 0.U)
  //Hold the memory location of the start of the pos index
  private val inputLBufReg                            = RegInit(ID, 0.U)
  private val inputRBufReg                            = RegInit(ID, 0.U)
  val idle :: readPos :: idxMatch :: waitChild :: Nil = Enum(4)
  private val stateReg                                = RegInit(idle)
  private val issueLReg                               = RegInit(ID, 0.U)
  private val issueRReg                               = RegInit(ID, 0.U)
  private val allReady                                = Wire(Bool())
  private val incL                                    = Wire(Bool())
  private val incR                                    = Wire(Bool())
  incL := DontCare
  incR := DontCare
  private val chanIOs = for { idx <- 0 until numInputs } yield {
    val passthrough  = passthroughDims contains idx
    val needPosition = needPositionDims contains idx
    val selfID =
      if (idx == inputChannelL)
        inputL
      else (if (idx == inputChannelR)
              inputR
            else IO(Flipped(Decoupled(ID))).suggestName(f"selfID_$idx"))
    val childID = IO(Decoupled(ID)).suggestName(f"childID_$idx")
    val done    = IO(Input(Bool())).suggestName(f"done_$idx")
    childID.valid := (stateReg === idxMatch || stateReg === waitChild) && (indexL.d === indexR.d)
    selfID.ready  := (stateReg =/= readPos) && (stateReg === idle || isEnd)
    val self = RegEnable(selfID.bits, selfID.fire()).suggestName(f"selfID_$idx")
    if (needPosition) {
      if (idx == inputChannelL)
        childID.bits := issueLReg
      else
        childID.bits := issueRReg
    } else if (passthrough) {
      childID.bits := self
    } else {
      childID.bits := selfSize.U * self + indexL.d
    }
    (selfID, childID, done)
  }
  val (selfIDs, childIDs) = chanIOs.map { case (s, c, _) => (s, c) } unzip
  val selfDone            = chanIOs.map(_._3)

  val done = IO(Output(Bool()))
  done     := chanIOs.map(_._3).foldLeft(stateReg === idle)(_ && _)
  allReady := chanIOs.map(_._2.ready).foldLeft(true.B)(_ && _)

  def connectParent(parents: Seq[IDChannel]): Unit = {
    if (parents.length != numInputs)
      throw new RuntimeException("parents length mismatch with numInputs")

    parents.zip(selfIDs).foreach { case (p, s) => p <> s }
  }
  def connectChildDone(done: Seq[Bool]): Unit = {
    if (done.length != numInputs)
      throw new RuntimeException("parents length mismatch with numInputs")

    done.zip(selfDone).foreach { case (p, s) => s <> p }
  }

  private def increaseIssueL = {
    incL := true.B
    when(issueLReg < finalIssueLReg - 1.U) {
      issueLReg := issueLReg + 1.U
    }
  }
  private def increaseIssueR = {
    incR := true.B
    when(issueRReg < finalIssueRReg - 1.U) {
      issueRReg := issueRReg + 1.U
    }
  }
  private def endL = {
    (issueLReg >= finalIssueLReg - 1.U)
  }
  private def endR = {
    issueRReg >= finalIssueRReg - 1.U
  }
  private def readInput = {
    inputLBufReg := inputL.bits
    inputRBufReg := inputR.bits
    positionL.a  := inputL.bits
    positionR.a  := inputR.bits
    issueLReg    := positionL.d
    issueRReg    := positionR.d
    stateReg     := readPos
  }
  private def isEnd = {
    ((endL && incL) || (endR && incR))
  }
  private def checkEnd = {
    when(isEnd) {
      when(inputL.valid && inputR.valid) {
        readInput
        stateReg := readPos
      }.otherwise {
        stateReg := idle
      }
    }
  }

  indexL.a := issueLReg
  indexR.a := issueRReg

  positionL.a := DontCare
  positionR.a := DontCare
  switch(stateReg) {
    is(idle) {
      when(inputL.valid && inputR.valid) {
        readInput
      }
    }
    is(readPos) {
      positionL.a    := inputLBufReg + 1.U
      positionR.a    := inputRBufReg + 1.U
      finalIssueLReg := positionL.d
      finalIssueRReg := positionR.d
      stateReg       := idxMatch
    }
    is(idxMatch) {
      when(indexL.d === indexR.d) {
        when(allReady) {
          increaseIssueL
          increaseIssueR
          checkEnd
        }.otherwise {
          stateReg := waitChild
        }
      }.elsewhen(indexL.d < indexR.d) {
        increaseIssueL
        incR := false.B
        checkEnd
      }.otherwise {
        increaseIssueR
        incL := false.B
        checkEnd
      }
    }
    is(waitChild) {
      when(allReady) {
        increaseIssueL
        increaseIssueR
        checkEnd
      }
    }
  }
}
