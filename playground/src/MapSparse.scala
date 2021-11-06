package hermes
import chisel3._
import chisel3.experimental._
import chisel3.util._

import hermes._

import scala.language.postfixOps

/** Map into a sparse dimension.
  * @param inputChannel the contents of this channel will be used to specify the address of position array
  * @param needPositionDims the output of this channel will be the current position in the index array
  * @param passthroughDims do nothing in these channels
  * @return the corresponding index
  */
class MapSparseInReg(
  selfSize:         Int,
  numInputs:        Int      = 1,
  passthroughDims:  Set[Int] = Set(),
  needPositionDims: Set[Int] = Set(),
  inputChannel:     Int
)(
  implicit val p: Parameters)
    extends MultiIOModule
    with HasParent
    with MetadataInReg {

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

  val done                  = IO(Output(Bool()))
  val input                 = IO(Flipped(Decoupled(ID))).suggestName(f"selfID_$inputChannel")
  private val firstIssueReg = RegInit(ID, 0.U)
  private val finalIssueReg = RegInit(ID, 0.U)
  private val inputBufReg   = RegInit(ID, 0.U)

  val idle :: readPos :: issueCmd :: waitChild :: Nil = Enum(4)

  private val stateReg     = RegInit(idle)
  private val nextIssueReg = RegInit(ID, 0.U)
  private val all_ready    = Wire(Bool())

  private def increaseIssue = {
    when(nextIssueReg >= (finalIssueReg - 1.U)) {
      //      nextIssue := 0.U
    }.otherwise {
      nextIssueReg := nextIssueReg + 1.U
    }
  }

  position.a := 0.U

  switch(stateReg) {
    is(idle) {
      nextIssueReg := DontCare
      position.a   := DontCare
      when(input.valid) {
        inputBufReg   := input.bits
        position.a    := input.bits
        firstIssueReg := position.d
        nextIssueReg  := position.d
        stateReg      := readPos
      }
    }
    is(readPos) {
      position.a    := inputBufReg + 1.U
      finalIssueReg := position.d
      stateReg      := issueCmd
    }
    is(issueCmd) {
      when(all_ready) {
        increaseIssue
        when(nextIssueReg >= (finalIssueReg - 1.U) && !input.valid) {
          stateReg := idle
        }.elsewhen(nextIssueReg >= (finalIssueReg - 1.U)) {
          inputBufReg   := input.bits
          position.a    := input.bits
          firstIssueReg := position.d
          nextIssueReg  := position.d
          stateReg      := readPos
        }
      }.otherwise {
        stateReg := waitChild
      }
    }
    is(waitChild) {
      when(all_ready) {
        when(nextIssueReg >= (finalIssueReg - 1.U)) {
          stateReg := idle
        }.otherwise {
          stateReg := issueCmd
        }
        increaseIssue
      }
    }
  }
  index.a := nextIssueReg

  private val chanIOs = for { idx <- 0 until numInputs } yield {
    val needPosition = needPositionDims contains idx
    val passthrough  = passthroughDims contains idx

    val selfID  = if (idx == inputChannel) input else IO(Flipped(Decoupled(ID))).suggestName(f"selfID_$idx")
    val childID = IO(Decoupled(ID)).suggestName(f"childID_$idx")
    val done    = IO(Input(Bool())).suggestName(f"done_$idx")
    childID.valid := ((stateReg === issueCmd) || (stateReg === waitChild)) && (nextIssueReg <= (finalIssueReg - 1.U))
    selfID.ready :=
      stateReg === idle ||
      (stateReg === waitChild && nextIssueReg >= (finalIssueReg - 1.U)) ||
      (nextIssueReg === (finalIssueReg - 1.U))

    val self = RegEnable(selfID.bits, selfID.fire()).suggestName(f"self_$idx")
    if (needPosition) {
      childID.bits := nextIssueReg
    } else if (passthrough) {
      childID.bits := self
    } else {
      childID.bits := selfSize.U * self + index.d
    }
    (selfID, childID, done)
  }
  val (selfIDs, childIDs) = chanIOs.map { case (s, c, _) => (s, c) } unzip
  val selfDone            = chanIOs.map(_._3)

  done      := chanIOs.map(_._3).foldLeft(stateReg === idle)(_ && _)
  all_ready := chanIOs.map(_._2.ready).foldLeft(true.B)(_ && _)
}

class MapSparseInMem(
  selfSize:         Int,
  numInputs:        Int      = 1,
  passthroughDims:  Set[Int] = Set(),
  needPositionDims: Set[Int] = Set(),
  inputChannel:     Int
)(
  implicit val p: Parameters)
    extends MultiIOModule
    with HasParent
    with MetadataInMem {

  def connect(parents: Seq[IDChannel], done: Seq[Bool]): Unit = {
    if (parents.length != numInputs)
      throw new RuntimeException("parents length mismatch with numInputs")

    parents.zip(selfIDs).foreach { case (p, s) => p <> s }
    done.zip(selfDones).foreach { case (p, s) => s <> p }
  }
  def connectParent(parents: Seq[IDChannel]): Unit = {
    if (parents.length != numInputs)
      throw new RuntimeException("parents length mismatch with numInputs")

    parents.zip(selfIDs).foreach { case (p, s) => p <> s }
  }

  val done               = IO(Output(Bool()))
  val input              = IO(Flipped(Decoupled(ID))).suggestName(f"selfID_$inputChannel")
  private val firstIssue = RegInit(ID, 11.U)
  private val finalIssue = RegInit(ID, 0.U)

  object State extends ChiselEnum {
    val idle, readFirst, readPos, issueCmd, waitChild = Value
  }

  import State._

  private val stateReg      = RegInit(idle)
  private val nextIssueReg  = RegInit(ID, 0.U)
  private val allReady      = Wire(Bool())
  private val nextIndexReg  = RegInit(ID, 0.U)
  private val indexAddrReg  = RegInit(ID, 0.U)
  private val indexValidReg = RegInit(0.B)

  private def increaseIssue = {
    when(nextIssueReg >= (finalIssue - 1.U)) {
      //      nextIssue := 0.U
    }.otherwise {
      nextIssueReg := nextIssueReg + 1.U
    }
    when(indexAddrReg < (finalIssue - 1.U)) {
      indexAddrReg := indexAddrReg + 1.U
    }
    indexValidReg := 1.B
  }

  position.a    := 0.U
  indexValidReg := 0.B

  switch(stateReg) {
    is(idle) {
      nextIssueReg := DontCare
      position.a   := DontCare
      when(input.valid) {
        position.a := input.bits
        stateReg   := readFirst
      }
    }
    is(readFirst) {
      firstIssue    := position.d
      indexAddrReg  := position.d
      position.a    := input.bits + 1.U
      stateReg      := readPos
      indexValidReg := 1.B
    }
    is(readPos) {
      finalIssue    := position.d
      stateReg      := issueCmd
      nextIssueReg  := firstIssue
      indexAddrReg  := firstIssue + 1.U
      indexValidReg := 1.B
    }
    is(issueCmd) {
      indexAddrReg := nextIssueReg + 1.U
      when(allReady) {
        increaseIssue
        when(nextIssueReg >= (finalIssue - 1.U) && !input.valid) {
          stateReg := idle
        }.otherwise(when(nextIssueReg >= (finalIssue - 1.U)) {
          position.a := input.bits
          stateReg   := readFirst
        })
      }.otherwise {
        stateReg := waitChild
      }
    }
    is(waitChild) {
      indexAddrReg := nextIssueReg + 1.U
      when(allReady) {
        when(nextIssueReg >= (finalIssue - 1.U)) {
          stateReg := idle
        }.otherwise {
          stateReg := issueCmd
        }
        increaseIssue
      }
    }
  }
  index.a := indexAddrReg
  when(indexValidReg) {
    nextIndexReg := index.d
  }

  private val chanIOs = for { idx <- 0 until numInputs } yield {
    val needPosition = needPositionDims contains idx
    val passThrough  = passthroughDims contains idx

    val selfID  = if (idx == inputChannel) input else IO(Flipped(Decoupled(ID))).suggestName(f"selfID_$idx")
    val childID = IO(Decoupled(ID)).suggestName(f"childID_$idx")
    val done    = IO(Input(Bool())).suggestName(f"done_$idx")
    childID.valid := ((stateReg === issueCmd) || (stateReg === waitChild)) && (nextIssueReg <= (finalIssue - 1.U))
    selfID.ready :=
      stateReg === idle ||
      (stateReg === waitChild && nextIssueReg >= (finalIssue - 1.U)) ||
      (nextIssueReg === (finalIssue - 1.U))

    val self = RegEnable(selfID.bits, selfID.ready && selfID.valid).suggestName(f"self_$idx")
    if (needPosition) {
      childID.bits := nextIssueReg
    } else if (passThrough) {
      childID.bits := self

    } else {
      when(indexValidReg) {
        childID.bits := selfSize.U * self + index.d
      }.otherwise {
        childID.bits := selfSize.U * self + nextIndexReg
      }
    }
    (selfID, childID, done)
  }
  val (selfIDs, childIDs) = chanIOs.map { case (s, c, _) => (s, c) } unzip
  val selfDones           = chanIOs.map(_._3)

  done     := chanIOs.map(_._3).foldLeft(stateReg === idle)(_ && _)
  allReady := chanIOs.map(_._2.ready).foldLeft(true.B)(_ && _)

}
