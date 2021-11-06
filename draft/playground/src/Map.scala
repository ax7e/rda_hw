package hermes

import chisel3._
import chisel3.experimental._
import chisel3.util._

import scala.language.postfixOps

/**
  * @param selfSize the size of the channel mapped
  * @param numInputs number of inputs
  * @param passthroughDims dims in this set will not be processed
  * @param p
  */
class Map(
  selfSize:        Int,
  numInputs:       Int      = 1,
  passthroughDims: Set[Int] = Set()
)(
  implicit p: Parameters)
    extends MultiIOModule {
  def connectParent(parents: Seq[IDChannel]): Unit = {
    if (parents.length != numInputs)
      throw new RuntimeException("parents length mismatch with numInputs")

    parents.zip(selfIDs).foreach { case (p, s) => p <> s }
  }

  /**
    * @param dones signals which indicates whether the following channel is done
    */
  def connectChildDone(dones: Seq[Bool]): Unit = {
    if (dones.length != numInputs)
      throw new RuntimeException("parents length mismatch with numInputs")
    print(dones.length)
    print(numInputs)
    selfDones.zip(dones).foreach { case (p, s) => p <> s }
  }

  val done = IO(Output(Bool()))
  private val chanIOs = for { idx <- 0 until numInputs } yield {
    val chan = Module(new MapChannel(selfSize, passthroughDims contains idx))

    val self = IO(Flipped(Decoupled(ID))).suggestName(f"selfID_$idx")
    self <> chan.selfID
    val child = IO(Decoupled(ID)).suggestName(f"childID_$idx")
    child <> chan.childID

    (self, child, chan.done)
  }
  val (selfIDs, childIDs) = chanIOs.map { case (s, c, _) => (s, c) } unzip

  val selfDones = Seq.fill(numInputs)(IO(Input(Bool())))

  done := chanIOs.map(_._3).foldLeft(true.B)(_ && _) && selfDones.foldLeft(true.B)(_ && _)
}

class MapChannel(selfSize: Int, passthrough: Boolean)(implicit p: Parameters) extends MultiIOModule {

  val idle :: issueCmd :: waitChild :: Nil = Enum(3)
  val selfID                               = IO(Flipped(Decoupled(ID)))
  val childID                              = IO(Decoupled(ID))
  val done                                 = IO(Output(Bool()))

  private val state     = RegInit(idle)
  private val nextIssue = RegInit(ID, 0.U)

  private def increaseIssue = {
    when(nextIssue >= (selfSize - 1).U) {
      nextIssue := 0.U
    }.otherwise {
      nextIssue := nextIssue + 1.U
    }
  }

  switch(state) {
    is(idle) {
      nextIssue := 0.U
      when(selfID.valid) {
        state := issueCmd
      }
    }
    is(issueCmd) {
      when(childID.ready) {
        increaseIssue
        when(nextIssue >= (selfSize - 1).U && !selfID.valid) {
          state := idle
        }
      }.otherwise {
        state := waitChild
      }
    }
    is(waitChild) {
      when(childID.ready) {
        when(nextIssue >= (selfSize - 1).U) {
          state := idle
        }.otherwise {
          state := issueCmd
        }
        increaseIssue
      }
    }
  }

  childID.valid := !(state === idle) && (nextIssue <= (selfSize - 1).U)
  selfID.ready :=
    state === idle ||
      (state === waitChild && nextIssue >= (selfSize - 1).U) ||
      (nextIssue === (selfSize - 1).U)
  done := state === idle

  private val self = RegEnable(selfID.bits, selfID.ready && selfID.valid)
  if (passthrough) {
    childID.bits := self
  } else {
    childID.bits := selfSize.U * self + nextIssue
  }
}
