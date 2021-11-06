package hermes

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._

package object testing {
  val maxInt = 32767 // for testing (should not exceed 32bit signed when multiplied together)
  val aluLatency = 2

  def fillMem(data: Seq[Int], writer: Valid[MemWriteBundle], clock: Clock)(
    implicit p: Parameters
  ): Unit = {
    writer.valid.poke(true.B)
    data.zipWithIndex map {
      case (v, idx) => (new MemWriteBundle).Lit(_.a -> idx.U, _.d -> v.U)
    } foreach { d =>
      writer.bits.poke(d)
      clock.step()
    }
    writer.valid.poke(false.B)
    clock.step()
  }

  def fillReg(data: Seq[Int], writer: Valid[RegFileWriteBundle], clock: Clock)(
    implicit p: Parameters
  ): Unit = {
    writer.valid.poke(true.B)
    data.zipWithIndex map {
      case (v, idx) => (new RegFileWriteBundle).Lit(_.a -> idx.U, _.d -> v.U)
    } foreach { d =>
      writer.bits.poke(d)
      clock.step()
    }
    writer.valid.poke(false.B)
    clock.step()
  }

  def pokeCmd(data: Seq[Int], cmd: IDChannel, clock: Clock)(
    implicit p: Parameters
  ): Unit = {
    data foreach { d =>
      cmd.valid.poke(true.B)
      cmd.bits.poke(d.U)
      clock.step()
      cmd.valid.poke(false.B)
      while (!cmd.ready.peek.litToBoolean) clock.step()
    }
  }

  def checkMem(data: Seq[Int],
               resultMem: MemBundle,
               clock: Clock,
               offset: Int = 0)(implicit p: Parameters): Unit = {
    data.zipWithIndex foreach {
      case (v, idx) =>
        val addr = idx + offset
        resultMem.a.poke(addr.U)
        clock.step()
        resultMem.d.expect(v.U, f"at address $addr%#x")
    }
  }

  // FIXME: need checkReg

  def checkResult(data: Seq[Int], result: Valid[UInt], clock: Clock)(
    implicit p: Parameters
  ): Unit = {
    data foreach { v =>
      while (!result.valid.peek.litToBoolean) clock.step()

      result.valid.expect(true.B)
      result.bits.expect(v.U)
      clock.step()
    }
  }

  // only used for standalone tests (i.e. PEs without done signal)
  // PEs with done signal should have the done signal checked
  def checkExit(valid: Bool, clock: Clock): Unit = {
    for { _ <- 0 until 8 } {
      valid.expect(false.B)
      clock.step()
    }
  }

  // used to check output bank for a single PE
  def waitAndCheckMem(startLatency: Int, dut: PEBase, data: Seq[Int], endLatency : Int = 2)(
    implicit p: Parameters
  ): Unit = {
    dut.clock.step(startLatency + 1)
    while (!dut.done.peek.litToBoolean) dut.clock.step()
    dut.clock.step(endLatency)
    dut.enable.poke(false.B)
    checkMem(data, dut.outMemExtRd, dut.clock)
  }

  trait SingleRegReaderDUT {
    this: PEBase with MultiIOModule =>
    private val reg = Module(new SharedRegister(1))

    override lazy val inRegRd = Wire(new RegFileBundle)
    val inRegWr = IO(Flipped(Valid(new RegFileWriteBundle)))

    reg.readers.head <> inRegRd
    reg.writer <> inRegWr
  }

  trait NoOutMem {
    this: PEBase =>
    outMemWr := DontCare
    outMemRd := DontCare
  }

  implicit class CmdBundleDriver(cmd: CmdBundle) {
    // FIXME: should only be used for synchronous testing for CmdBundle.
    def enqueue(clock: Clock, v: (Int, Int)): Unit = {
      cmd.memRAddr.valid.poke(true.B)
      cmd.memRAddr.bits.poke(v._1.U)

      cmd.regRAddr.valid.poke(true.B)
      cmd.regRAddr.bits.poke(v._2.U)

      clock.step()

      cmd.memRAddr.valid.poke(false.B)
      cmd.regRAddr.valid.poke(false.B)
    }

    def enqueueSeq(clock: Clock, s: Seq[(Int, Int)]): Unit = {
      if (s.nonEmpty) {
        enqueue(clock, s.head)
        enqueueSeq(clock, s.tail)
      }
    }
  }
}
