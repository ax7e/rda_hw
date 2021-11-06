package hermes

import chisel3._
import chisel3.experimental._
import chisel3.util._

/** Arithmetic unit for a single PE. */
class Arithmetic(compute: (UInt, UInt) => UInt)(implicit val p: Parameters)
    extends MultiIOModule
    with HasParent
    with InMem
    with InReg {

  // FIXME: this is not the case if the calculation is not combinational
  val idle :: readMem :: readMemPipeline :: lastCalc :: Nil = Enum(4)

  val cmd     = IO(new CmdBundle)
  val outData = IO(Valid(MemData))

  def connectParent(parent: Seq[IDChannel]): Unit = {
    if (parent.length != 2)
      throw new RuntimeException(
        "Arithmetic expects exactly 2 parent channels for mem and reg"
      )
    parent.zip(cmd).foreach { case (p, c) => p <> c }
  }

  private val state        = RegInit(idle)
  private val l            = Reg(RegData)
  private val r            = Reg(MemData)
  private val regAddrDelay = Reg(hermes.RegAddr)

  regAddrDelay := cmd.regRAddr.bits
  inReg.a      := regAddrDelay
  inMem.a      := cmd.memRAddr.bits

  // FIXME: this is not the case if the calculation is not combinational
  private val thisReady = true.B

  cmd.zip(cmd.reverse).foreach { case (a, b) => a.ready := b.valid && thisReady }

  private val addrsValid = cmd.memRAddr.valid && cmd.regRAddr.valid

  when(state === readMem || state === readMemPipeline) {
    r := inMem.d
    l := inReg.d
  }

  outData.bits  := compute(l, r)
  outData.valid := (state === readMemPipeline) || (state === lastCalc)

  switch(state) {
    is(idle) {
      when(addrsValid) { state := readMem }
    }
    is(readMem) {
      when(addrsValid) {
        state := readMemPipeline
      }.otherwise {
        state := lastCalc
      }
    }
    is(readMemPipeline) {
      when(!addrsValid) {
        state := lastCalc
      }
    }
    is(lastCalc) {
      when(addrsValid) {
        state := readMem
      }.otherwise {
        state := idle
      }
    }
  }
}
