package hermes

import chisel3._
import chisel3.experimental._
import chisel3.util._

/**
  * Reduce unit for a single PE.  This unit also serves normal output data (when the reduce
  * function does not involve the original data in the output buffer.
  */
class Reduce(func: Either[(UInt, UInt) => UInt, UInt => UInt], aluLatency: Int = 2)(implicit val p: Parameters)
    extends MultiIOModule
    with HasParent
    with InMem {

  val idle :: readMem :: readWriteMem :: lastWrite :: Nil = Enum(4)
  val cmd                                                 = IO(Flipped(Decoupled(MemAddr)))
  val outMem                                              = IO(Valid(new MemWriteBundle))
  val inData                                              = IO(Flipped(Valid(MemData)))

  // Reduce always passively connect to a parent
  def connectParent(parent: Seq[IDChannel]): Unit = {
    if (parent.length != 1)
      throw new RuntimeException("expected exactly 1 parent for reduce")
    parent.head <> cmd
  }

  private val state = RegInit(idle)

  val isUpdate        = func.isLeft
  private val outData = Wire(MemData)
  private val inValid = Wire(Bool())

  outMem.bits.a := ShiftRegister(cmd.bits, aluLatency)

  outMem.bits.d := outData
  outMem.valid  := inData.valid

  cmd.ready := true.B

  inValid := inData.valid && cmd.valid

  if (isUpdate) {
    inMem.a := ShiftRegister(cmd.bits, aluLatency - 1)
    outData := func.left.get.apply(inData.bits, inMem.d)
  } else {
    inMem.a := DontCare
    outData := func.right.get.apply(inData.bits)
  }

  switch(state) {
    is(idle) {
      when(inValid) {
        if (isUpdate) {
          state := readMem
        } else {
          state := readWriteMem
        }
      }
    }
    is(readMem) {
      when(inValid) {
        state := readWriteMem
      }.otherwise {
        state := lastWrite
      }
    }
    is(readWriteMem) {
      when(!inValid) {
        state := lastWrite
      }
    }
    is(lastWrite) {
      when(inValid) {
        if (isUpdate) {
          state := readMem
        } else {
          state := readWriteMem
        }
      }.otherwise {
        state := idle
      }
    }
  }
}
