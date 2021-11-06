package hermes

import chisel3._
import chisel3.util._

object FuncUnit {
  val FU_ADD    = 0.U(4.W)
  val FU_SUB    = 1.U(4.W)
  val FU_MUL    = 2.U(4.W)
  /*
  val FU_AND    = 2.U(4.W)
  val FU_OR     = 3.U(4.W)
  val FU_XOR    = 4.U(4.W)
  val FU_SLT    = 5.U(4.W)
  val FU_SLL    = 6.U(4.W)
  val FU_SLTU   = 7.U(4.W)
  val FU_SRL    = 8.U(4.W)
  val FU_SRA    = 9.U(4.W)
  val FU_COPY_A = 10.U(4.W)
  val FU_COPY_B = 11.U(4.W)
  val FU_XXX    = 15.U(4.W)
   */
}

class FunctionalUnitIO(implicit p: Parameters) extends Bundle {
  val A      = Input(UInt(p.regDataWidth.W))
  val B      = Input(UInt(p.regDataWidth.W))
  val FU_op = Input(UInt(4.W))
  val out    = Output(UInt(p.regDataWidth.W))
}

import FuncUnit._

abstract class FuncUnit(implicit val p: Parameters) extends Module {
  val io = IO(new FunctionalUnitIO())
}

class FuncUnitSimple(implicit p: Parameters) extends FuncUnit()(p) {
  io.out := MuxLookup(
    io.FU_op,
    io.B,
    Seq(
      FU_ADD -> (io.A + io.B),
      FU_SUB -> (io.A - io.B),
      FU_MUL -> (io.A * io.B),
    )
  )
}