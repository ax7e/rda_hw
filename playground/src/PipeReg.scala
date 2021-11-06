package hermes
import chisel3._
class RegWriteBundle(implicit p: Parameters) extends Bundle {
  val en = Input(Bool())
  val a  = Input(PipeRegAddr)
  val d  = Input(PipeRegData)
}

class RegInBundle(implicit p: Parameters) extends Bundle {
  val en = Input(UInt(p.PRCount.W))
  val d  = Input(Vec(p.PRCount, UInt(p.regDataWidth.W)))
}

class RegControlBundle(implicit p : Parameters) extends Bundle {
  val in_en = Input(UInt(p.PRCount.W))
  val w_en = Input(Bool())
  val a  = Input(PipeRegAddr)
}

/**
  * Pipelined registers
  */
class PipeReg(implicit val p: Parameters) extends MultiIOModule {
  val io = IO(new Bundle {
    val in  = new RegInBundle()
    val out = Output(Vec(p.PRCount, UInt(p.regDataWidth.W)))
    val w   = new RegWriteBundle()
  })
  def connect(b : RegControlBundle): Unit = {
    io.in.en := b.in_en
    io.w.en := b.w_en
    io.w.a := b.a
  }
  val reg = Mem(p.PRCount, UInt(p.regDataWidth.W))
  for (i <- 0 until p.PRCount) {
    reg(i)    := Mux(io.w.en & (io.w.a === i.U), io.w.d, Mux(io.in.en(i), io.in.d(i), reg(i)))
    io.out(i) := reg(i)
  }
}
