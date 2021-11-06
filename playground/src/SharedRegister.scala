package hermes

import chisel3._
import chisel3.util._

/** Interface for shared register file. */
class RegFileBundle(implicit p: Parameters) extends Bundle {
  val a = Input(RegAddr)
  val d = Output(RegData)
}

class RegFileWriteBundle(implicit p: Parameters) extends Bundle {
  val a = RegAddr
  val d = RegData
}

class SharedRegister(numReaders: Int)(implicit p: Parameters) extends MultiIOModule {
  val readers = Seq.fill(numReaders)(IO(new RegFileBundle))
  val writer  = IO(Flipped(Valid(new RegFileWriteBundle)))

  private val data =
    Mem(1 << p.regAWidth, RegData)

  when(writer.valid) {
    data(writer.bits.a) := writer.bits.d
  }

  readers.foreach { r =>
    r.d := data(r.a)
  }
}
