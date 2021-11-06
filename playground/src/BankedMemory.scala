package hermes

import chisel3._
import chisel3.util._

/** Memory interface for PE memory banks. */
class MemBundle(implicit p: Parameters) extends Bundle {
  val a = Input(MemAddr)
  val d = Output(MemData)
}

class MemWriteBundle(implicit p: Parameters) extends Bundle {
  val a = MemAddr
  val d = MemData
}

class BankedMemory(
  ruw: SyncReadMem.ReadUnderWrite = SyncReadMem.WriteFirst
)(
  implicit p: Parameters)
    extends MultiIOModule {
  val reader = IO(new MemBundle)
  val writer = IO(Flipped(Valid(new MemWriteBundle)))

  private val data =
    SyncReadMem(1 << p.memAWidth, MemData, ruw)

  when(writer.valid) {
    data.write(writer.bits.a, writer.bits.d)
  }

  // reader always enabled
  reader.d := data.read(reader.a, true.B)
}
