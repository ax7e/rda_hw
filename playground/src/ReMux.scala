package hermes
import chisel3._
import chisel3.util.{MixedVec, _}

class ReMuxControlBundle(channelDims : Seq[Int], outChannelDim: Int) (implicit p:Parameters) extends Bundle {
  // c1 represents the channel of the output result
  // c2 represents the index of the output result
  // Additional inputs is used to select 0.U
  val maxChanIdxWidth  = log2Ceil(channelDims.length+1)
  val maxChanAddrWidth = log2Ceil(channelDims.max)
  val c1 = Input(Vec(outChannelDim,UInt(maxChanIdxWidth.W)))
  val c2 = Input(Vec(outChannelDim,UInt(maxChanAddrWidth.W)))
}

class ReMux(channelDims: Seq[Int], outChannelDim: Int)(implicit p: Parameters) extends MultiIOModule {
  val numInputs = channelDims.length
  val maxChanIdxWidth  = log2Ceil(channelDims.length+1)
  val maxChanAddrWidth = log2Ceil(channelDims.max)
  val controlIOs = IO(new ReMuxControlBundle(channelDims, outChannelDim))
  val channelInputs = IO(Input(MixedVec({
    0 until numInputs
  }.map { i => Vec(channelDims(i), UInt(p.regDataWidth.W)) })))
  val channelOutputs = for { _ <- 0 until outChannelDim } yield {
    val out = IO(Output(UInt(p.regDataWidth.W)))
    out
  }
  def connect(muxIn : ReMuxControlBundle): Unit = {
    controlIOs <> muxIn
  }

  for { idx <- 0 until outChannelDim } {
    channelOutputs(idx) := MuxLookup(
      controlIOs.c1(idx),
      0.U, {
        0 until numInputs
      }.map { case x => (x.U, channelInputs(x)(controlIOs.c2(idx))) }
    )
  }
}
