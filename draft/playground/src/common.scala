import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import scala.language.implicitConversions

package object hermes {
  def PipeRegAddr(implicit p: Parameters) = UInt(p.pipeRegAddrWidth.W)
  def PipeRegData(implicit p: Parameters) = UInt(p.regDataWidth.W)

  def RegAddr(implicit p: Parameters) = UInt(p.regAWidth.W)
  def RegData(implicit p: Parameters) = UInt(p.regDWidth.W)
  def MemAddr(implicit p: Parameters) = UInt(p.memAWidth.W)
  def MemData(implicit p: Parameters) = UInt(p.memDWidth.W)
  def ID(implicit p: Parameters) = UInt(p.idWidth.W)

  /** broadcast upstream ID, selectively mask for some channels */
  def broadcastID(cmd: IDChannel, numChildren: Int, masks: Set[Int] = Set.empty)(implicit p: Parameters) = {
    generateID(
      cmd,
      numChildren,
      {
        case (pc, idx) =>
          if (masks contains idx) 0.U else pc
      }
    )
  }

  /** generate ID to downstream with given function. */
  def generateID(
    cmd:         IDChannel,
    numChildren: Int,
    func:        (UInt, Int) => UInt
  )(
    implicit p: Parameters
  ) = {
    val children = Seq.fill(numChildren) { Wire(Decoupled(ID)) }
    children.zipWithIndex.foreach {
      case (sid, idx) =>
        sid.valid := cmd.valid
        sid.bits  := func(cmd.bits, idx)
    }
    cmd.ready := children.map(_.ready).foldLeft(true.B)(_ && _)
    children
  }

  type IDChannel = DecoupledIO[UInt]

  /** Marks the command for a single arithmetic unit in a PE. */
  implicit def cmdToSeq(cmd: CmdBundle) = {
    Seq(cmd.memRAddr, cmd.regRAddr)
  }
}

package hermes {
  case class Parameters(
    memAWidth:  Int = 16,
    memDWidth:  Int = 32,
    regAWidth:  Int = 10,
    regDWidth:  Int = 32,
    idWidth:    Int = 16,
    posAWidth:  Int = 16,
    posDWidth:  Int = 32,
    idxAWidth:  Int = 16,
    idxDWidth:  Int = 32,
    pos1AWidth: Int = 16,
    pos1DWidth: Int = 32,
    idx1AWidth: Int = 16,
    idx1DWidth: Int = 32,


    //Created for rda
    memAddrWidth: Int = 16,
    memDataWidth: Int = 32,
    regDataWidth: Int = 32,
    PRCount:      Int = 4,
    PRIdxWidth:   Int = 2,
    lane:         Int = 4,
    pipeRegCount: Int = 6,
    // util.log2Ceil(pipeRegCount)
    pipeRegAddrWidth: Int = util.log2Ceil(6),
    funcUnitOpWidth : Int = 4,
    stage:            Int = 4)



  class CmdBundle(implicit p: Parameters) extends Bundle {
    val memRAddr = Flipped(Decoupled(MemAddr))
    val regRAddr = Flipped(Decoupled(RegAddr))
  }

  trait InMem {
    this: MultiIOModule =>
    implicit val p: Parameters

    val inMem = IO(Flipped(new MemBundle))
  }

  trait InReg {
    this: MultiIOModule =>
    implicit val p: Parameters

    val inReg = IO(Flipped(new RegFileBundle))
  }

  trait MetadataInReg {
    this: MultiIOModule =>
    implicit val p: Parameters

    val position = IO(
      Flipped(
        new RegFileBundle()(
          p.copy(regAWidth = p.posAWidth, regDWidth = p.posDWidth)
        )
      )
    )
    val index = IO(
      Flipped(
        new RegFileBundle()(
          p.copy(regAWidth = p.idxAWidth, regDWidth = p.idxDWidth)
        )
      )
    )
  }

  trait MetadataInMem {
    this: MultiIOModule =>
    implicit val p: Parameters

    val position = IO(
      Flipped(
        new MemBundle()(
          p.copy(regAWidth = p.posAWidth, regDWidth = p.posDWidth)
        )
      )
    )
    val index = IO(
      Flipped(
        new MemBundle()(
          p.copy(regAWidth = p.idxAWidth, regDWidth = p.idxDWidth)
        )
      )
    )
  }

  trait HasParent {
    def connectParent(parent: Seq[IDChannel])
  }

  trait PEBaseImpl {
    this: MultiIOModule =>
    implicit val p: Parameters

    val cmd  = IO(Flipped(Decoupled(ID)))
    val done = IO(Output(Bool()))

    // when enable = true, outMem connects to internal RW ports.
    // when enable = false, outMem connects to external RW ports (e.g. LSU).
    val enable      = IO(Input(Bool()))
    private val mem = Module(new BankedMemory)

    val inRegRd: RegFileBundle

    val inMemWr = IO(Flipped(Valid(new MemWriteBundle)))
    val inMemRd = Wire(new MemBundle)
    mem.writer <> inMemWr
    inMemRd <> mem.reader

    def outASize = p.memAWidth

    val outP        = p.copy(memAWidth = outASize)
    private val out = Module(new BankedMemory()(outP))

    val outMemWr = Wire(Valid(new MemWriteBundle()(outP)))
    val outMemRd = Wire(new MemBundle()(outP))

    val outMemExtWr = IO(Flipped(Valid(new MemWriteBundle()(outP))))
    val outMemExtRd = IO(new MemBundle()(outP))
    outMemExtRd.d := out.reader.d
    outMemRd.d    := out.reader.d
    out.reader.a  := Mux(enable, outMemRd.a, outMemExtRd.a)
    out.writer <> Mux(enable, outMemWr, outMemExtWr)
  }

  abstract class PEBase extends MultiIOModule with PEBaseImpl

  trait SharedPEImpl {
    this: PEBase =>

    override lazy val inRegRd = IO(Flipped(new RegFileBundle))
  }

}
