package hermes.testing
import chisel3._
import chisel3.util._
import chisel3.tester._
import utest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.util.Random
import hermes._
import testing._

import scala.language.postfixOps

abstract class MatrixSparseInnerProductPEBase(implicit val p: Parameters) extends PEBase {
  val dimSize = 1 << p.memAWidth
  println(f"dimSize in PE = $dimSize")

  override def outASize: Int = p.regAWidth
  val map_3o = Module(new Map(dimSize, 3, passthroughDims = Set(0)))
  done := map_3o.done && ShiftRegister(map_3o.done, aluLatency + 1) && !outMemWr.valid
  val map_3i = Module(
    new AndZip(
      dimSize,
      numInputs        = 3,
      needPositionDims = Set(0, 1),
      passthroughDims  = Set(2),
      inputChannelL    = 0,
      inputChannelR    = 1
    )
  )
  map_3o.connectParent(broadcastID(cmd, 3, Set(0, 1, 2)))
  map_3o.connectChildDone(Seq.fill(3){map_3i.done})
  val (aluChans, reduceChan) = map_3i.childIDs.splitAt(2)
  val alu                    = Module(new Arithmetic(_ * _))
  alu.connectParent(aluChans)

  map_3i.connectParent(map_3o.childIDs)
  map_3i.connectChildDone(Seq(true.B, true.B, true.B))
  val reduce = Module(new Reduce(Left(_ + _)))
  reduce.connectParent(reduceChan)

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  reduce.inData <> alu.outData
  reduce.inMem <> outMemRd
  reduce.outMem <> outMemWr

  /** @todo for the convenience of test,  use register instead for now */
  private val posMem = Module(new SharedRegister(numReaders = 1))

  val posMemWr = IO(Flipped(Valid(new RegFileWriteBundle())))
  val posMemRd = Wire(new RegFileBundle())
  posMem.writer <> posMemWr
  posMem.readers.head <> posMemRd
  posMemRd <> map_3i.positionL

  private val idxMem = Module(new SharedRegister(numReaders = 1))

  val idxMemWr = IO(Flipped(Valid(new RegFileWriteBundle())))
  val idxMemRd = Wire(new RegFileBundle())
  idxMem.writer <> idxMemWr
  idxMem.readers.head <> idxMemRd
  idxMemRd <> map_3i.indexL

  private val posReg = Module(new SharedRegister(numReaders = 1))
  val posRegWr       = IO(Flipped(Valid(new RegFileWriteBundle)))
  val posRegRd       = Wire(new RegFileBundle)
  posReg.writer <> posRegWr
  posReg.readers.head <> posRegRd
  posRegRd <> map_3i.positionR

  private val idxReg = Module(new SharedRegister(numReaders = 1))
  val idxRegWr       = IO(Flipped(Valid(new RegFileWriteBundle)))
  val idxRegRd       = Wire(new RegFileBundle)
  idxReg.writer <> idxRegWr
  idxReg.readers.head <> idxRegRd
  idxRegRd <> map_3i.indexR
}

class MatrixSparseInnerProductPEDUT(implicit override val p: Parameters)
    extends MatrixSparseInnerProductPEBase
    with SingleRegReaderDUT

object SparseInnerProduct extends ChiselUtestTester {
  val r = new Random(42)
  val maxInt     = 128
  implicit val p = Parameters(memAWidth = 5, regAWidth = 10)
  val tests = Tests {
    test("sparse_matrix_inner_product") {
      val nonZero  = 13
      val rightNnz = 200
      val dimSize  = 1 << p.memAWidth
      println(f"dimSize = $dimSize\n")
      implicit class MatrixPrinter(a: Seq[Int]) {
        def printMatrix(name: String, dimSize: Int) = {
          println("=====" + name + "=====")
          a.grouped(dimSize)
            .foreach(o => {
              o.foreach(x => print(x.toHexString + " "))
              println()
            })
          a
        }
      }
      var cnt = 0
      val leftValues = Seq
        .fill(nonZero) {
          cnt = (cnt + 1)
          cnt
          //r.nextInt(maxInt)
        }
        .printMatrix("left", nonZero)
      val leftIndex =
        r.shuffle((0 until dimSize).toList).splitAt(nonZero)._1.sorted.printMatrix("left index", nonZero)
      val leftPosition = Seq(0, nonZero).printMatrix("left pos", 2)
      cnt = 0
      val rightValues = Seq
        .fill(rightNnz) {
          cnt = (cnt + 1)
          cnt
          //r.nextInt(maxInt)
        }
        .printMatrix("right nnz", rightNnz)
      val rightMatrix = r
        .shuffle((0 until (dimSize * dimSize)).toList)
        .splitAt(rightNnz)
        ._1
        .sorted
        .printMatrix("rightMat", rightNnz)
      val rightIndex = rightMatrix.map(_ % dimSize).printMatrix("right idx", rightNnz)
      val rightPosition =
        (0 to dimSize).map(x => rightMatrix.count(_ / dimSize < x)).printMatrix("right pos", dimSize + 1)
      val outValues = for {
        x <- 0 until dimSize
      } yield {
        ((
          for { y <- 0 until dimSize } yield {
            if ((leftIndex contains y) && (rightMatrix contains (x * dimSize + y))) {
              leftValues(leftIndex.zipWithIndex.filter(_._1 == y).head._2) *
                rightValues(rightMatrix.zipWithIndex.filter(_._1 == (x * dimSize + y)).head._2)
            } else 0
          }
        ) ).sum
      }
      outValues.printMatrix("outValues", dimSize)

      testCircuit(new MatrixSparseInnerProductPEDUT, Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        dut.enable.poke(true.B)
        fork {
          fillReg(rightValues, dut.inRegWr, dut.clock)
        }.fork {
          fillMem(leftValues, dut.inMemWr, dut.clock)
        }.fork {
          fillReg(leftIndex, dut.idxMemWr, dut.clock)
        }.fork {
          fillReg(leftPosition, dut.posMemWr, dut.clock)
        }.fork {
          fillReg(rightIndex, dut.idxRegWr, dut.clock)
        }.fork {
          fillReg(rightPosition, dut.posRegWr, dut.clock)
        }.fork {
          dut.clock.step(rightValues.length)
          while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
          pokeCmd(Seq(0), dut.cmd, dut.clock)
        }.fork {
          waitAndCheckMem(rightValues.length, dut, outValues, 0)
        }.join()
      }
    }
  }
}
