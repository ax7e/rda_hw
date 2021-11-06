package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import utest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.util.Random
import hermes._

import scala.language.postfixOps

abstract class MatrixSparseProductPEBase(implicit val p: Parameters)
  extends PEBase {
  val dimSize = 1 << p.memAWidth

  override def outASize = p.regAWidth

  val map_3i = Module(new MapSparseInMem(dimSize, numInputs = 3, needPositionDims = Set(1),
    passthroughDims = Set(0), inputChannel = 0))
  done := map_3i.done && !outMemWr.valid
  map_3i.connect(broadcastID(cmd, 3, Set(1, 2)), Seq(true.B, true.B, true.B))

  val (aluChans, reduceChan) = map_3i.childIDs splitAt 2

  val alu = Module(new Arithmetic(_ * _))
  alu.connectParent(aluChans)

  val reduce = Module(new Reduce(Right(identity))(p.copy(memAWidth = outASize)))
  reduce.connectParent(reduceChan)

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  reduce.inData <> alu.outData
  reduce.inMem <> outMemRd
  reduce.outMem <> outMemWr

  private val posMem = Module(new BankedMemory)

  val posMemWr = IO(Flipped(Valid(new MemWriteBundle)))
  val posMemRd = Wire(new MemBundle)
  posMem.writer <> posMemWr
  posMem.reader <> posMemRd
  posMemRd <> map_3i.position

  private val idxMem = Module(new BankedMemory)

  val idxMemWr = IO(Flipped(Valid(new MemWriteBundle)))
  val idxMemRd = Wire(new MemBundle)
  idxMem.writer <> idxMemWr
  idxMem.reader <> idxMemRd
  idxMemRd <> map_3i.index

}

class MatrixSparseProductPEDUT(implicit override val p: Parameters)
  extends MatrixSparseProductPEBase
    with SingleRegReaderDUT

class MatrixSparseProductPEShared(implicit override val p: Parameters)
  extends MatrixSparseProductPEBase
    with SharedPEImpl

object SparseProduct extends ChiselUtestTester {
  val maxInt = 128
  implicit val p = Parameters(memAWidth = 5, regAWidth = 8)
  val tests = Tests {
    test("sparse_matrix_product") {
      val nonzero = 12
      val dimSize = 1 << p.memAWidth
      implicit class MatrixPrinter(a: Seq[Int]) {
        def printMatrix(dimSize: Int) = {
          println("=====")
          a.grouped(dimSize).foreach(
            o => {
              o.foreach(x => print(x.toHexString + " "))
              println()
            }
          )
          a
        }
      }

      val leftValues = Seq.fill(1) {
        Random.nextInt(maxInt)
      } printMatrix 1
      val rightValues = Seq.fill(nonzero) {
        Random.nextInt(maxInt)
      } printMatrix nonzero
      val rightIndex = Random.shuffle((0 until dimSize).toList).splitAt(nonzero)._1.sorted printMatrix nonzero
      val rightPosition = Seq(0, nonzero) printMatrix 2
      val outValues = for {
        y <- 0 until dimSize
      } yield {
        if (rightIndex contains y) {
          leftValues(0) * rightValues(rightIndex.zipWithIndex.filter(_._1 == y)(0)._2)
        }
        else 0
      }
      outValues.printMatrix(dimSize)
      //      val rowIdx = Random.nextInt(dimSize)
      //      val outValues = for {
      //        x <- 0 until dimSize
      //        y <- 0 until dimSize
      //      } yield leftValues(x) * rightValues(rowIdx * dimSize + y)
      testCircuit(
        new MatrixSparseProductPEDUT,
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        dut.enable.poke(true.B)
        fork {
          fillReg(rightValues, dut.inRegWr, dut.clock)
        } fork {
          fillMem(leftValues, dut.inMemWr, dut.clock)
        } fork {
          fillMem(rightIndex, dut.idxMemWr, dut.clock)
        } fork {
          fillMem(rightPosition, dut.posMemWr, dut.clock)
        } fork {
          dut.clock.step(rightValues.length)
          while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
          pokeCmd(Seq(0), dut.cmd, dut.clock)
        } fork {
          waitAndCheckMem(rightValues.length, dut, outValues)
        } join()
      }
    }
  }
}
