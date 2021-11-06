package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import utest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.util.Random
import hermes._

import scala.language.postfixOps

abstract class MatrixSparseOuterProductPEBase(implicit val p: Parameters)
  extends PEBase {
  val dimSize = 1 << p.memAWidth

  override def outASize = p.regAWidth

  val map_3o = Module(new MapSparseInMem(dimSize, numInputs = 3, needPositionDims = Set(0),
    passthroughDims = Set(1), inputChannel = 0))
  done := map_3o.done && !outMemWr.valid

  val map_3i = Module(new MapSparseInReg(dimSize, numInputs = 3, needPositionDims = Set(1),
    passthroughDims = Set(0), inputChannel = 1))
  map_3o.connect(broadcastID(cmd, 3, Set(0, 2)), Seq.fill(3)(map_3i.done))

  map_3i.connectParent(map_3o.childIDs)
  map_3i.connectChildDone(Seq(true.B, true.B, true.B))


  //  val map_3i = Module(new MapSparseInMem(dimSize, numInputs = 3, needpositionDims = Set(1),
  //    passthroughDims = Set(0), inputChannel = 0))
  //  done := map_3i.done && !outMemWr.valid
  //  map_3i.connectParent(broadcastID(cmd, 3, Set(1, 2)))

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
  posMemRd <> map_3o.position

  private val idxMem = Module(new BankedMemory)

  val idxMemWr = IO(Flipped(Valid(new MemWriteBundle)))
  val idxMemRd = Wire(new MemBundle)
  idxMem.writer <> idxMemWr
  idxMem.reader <> idxMemRd
  idxMemRd <> map_3o.index
  //  private val posMem = Module(new SharedRegister(1))
  //
  //  val posMemWr = IO(Flipped(Valid(new RegFileWriteBundle)))
  //  val posMemRd = Wire(new RegFileBundle)
  //  posMem.writer <> posMemWr
  //  posMem.readers.head <> posMemRd
  //  posMemRd <> map_3o.position
  //
  //  private val idxMem = Module(new SharedRegister(1))
  //
  //  val idxMemWr = IO(Flipped(Valid(new RegFileWriteBundle)))
  //  val idxMemRd = Wire(new RegFileBundle)
  //  idxMem.writer <> idxMemWr
  //  idxMem.readers.head <> idxMemRd
  //  idxMemRd <> map_3o.index

  private val posReg = Module(new SharedRegister(1))

  val posRegRd = Wire(new RegFileBundle)
  val posRegWr = IO(Flipped(Valid(new RegFileWriteBundle)))

  posReg.readers.head <> posRegRd
  posReg.writer <> posRegWr
  posRegRd <> map_3i.position

  private val idxReg = Module(new SharedRegister(1))

  val idxRegRd = Wire(new RegFileBundle)
  val idxRegWr = IO(Flipped(Valid(new RegFileWriteBundle)))

  idxReg.readers.head <> idxRegRd
  idxReg.writer <> idxRegWr
  idxRegRd <> map_3i.index

}

class MatrixSparseOuterProductPEDUT(implicit override val p: Parameters)
  extends MatrixSparseOuterProductPEBase
    with SingleRegReaderDUT

class MatrixSparseOuterProductPEShared(implicit override val p: Parameters)
  extends MatrixSparseOuterProductPEBase
    with SharedPEImpl

object SparseOuterProduct extends ChiselUtestTester {
  val maxInt = 128
  implicit val p = Parameters(memAWidth = 5, regAWidth = 10)
  val tests = Tests {
    test("sparse_matrix_outer_product") {
      val nonzero = 23
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

      val leftValues = Seq.fill(12) {
        Random.nextInt(maxInt)
      } printMatrix 12
      val leftIndex = Random.shuffle((0 until dimSize).toList).splitAt(12)._1.sorted printMatrix nonzero
      val leftPosition = Seq(0, 12) printMatrix 2
      val rightValues = Seq.fill(nonzero) {
        Random.nextInt(maxInt)
      } printMatrix nonzero
      val rightIndex = Random.shuffle((0 until dimSize).toList).splitAt(nonzero)._1.sorted printMatrix nonzero
      val rightPosition = Seq(0, nonzero) printMatrix 2
      val outValues = for {
        x <- 0 until dimSize
        y <- 0 until dimSize
      } yield {
        if ((leftIndex contains x) && (rightIndex contains y)) {
          leftValues(leftIndex.zipWithIndex.filter(_._1 == x)(0)._2) * rightValues(rightIndex.zipWithIndex.filter(_._1 == y)(0)._2)
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
        new MatrixSparseOuterProductPEDUT,
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        dut.enable.poke(true.B)
        fork {
          fillReg(rightValues, dut.inRegWr, dut.clock)
        } fork {
          fillMem(leftValues, dut.inMemWr, dut.clock)
        } fork {
          fillMem(leftIndex, dut.idxMemWr, dut.clock)
        } fork {
          fillMem(leftPosition, dut.posMemWr, dut.clock)
        } fork {
          fillReg(rightIndex, dut.idxRegWr, dut.clock)
        } fork {
          fillReg(rightPosition, dut.posRegWr, dut.clock)
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
