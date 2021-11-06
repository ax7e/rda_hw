package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import utest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.util.Random
import hermes._

import scala.language.postfixOps

abstract class MatrixOuterProductPEBase(implicit val p: Parameters)
    extends PEBase {
  val dimSize = 1 << p.memAWidth
  override def outASize = p.regAWidth

  // 3 channel(s):
  // - map_3i (ALU Mem: left matrix column points) - masked
  // - map_3i (ID gen for ALU Reg) - passthrough
  // - map_3i (ID gen for Reduce) - masked
  val map_3o = Module(new Map(dimSize, numInputs = 3, Set(1)))
  // Once started, the PE will keep writing the memory,
  // so when the address supported to outMemWr is none, the writing process is over
  done := map_3o.done && !outMemWr.valid
  map_3o.connectParent(broadcastID(cmd, 3, Set(0, 2)))
  map_3o.connectChildDone(Seq.fill(3){true.B})

  // 3 channel(s):
  // - ALU Mem (left matrix column points) - passthrough
  // - ALU Reg (right matrix row points)
  // - Reduce (output buffer points)
  val map_3i = Module(new Map(dimSize, numInputs = 3, Set(0)))
  map_3i.connectParent(map_3o.childIDs)
  map_3i.connectChildDone(Seq.fill(3){true.B})

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
}
class MatrixOuterProductPEDUT(implicit override val p: Parameters)
    extends MatrixOuterProductPEBase
    with SingleRegReaderDUT
class MatrixOuterProductPEShared(implicit override val p: Parameters)
    extends MatrixOuterProductPEBase
    with SharedPEImpl

object OuterProduct extends ChiselUtestTester {
  val maxInt = 128
  implicit val p = Parameters(memAWidth = 4, regAWidth = 8)
  val tests = Tests {
    test("matrix_outer_product") {
      val dimSize = 1 << p.memAWidth
      implicit class MatrixPrinter(a: Seq[Int]) {
        def printMatrix(dimSize: Int) = {
          println("=====")
          a.grouped(dimSize).foreach { r =>
            println(r.mkString(" "))
          }
          a
        }
      }

      val leftValues = Seq.fill(dimSize) {
        Random.nextInt(maxInt)
      } printMatrix dimSize
      val rightValues = Seq.fill(1 << p.regAWidth) {
        Random.nextInt(maxInt)
      } printMatrix dimSize
      val rowIdx = Random.nextInt(dimSize)
      val outValues = for {
        x <- 0 until dimSize
        y <- 0 until dimSize
      } yield leftValues(x) * rightValues(rowIdx * dimSize + y)
      testCircuit(
        new MatrixOuterProductPEDUT,
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        dut.enable.poke(true.B)
        fork {
          fillReg(rightValues, dut.inRegWr, dut.clock)
        } fork {
          fillMem(leftValues, dut.inMemWr, dut.clock)
        } fork {
          dut.clock.step(rightValues.length)
          while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
          pokeCmd(Seq(rowIdx), dut.cmd, dut.clock)
        } fork {
          waitAndCheckMem(rightValues.length, dut, outValues)
        } join ()
      }
    }
  }
}
