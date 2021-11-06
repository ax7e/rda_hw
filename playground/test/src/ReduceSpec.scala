package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import utest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.util.Random
import hermes._

class VectorReducePE(isElementWise: Boolean = true)(implicit val p: Parameters)
    extends PEBase
    with SingleRegReaderDUT {
  val dimSize = 1 << p.memAWidth

  // 3 IDs: 2 ALU, 1 Reduce (passthrough)
  val passthroughs: Set[Int] =
    if (isElementWise) { Set() } else { Set(2) }
  override def outASize = if (isElementWise) { super.outASize } else { 1 }

  val map_3 = Module(new Map(dimSize, numInputs = 3, passthroughs))
  done := map_3.done && !outMemWr.valid

  val alu = Module(new Arithmetic(_ * _))
  val reduce = Module(new Reduce(Left(_ + _)))

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  reduce.inData <> alu.outData
  reduce.inMem <> outMemRd
  reduce.outMem <> outMemWr

  map_3.connectParent(broadcastID(cmd, 3))
  map_3.connectChildDone(Seq.fill(3){true.B})

  val (aluChans, reduceChan) = map_3.childIDs splitAt 2
  alu.connectParent(aluChans)
  reduce.connectParent(reduceChan)
}

object ReduceSpec extends ChiselUtestTester {
  val tests = Tests {
    test("vector_elemwise_multiply") {
      implicit val p = Parameters(memAWidth = 5, regAWidth = 5)
      val nElems = 1 << p.memAWidth
      val values = Seq.fill(nElems) {
        (Random.nextInt(maxInt), Random.nextInt(maxInt), Random.nextInt(maxInt))
      }

      testCircuit(
        new VectorReducePE,
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        dut.enable.poke(true.B)
        fork {
          fillReg(values.map(_._1), dut.inRegWr, dut.clock)
        } fork {
          fillMem(values.map(_._2), dut.inMemWr, dut.clock)
        } fork {
          dut.enable.poke(false.B)
          fillMem(values.map(_._3), dut.outMemExtWr, dut.clock)
        } fork {
          dut.clock.step(values.length)
          while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
          pokeCmd(Seq(0), dut.cmd, dut.clock)
        } fork {
          waitAndCheckMem(values.length, dut, values.map {
            case (v1, v2, v3) => v1 * v2 + v3
          })
        } join ()
      }
    }
    test("vector_dot") {
      val maxInt = 128 // use small value to avoid overflow in test framework
      implicit val p = Parameters(memAWidth = 5, regAWidth = 5)
      val nElems = 1 << p.memAWidth
      val values = Seq.fill(nElems) {
        (Random.nextInt(maxInt), Random.nextInt(maxInt))
      }
      testCircuit(
        new VectorReducePE(false),
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        dut.enable.poke(true.B)
        fork {
          fillReg(values.map(_._1), dut.inRegWr, dut.clock)
        } fork {
          fillMem(values.map(_._2), dut.inMemWr, dut.clock)
        } fork {
          dut.clock.step(values.length)
          while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
          pokeCmd(Seq(0), dut.cmd, dut.clock)
        } fork {
          waitAndCheckMem(values.length, dut, Seq(values.foldRight(0) {
            case ((l, r), b) => b + l * r
          }))
        } join ()
      }
    }
  }
}
