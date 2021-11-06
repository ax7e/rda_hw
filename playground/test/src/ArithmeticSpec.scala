package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import utest._

import chiseltest.internal.WriteVcdAnnotation

import scala.util.Random

import hermes._

class SimpleAdderPE(implicit val p: Parameters)
    extends PEBase
    with SingleRegReaderDUT
    with NoOutMem {
  val adder = Module(new Arithmetic(_ + _))

  adder.inReg <> inRegRd
  adder.inMem <> inMemRd

  val result = IO(Valid(MemData))
  // conflict with PEBase.cmd
  val myCmd = IO(new CmdBundle)
  cmd <> DontCare

  result <> adder.outData
  adder.cmd <> myCmd

  done := DontCare
}

object ArithmeticSpec extends ChiselUtestTester {
  val tests = Tests {
    implicit val p = Parameters(memAWidth = 4, regAWidth = 4)
    val nElems = 16
    val waitWriterCycles = 4
    val values = Seq.fill(nElems) {
      (Random.nextInt(maxInt), Random.nextInt(maxInt))
    }

    val cmds = for { n <- 0 until nElems } yield (n, n)

    test("random_calculation") {
      testCircuit(new SimpleAdderPE, Seq(WriteVcdAnnotation)) { dut =>
        fork {
          fillMem(values.map(_._1), dut.inMemWr, dut.clock)
        } fork {
          fillReg(values.map(_._2), dut.inRegWr, dut.clock)
        } fork {
          // generate command
          dut.clock.step(nElems + waitWriterCycles)
          val (c1q, c2q) = cmds.splitAt(cmds.length / 3)
          val (c21q, c22q) = c2q.splitAt(cmds.length / 3)
          dut.myCmd.enqueueSeq(dut.clock, c1q)
          dut.clock.step() // stall for 1 cycle
          dut.myCmd.enqueueSeq(dut.clock, c21q)
          dut.clock.step(2) // stall for 2 cycles
          dut.myCmd.enqueueSeq(dut.clock, c22q)
        } fork {
          // collect result
          checkResult(
            values.map { case (a, b) => a + b },
            dut.result,
            dut.clock
          )
          checkExit(dut.result.valid, dut.clock)
        } join ()
      }
    }
    test("stall_one_channel") {
      val (l, r) = cmds.unzip
      val (l1, l2) = l splitAt cmds.length / 3
      val (r1, r2) = r splitAt 2 * cmds.length / 3

      testCircuit(new SimpleAdderPE, Seq(WriteVcdAnnotation)) { dut =>
        dut.myCmd.memRAddr.initSource()
        dut.myCmd.memRAddr.setSourceClock(dut.clock)
        dut.myCmd.regRAddr.initSource()
        dut.myCmd.regRAddr.setSourceClock(dut.clock)

        fork {
          fillMem(values.map(_._1), dut.inMemWr, dut.clock)
        } fork {
          fillReg(values.map(_._2), dut.inRegWr, dut.clock)
        } fork {
          dut.clock.step(nElems + waitWriterCycles)
          dut.clock.step()
          dut.myCmd.regRAddr.enqueueSeq(l1.map(_.U))
          dut.clock.step(5)
          dut.myCmd.regRAddr.enqueueSeq(l2.map(_.U))
        } fork {
          dut.clock.step(nElems + waitWriterCycles)
          dut.myCmd.memRAddr.enqueueSeq(r1.map(_.U))
          dut.clock.step(10)
          dut.myCmd.memRAddr.enqueueSeq(r2.map(_.U))
        } fork {
          // collect result
          checkResult(
            values.map { case (a, b) => a + b },
            dut.result,
            dut.clock
          )
          checkExit(dut.result.valid, dut.clock)
        } join ()
      }
    }
  }
}
