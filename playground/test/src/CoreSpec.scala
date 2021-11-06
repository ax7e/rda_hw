package hermes.testing
import chisel3.Num.toBigInt
import chisel3._
import chisel3.tester._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import hermes._
import utest._

import scala.util.Random

object CoreSpec extends ChiselUtestTester with TestUtils {
  val tests = Tests {
    val waitWriterCycles = 1
    test("test reduce add sum network") {
      implicit val p = Parameters(lane = 4, stage = 4)
      val maxInt     = 128
      val testA = { Seq.fill(p.lane) { Random.nextInt(maxInt) } }
      val testB = { Seq.fill(p.lane) { Random.nextInt(maxInt) } }
      for (i <- 0 until p.lane) {
        Predef.printf(s" ${testA(i)}")
      }
      println("\n");
      for (i <- 0 until p.lane) {
        Predef.printf(s" ${testB(i)}")
      }
      println("\n");
      val gold = (testA.zip(testB)).foldLeft(0) { (x, y) => x + y._1 * 2 + y._2 }
      testCircuit(new ComputeCore(), Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        for (i <- 0 until dut.controlIO.funcUnitCmd.length)
          dut.controlIO.funcUnitCmd(i).poke(FuncUnit.FU_ADD)
        for (i <- 0 until p.lane * p.stage) {
          val x = 0
          dut.controlIO.muxSetup.muxBefore(i).c1(0).poke(1.U)
          dut.controlIO.muxSetup.muxBefore(i).c2(0).poke(0.U)
          dut.controlIO.muxSetup.muxBefore(i).c1(1).poke(1.U)
          dut.controlIO.muxSetup.muxBefore(i).c2(1).poke(1.U)
        }
        val reduceNodes = Seq((0, 1), (0, 2), (2, 1))
        for ((x, y) <- reduceNodes) {
          val idx = x * p.stage + y;
          dut.controlIO.muxSetup.muxBefore(idx).c1(1).poke(2.U)
          dut.controlIO.muxSetup.muxBefore(idx).c2(1).poke(0.U)
        }

        /** From right register
         * ---------------------|-------------|
         *  Reduce input        |             |
         * ---------------------|   FU(0,3)   |
         *                      |             |
         * ---------------------|_____________|
         */
        dut.controlIO.muxSetup.muxBefore(p.stage-1).c1(1).poke(0.U)
        dut.controlIO.muxSetup.muxBefore(p.stage-1).c2(1).poke(0.U)

        for (i <- 0 until (p.lane - 1) * p.stage) {
          dut.controlIO.muxSetup.muxAfter(i).c1(0).poke(0.U)
          dut.controlIO.muxSetup.muxAfter(i).c2(0).poke(0.U)
        }
        for (i <- 0 until p.lane * (p.stage+1)) {
          dut.controlIO.regSetup(i).in_en.poke(0.U)
          dut.controlIO.regSetup(i).w_en.poke(true.B)
          dut.controlIO.regSetup(i).a.poke(0.U)
        }
        for (i <- 0 until p.lane) {
          //0x11
          dut.controlIO.regSetup(i*(p.stage+1)).in_en.poke(0.U)
          dut.controlIO.regSetup(i*(p.stage+1)).w_en.poke(true.B)
          dut.controlIO.regSetup(i*(p.stage+1)).a.poke(0.U)
          dut.vectorInput(i).poke(testA(i).U)
        }
        dut.clock.step()
        for (i <- 0 until p.lane) {
          //0x11
          dut.controlIO.regSetup(i*(p.stage+1)).in_en.poke(0.U)
          dut.controlIO.regSetup(i*(p.stage+1)).w_en.poke(true.B)
          dut.controlIO.regSetup(i*(p.stage+1)).a.poke(1.U)
          dut.vectorInput(i).poke(testB(i).U)
        }
        dut.clock.step()
        for (i <- 0 until 4) {
          dut.clock.step()
          Predef.println(s"Scala output = ${dut.scalaOutput.peek()}");
        }
        println(s"gold = ${gold}")
        dut.scalaOutput.expect(gold.U)
      }
    }
  }
}
