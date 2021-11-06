package hermes.testing
import chisel3._
import chisel3.tester._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import hermes._
import utest._

import scala.util.Random

object ReMuxSpec extends ChiselUtestTester with TestUtils {
  val tests = Tests {
    test("test reconfigurable mux") {
      implicit val p = Parameters()
      testCircuit(
        new ReMux(Seq(p.PRCount, p.PRCount, p.PRCount), 1),
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        for (ch <- 0 until 3) for (idx <- 0 until p.PRCount) {
          dut.channelInputs(ch)(idx).poke((ch * p.PRCount + idx).U)
        }
        for (ch <- 0 until 3) for (idx <- 0 until p.PRCount) {
          dut.controlIOs.c1(0).poke(ch.U)
          dut.controlIOs.c2(0).poke(idx.U)
          dut.channelOutputs(0).expect((ch * p.PRCount + idx).U);
          dut.clock.step()
        }
      }
      testCircuit(
        new ReMux(Seq(p.PRCount, p.PRCount, p.PRCount), 2),
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        for (ch <- 0 until 3) for (idx <- 0 until p.PRCount) {
          dut.channelInputs(ch)(idx).poke((ch * p.PRCount + idx).U)
        }
        for (ch <- 0 until 3) for (idx <- 0 until p.PRCount) {
          dut.controlIOs.c1(0).poke(ch.U)
          dut.controlIOs.c2(0).poke(idx.U)
          dut.controlIOs.c1(1).poke(ch.U)
          dut.controlIOs.c2(1).poke(((idx + 1) % p.PRCount).U)
          dut.channelOutputs(0).expect((ch * p.PRCount + idx).U);
          dut.channelOutputs(1).expect((ch * p.PRCount + (idx + 1) % p.PRCount).U);
          dut.clock.step()
        }
      }
    }
  }
}
