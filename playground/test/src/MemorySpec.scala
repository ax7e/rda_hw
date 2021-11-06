package hermes.testing

import chisel3._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.{
  TesterThreadList,
  VerilatorBackendAnnotation,
  WriteVcdAnnotation
}
import utest._
import hermes._

import scala.util.Random

object MemorySpec extends ChiselUtestTester {
  val tests = Tests {
    test("simple_read") {
      implicit val p = Parameters(memAWidth = 4) // 16 elements
      val values =
        Seq.fill(1 << p.memAWidth)(Random.nextInt(maxInt))
      testCircuit(
        new BankedMemory,
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        fork {
          fillMem(values, dut.writer, dut.clock)
        } fork {
          dut.clock.step(values.length + 4)
          checkMem(values, dut.reader, dut.clock)
        } join ()
      }
    }
    test("simultaneous_same_addr_rw") {
      implicit val p = Parameters(memAWidth = 4)
      val values =
        Seq.fill(1 << p.memAWidth) {
          (Random.nextInt(maxInt), Random.nextInt(maxInt))
        }
      testCircuit(
        new BankedMemory(SyncReadMem.ReadFirst),
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        fork {
          fillMem(values.map(_._1), dut.writer, dut.clock)
        } fork {
          dut.clock.step(values.length + 4)
          checkMem(values.map(_._1), dut.reader, dut.clock)
        } fork {
          dut.clock.step(values.length + 4)
          fillMem(values.map(_._2), dut.writer, dut.clock)
        } fork {
          dut.clock.step(2 * values.length + 8)
          checkMem(values.map(_._2), dut.reader, dut.clock)
        } join ()
      }
    }
    test("simultaneous_different_addr_rw") {
      implicit val p = Parameters(memAWidth = 4)
      val values =
        Seq.fill(1 << p.memAWidth) {
          (Random.nextInt(maxInt), Random.nextInt(maxInt))
        }
      testCircuit(
        new BankedMemory,
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        fork {
          fillMem(values.map(_._1), dut.writer, dut.clock)
        } fork {
          dut.clock.step(values.length + 4)
          checkMem(values.map(_._1), dut.reader, dut.clock)
        } fork {
          dut.clock.step(values.length + 8) // shift apart from read thread
          fillMem(values.map(_._2), dut.writer, dut.clock)
        } fork {
          dut.clock.step(2 * values.length + 16)
          checkMem(values.map(_._2), dut.reader, dut.clock)
        } join ()
      }
    }
  }
}
