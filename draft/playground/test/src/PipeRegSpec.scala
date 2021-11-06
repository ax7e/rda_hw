package hermes.testing

import chisel3._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.{TesterThreadList, VerilatorBackendAnnotation, WriteVcdAnnotation}
import utest._
import hermes._

import scala.util.Random
import scala.util.Random

object PipeRegSpec extends ChiselUtestTester {
  val tests = Tests {
    val waitWriterCycles = 1
    val maxInt = 128
    test("write through w") {
      implicit val p = Parameters()
      val testValues = for (x <- 0 until p.PRCount) yield (x, Random.nextInt(maxInt))
      testCircuit(new PipeReg(), Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        val writeSeq = testValues.map {
          case (x, y) => (true.B,x.U, y.U)
        }
        println(s"WriteSeq.length = ${writeSeq.length}")
        dut.io.in.en.poke(0.U)
        val t = fork {
          writeSeq.foreach { data =>
            dut.io.w.en.poke(data._1)
            dut.io.w.a.poke(data._2)
            dut.io.w.d.poke(data._3)
            println(s"reg[${data._2}]=${data._3}")
            dut.clock.step()
          }
        }.fork {
          dut.clock.step(writeSeq.length + waitWriterCycles)
          for (i <- 0 until p.PRCount) {
            dut.io.out(i).expect(writeSeq(i)._3)
            println(s"out($i) = ${dut.io.out(i).peek()}")
          }
        }.join()
      }
    }
    test("write through in") {
      implicit val p = Parameters()
      val testValues = for (x <- 0 until p.PRCount) yield (x, Random.nextInt(maxInt))
      testCircuit(new PipeReg(), Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        dut.io.in.en.poke(((1<<p.PRCount)-1).U)
        val t = fork {
          dut.io.w.en.poke(false.B)
          for (i <- 0 until p.PRCount) {
            dut.io.in.d(i).poke(testValues(i)._2.U)
          }
          dut.clock.step()
        }.fork {
          dut.clock.step(waitWriterCycles)
          for (i <- 0 until p.PRCount) {
            dut.io.out(i).expect(testValues(i)._2.U)
          }
        }.join()
      }
    }
    test("write through in and w") {
      implicit val p = Parameters()
      val wTestValues = for (x <- 0 until p.PRCount) yield (x, Random.nextInt(maxInt))
      val inTestValues = for (x <- 0 until p.PRCount) yield (x, Random.nextInt(maxInt))
      testCircuit(new PipeReg(), Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        dut.io.in.en.poke(((1<<p.PRCount)-1).U)
        val t = fork {
          dut.io.w.en.poke(false.B)
          for (i <- 0 until p.PRCount) {
            dut.io.in.d(i).poke(inTestValues(i)._2.U)
          }
          dut.clock.step()
          for (i <- 0 until p.PRCount) {
            dut.io.w.en.poke(true.B)
            dut.io.w.a.poke(wTestValues(i)._1.U)
            dut.io.w.d.poke(wTestValues(i)._2.U)
            dut.io.out(i).expect(inTestValues(i)._2.U)
            if (i > 0) {
              dut.io.out(i-1).expect(wTestValues(i-1)._2.U)
            }
            dut.clock.step()
          }
          dut.io.out(p.PRCount-1).expect(wTestValues(p.PRCount-1)._2.U)
        }.join()
      }

    }

  }
}
