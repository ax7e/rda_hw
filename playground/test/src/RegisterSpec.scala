package hermes.testing

import chisel3._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.TesterThreadList
import utest._

import hermes._

import scala.util.Random

object RegisterSpec extends ChiselUtestTester {
  val tests = Tests {
    test("random_simultaneous_read") {
      val regAWidth = 4
      val waitWriterCycles = 4
      val readerRounds = 32
      val numReaders = 4
      val numRegs = 1 << regAWidth

      implicit val p = Parameters(regAWidth = regAWidth)
      val testValues = for { x <- 0 until numRegs } yield
        (x, Random.nextInt(1 << p.regDWidth))

      println(
        f"Testing for $numReaders readers for $readerRounds rounds with awidth = $regAWidth"
      )

      testCircuit(new SharedRegister(numReaders)) { dut =>
        val writerSeq = testValues map {
          case (x, y) => (new RegFileWriteBundle).Lit(_.a -> x.U, _.d -> y.U)
        }
        def genRandomReads(numReads: Int) = Seq.fill(numReads) {
          val addr = Random.nextInt(numRegs)
          (new RegFileBundle).Lit(_.a -> addr.U, _.d -> testValues(addr)._2.U)
        }
        val rSeqs = Seq.fill(numReaders)(genRandomReads(readerRounds))

        def buildReader(t: TesterThreadList, readerID: Int) =
          t.fork {
            dut.clock.step(writerSeq.length + waitWriterCycles)
            rSeqs(readerID) foreach { o =>
              dut.readers(readerID).a.poke(o.a)
              dut.readers(readerID).d.expect(o.d)
              dut.clock.step()
            }
          }

        var t = fork {
          writerSeq foreach { data =>
            dut.writer.valid.poke(true.B)
            dut.writer.bits.poke(data)
            dut.clock.step()
          }
          dut.writer.valid.poke(false.B)
        }
        Seq.tabulate(numReaders) { id =>
          t = buildReader(t, id)
        }
        t.join()
      }
    }
  }
}
