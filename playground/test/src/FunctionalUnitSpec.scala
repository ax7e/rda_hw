package hermes.testing

import chisel3.Num.toBigInt
import chisel3._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.{TesterThreadList, VerilatorBackendAnnotation, WriteVcdAnnotation}
import utest._
import hermes._

import scala.util.Random
import scala.util.Random

object FunctionalUnitSpec extends ChiselUtestTester with TestUtils {
  val tests = Tests {
    val waitWriterCycles = 1
    test("functional unit test") {
      implicit val p = Parameters()
      val maxInt = 128
      val testA = { Seq.fill(p.PRCount) { Random.nextInt(maxInt) } }
      val testB = { Seq.fill(p.PRCount) { Random.nextInt(maxInt) } }
      val OP = Seq(
        (FuncUnit.FU_ADD, (x: Int, y: Int) => x + y),
        (FuncUnit.FU_SUB, (x: Int, y: Int) => x - y),
        (FuncUnit.FU_MUL, (x: Int, y: Int) => x * y)
      )
      val gold = for {
        a <- testA.zip(testB)
        op <- OP
      } yield { (op._1, a._1.U, a._2.U, toBigInt(op._2(a._1, a._2)).U(p.regDataWidth.W)) }
      testCircuit(new FuncUnitSimple(), Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        for ( (op, a, b, goldRes) <- gold ) {
          dut.io.A.poke(a)
          dut.io.B.poke(b)
          dut.io.FU_op.poke(op)
          dut.io.out.expect(goldRes)
          dut.clock.step()
        }
      }
    }
  }
}
