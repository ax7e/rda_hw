package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.WriteVcdAnnotation
import scala.util.Random
import utest._

import hermes._

class SimpleMapMultiplyPE(testSize: Int)(implicit val p: Parameters)
    extends PEBase
    with SingleRegReaderDUT
    with NoOutMem {
  val map = Module(new Map(testSize))
  val alu = Module(new Arithmetic(_ * _))
  val result = IO(Valid(MemData))

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  result <> alu.outData

  map.connectParent(Seq(cmd))
  map.connectChildDone(Seq.fill(1){true.B})
  alu.connectParent(broadcastID(map.childIDs.head, 2, Set(1)))

  done := map.done
}

class NestedMapMultiplyPE(dimSize: Int)(implicit val p: Parameters)
    extends PEBase
    with SingleRegReaderDUT
    with NoOutMem {
  val map_i = Module(new Map(dimSize))
  val map_o = Module(new Map(dimSize))
  val alu = Module(new Arithmetic(_ * _))
  val result = IO(Valid(MemData))

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  result <> alu.outData

  map_o.connectParent(Seq(cmd))
  map_i.connectParent(map_o.childIDs)
  map_o.connectChildDone(Seq.fill(1){true.B})
  map_i.connectChildDone(Seq.fill(1){true.B})
  alu.connectParent(broadcastID(map_i.childIDs.head, 2, Set(1)))

  done := map_o.done
}

class ZipMultiplyPE(dimSize: Int)(implicit val p: Parameters)
    extends PEBase
    with SingleRegReaderDUT
    with NoOutMem {
  val map_2 = Module(new Map(dimSize, numInputs = 2))
  val alu = Module(new Arithmetic(_ * _))
  val result = IO(Valid(MemData))

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  result <> alu.outData

  map_2.connectParent(broadcastID(cmd, 2))
  map_2.connectChildDone(Seq.fill(2){true.B})
  alu.connectParent(map_2.childIDs)

  done := map_2.done
}

object MapSpec extends ChiselUtestTester {
  val tests = Tests {
    test("dense_map_multiply") {
      implicit val p = Parameters(regAWidth = 1, memAWidth = 5)
      val nElems = 1 << p.memAWidth
      val scale = Random.nextInt(32)
      val values = Seq.fill(nElems)(Random.nextInt(maxInt))

      testCircuit(new SimpleMapMultiplyPE(nElems), Seq(WriteVcdAnnotation)) {
        dut =>
          fork {
            fillReg(Seq(scale), dut.inRegWr, dut.clock)
            fillMem(values, dut.inMemWr, dut.clock)
          } fork {
            dut.clock.step(nElems + 4)

            while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
            pokeCmd(Seq(0), dut.cmd, dut.clock)
          } fork {
            checkResult(values.map(_ * scale), dut.result, dut.clock)
            checkExit(dut.result.valid, dut.clock)
          } join ()
      }
    }
    test("matrix_scale") {
      implicit val p = Parameters(regAWidth = 1, memAWidth = 6) // 8x8 matrix
      val nElems = 8 // per dimension
      val scale = Random.nextInt(32)
      val values = Seq.fill(nElems * nElems)(Random.nextInt(maxInt))

      testCircuit(new NestedMapMultiplyPE(nElems), Seq(WriteVcdAnnotation)) {
        dut =>
          fork {
            fillReg(Seq(scale), dut.inRegWr, dut.clock)
            fillMem(values, dut.inMemWr, dut.clock)
          } fork {
            dut.clock.step(values.length + 4)

            while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
            pokeCmd(Seq(0), dut.cmd, dut.clock)
          } fork {
            checkResult(values.map(_ * scale), dut.result, dut.clock)
            checkExit(dut.result.valid, dut.clock)
          } join ()
      }
    }
    test("vector_product") {
      implicit val p = Parameters(regAWidth = 4, memAWidth = 4)
      val nElems = 16
      val values = Seq.fill(nElems, 2)(Random.nextInt(maxInt))
      testCircuit(new ZipMultiplyPE(nElems), Seq(WriteVcdAnnotation)) { dut =>
        fork {
          fillReg(values.map(_(0)), dut.inRegWr, dut.clock)
          fillMem(values.map(_(1)), dut.inMemWr, dut.clock)
        } fork {
          dut.clock.step(2 * nElems + 4)

          while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
          pokeCmd(Seq(0), dut.cmd, dut.clock)
        } fork {
          checkResult(values.map(_.product), dut.result, dut.clock)
          checkExit(dut.result.valid, dut.clock)
        } join ()
      }
    }
  }
}
