package hermes.testing

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._
import utest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}

import scala.util.Random
import hermes._

import scala.language.postfixOps

/** Dense matrix multiplication using inner product dataflow. */
abstract class MatrixInnerProductPEBase(implicit val p: Parameters)
    extends PEBase {
  val dimSize = 1 << p.memAWidth

  // 3 channel(s):
  // - map_3i (ID gen for ALU Mem) - masked
  // - map_3i (ID gen for ALU Reg) - masked
  // - map_3i (ID gen for Reduce) - masked

  // Note: it is also possible to use 1-channel outer map and broadcast its child to inner map

  // 3 channel(s):
  // - ALU Mem (left matrix row points)
  // - ALU Reg (right matrix column points)
  // - Reduce (output buffer points) - passthrough
  val map_3i = Module(new Map(dimSize, numInputs = 3, Set(2)))
  val map_3o = Module(new Map(dimSize, numInputs = 3, Set(0)))
  done := map_3o.done && !outMemWr.valid
  map_3o.connectParent(broadcastID(cmd, 3, Set(0, 1, 2)))
  map_3o.connectChildDone(Seq.fill(3){map_3i.done})
  map_3i.connectParent(map_3o.childIDs)
  map_3i.connectChildDone(Seq.fill(3){true.B})

  val (aluChans, reduceChan) = map_3i.childIDs splitAt 2

  val alu = Module(new Arithmetic(_ * _))
  alu.connectParent(aluChans)

  val reduce = Module(new Reduce(Left(_ + _)))
  reduce.connectParent(reduceChan)

  inRegRd <> alu.inReg
  inMemRd <> alu.inMem
  reduce.inData <> alu.outData
  reduce.inMem <> outMemRd
  reduce.outMem <> outMemWr
}
class MatrixInnerProductPEDUT(implicit override val p: Parameters)
    extends MatrixInnerProductPEBase
    with SingleRegReaderDUT
class MatrixInnerProductPEShared(implicit override val p: Parameters)
    extends MatrixInnerProductPEBase
    with SharedPEImpl

class InnerProductPEArrayDUT(nPEs: Int)(implicit p: Parameters)
    extends MultiIOModule {
  val reg = Module(new SharedRegister(nPEs))

  val cmd = IO(Flipped(Decoupled(ID)))
  val enable = IO(Input(Bool()))
  val done = IO(Output(Bool()))
  val inRegWr = IO(Flipped(reg.writer.cloneType))

  // inner product: everyone takes 0 ID - broadcast
  val (peMems, peDones) =
    generateID(cmd, nPEs, { case (_, idx) => idx.U }).zipWithIndex map {
      case (c, idx) =>
        val pe = Module(new MatrixInnerProductPEShared)
        pe.inRegRd <> reg.readers(idx)
        pe.cmd <> c
        pe.enable := enable

        val inMemWr =
          IO(Flipped(pe.inMemWr.cloneType)).suggestName(f"pe_${idx}_inMemWr")
        inMemWr <> pe.inMemWr
        val outMemExtWr = IO(Flipped(pe.outMemExtWr.cloneType))
          .suggestName(f"pe_${idx}_outMemExtWr")
        outMemExtWr <> pe.outMemExtWr
        val outMemExtRd =
          IO(pe.outMemExtRd.cloneType).suggestName(f"pe_${idx}_outMemExtRd")
        outMemExtRd <> pe.outMemExtRd
        ((inMemWr, outMemExtWr, outMemExtRd), pe.done)
    } unzip

  done := peDones.foldLeft(true.B)(_ && _)
  inRegWr <> reg.writer
}

object InnerProduct extends ChiselUtestTester {
  val maxInt = 128
  implicit val p = Parameters(memAWidth = 4, regAWidth = 8)
  val tests = Tests {
    test("matrix_inner_product") {
      val leftValues = Seq.fill(1 << p.memAWidth) {
        Random.nextInt(maxInt)
      }
      val rightValues = Seq.fill(1 << p.regAWidth) {
        Random.nextInt(maxInt)
      }
      val outValues = rightValues.grouped(leftValues.length) map { rr =>
        (leftValues zip rr).map(a => a._1 * a._2).sum
      } toSeq

      testCircuit(
        new MatrixInnerProductPEDUT,
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
          pokeCmd(Seq(0), dut.cmd, dut.clock)
        } fork {
          waitAndCheckMem(rightValues.length, dut, outValues)
        } join ()
      }
    }
    test("shared_inner_product") {
      val nPEs = 16
      def genMatrix = Seq.fill(1 << p.regAWidth) {
        Random.nextInt(maxInt)
      }
      val leftMatrix = genMatrix
      val rightMatrix = genMatrix
      val outRows = leftMatrix.grouped(nPEs) map { lr =>
        rightMatrix.grouped(nPEs) map { rr =>
          (lr zip rr).map(a => a._1 * a._2).sum
        } toSeq
      } toSeq

      testCircuit(
        new InnerProductPEArrayDUT(nPEs),
        Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
      ) { dut =>
        dut.enable.poke(true.B)

        val launchThreadList = leftMatrix
          .grouped(nPEs)
          .zipWithIndex
          .foldLeft(fork {
            fillReg(rightMatrix, dut.inRegWr, dut.clock)
          }) {
            case (threadList, (data, idx)) =>
              threadList fork {
                // fill input memory bank
                fillMem(data, dut.peMems(idx)._1, dut.clock)
              }
          }
          .fork {
            dut.clock.step(rightMatrix.length)
            while (!dut.cmd.ready.peek.litToBoolean) dut.clock.step()
            pokeCmd(Seq(0), dut.cmd, dut.clock)
          }
          .fork {
            dut.clock.step(rightMatrix.length + 1)
            while (!dut.done.peek.litToBoolean) dut.clock.step()
            dut.enable.poke(false.B)
            // wait for out memory check before enable goes back to low
            dut.clock.step(outRows.head.length + 1)
          }

        outRows.zipWithIndex
          .foldLeft(launchThreadList) {
            case (threadList, (data, idx)) =>
              threadList fork {
                dut.clock.step(rightMatrix.length + 1)
                while (!dut.done.peek.litToBoolean) dut.clock.step()
                checkMem(data, dut.peMems(idx)._3, dut.clock)
              }
          }
          .join()
      }
    }
  }
}
