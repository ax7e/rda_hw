package hermes
import chisel3._
import chisel3.util._

class CoreMuxControlBundle(implicit val p: Parameters) extends Bundle {
  var reduceNodes = List[(Int, Int)]()
  for (i <- 0 until log2Ceil(p.lane)) {
    for (j <- 0 until p.lane >> (i + 1)) {
      val x = j << (i + 1);
      val y = 1 + i;
      reduceNodes = reduceNodes :+ (x, y)
    }
  }
  private var before = for (x <- 0 until p.lane) yield {
    for (y <- 0 until p.stage) yield {
      if (!reduceNodes.exists(a => a._1 == x && a._2 == y)) {
        val before = new ReMuxControlBundle(Seq(p.PRCount, p.PRCount, p.PRCount), 2)
        before
      } else {
        val before = new ReMuxControlBundle(Seq(p.PRCount, p.PRCount), 2)
        before
      }
    }
  }
  val muxBefore            = Input(MixedVec(before.flatten))
  private var gridMuxAfter = Array.tabulate(p.lane - 1, p.stage)((_, _) => new ReMuxControlBundle(Seq(1, p.PRCount), 1))
  val muxAfter             = Input(MixedVec(gridMuxAfter.flatten))
}

class ComputeCoreControlIO(implicit val p: Parameters) extends Bundle {
  val funcUnitCmd = Input(Vec(p.stage, UInt(p.funcUnitOpWidth.W)))
  val regSetup    = Vec(p.lane * (p.stage + 1), new RegControlBundle())
  val muxSetup    = new CoreMuxControlBundle()
}

class ComputeCore(implicit val p: Parameters) extends MultiIOModule {
  val controlIO    = IO(new ComputeCoreControlIO())
  val vectorInput  = IO(Input(Vec(p.lane, UInt(p.regDataWidth.W))))
  val scalaInput   = IO(Input(UInt(p.regDataWidth.W)))
  val scalaOutput  = IO(Output(UInt(p.regDataWidth.W)))
  val gridFuncUnit = Array.tabulate(p.lane, p.stage)((_, _) => Module(new FuncUnitSimple()))
  val gridReg      = Array.tabulate(p.lane, p.stage + 1)((_, _) => Module(new PipeReg()))

  for (i <- 0 until p.lane) {
    gridReg(i)(0).io.in.d  := DontCare
    gridReg(i)(0).io.in.en := 0.U
    gridReg(i)(0).io.w.d   := vectorInput(i)
  }
  //Scala Output is set to register 0 of the first lane and the first register
  scalaOutput := gridReg(0)(p.stage).io.out(0)
  for (i <- 0 until p.lane)
    for (j <- 0 until p.stage)
      gridFuncUnit(i)(j).io.FU_op := controlIO.funcUnitCmd(j)
  for (i <- 0 until p.lane)
    for (j <- 0 until p.stage + 1)
      gridReg(i)(j).connect(controlIO.regSetup(i * (p.stage + 1) + j))
  if (log2Ceil(p.lane) != log2Floor(p.lane)) {
    throw new RuntimeException("Lane count must be power of 2")
  }
  if (p.stage < 1 + log2Ceil(p.lane) + 1) {
    throw new RuntimeException("Parameter cannot support reduce network")
  }

  var reduceNodes = List[(Int, Int)]()
  for (i <- 0 until log2Ceil(p.lane)) {
    for (j <- 0 until p.lane >> (i + 1)) {
      val x = j << (i + 1);
      val y = 1 + i;
      reduceNodes = reduceNodes :+ (x, y)
    }
  }
  val before =
    for (x <- 0 until p.lane) yield for (y <- 0 until p.stage) yield {
      if (!reduceNodes.exists(a => a._1 == x && a._2 == y)) {
        val before = Module(new ReMux(Seq(p.PRCount, p.PRCount), 2))
        before.channelInputs(0) <> gridReg(x)(y + 1).io.out
        before.channelInputs(1) <> gridReg(x)(y).io.out
        before.channelOutputs(0) <> gridFuncUnit(x)(y).io.A
        before.channelOutputs(1) <> gridFuncUnit(x)(y).io.B
        before.connect(controlIO.muxSetup.muxBefore(x * p.stage + y))
        before
      } else {
        println(s"Reduce node at ($x,$y), link to (${x + (1 << (y - 1))},${y})")
        val before = Module(new ReMux(Seq(p.PRCount, p.PRCount, p.PRCount), 2))
        before.channelInputs(0) <> gridReg(x)(y + 1).io.out
        before.channelInputs(1) <> gridReg(x)(y).io.out
        before.channelInputs(2) <> gridReg(x + (1 << (y - 1)))(y).io.out
        before.channelOutputs(0) <> gridFuncUnit(x)(y).io.A
        before.channelOutputs(1) <> gridFuncUnit(x)(y).io.B
        before.connect(controlIO.muxSetup.muxBefore(x * p.stage + y))
        before
      }
    }

  // Link for mux after functional unit
  val gridMuxAfter = Array.tabulate(p.lane - 1, p.stage)((_, _) => Module(new ReMux(Seq(1, p.PRCount), 1)))
  for (x <- 0 until p.lane - 1) {
    for (y <- 0 until p.stage) {
      gridMuxAfter(x)(y).channelInputs(0)(0) <> gridFuncUnit(x)(y).io.out
      gridMuxAfter(x)(y).channelInputs(1) <> gridReg(x + 1)(y).io.out
      gridMuxAfter(x)(y).channelOutputs(0) <> gridReg(x)(y + 1).io.w.d
      gridMuxAfter(x)(y).connect(controlIO.muxSetup.muxAfter(x * p.stage + y))
    }
  }
  for (y <- 0 until p.stage) {
    val x = p.lane - 1
    gridFuncUnit(x)(y).io.out <> gridReg(x)(y + 1).io.w.d
  }

  //Link for pipelined register
  for (x <- 0 until p.lane) {
    for (y <- 0 until p.stage) {
      gridReg(x)(y + 1).io.in.d := gridReg(x)(y).io.out
    }
  }
  for (x <- 0 until p.lane) {
    for (y <- 0 until p.stage + 1) {
      gridReg(x)(y).connect(controlIO.regSetup(x * (p.stage + 1) + y))
    }
  }
  printf("Reg State\n")
  for (x <- 0 until p.lane) {
    printf("Lane %d", x.U)
    for (y <- 0 until p.stage + 1) {
      printf("|%d,%d", gridReg(x)(y).io.out(0),gridReg(x)(y).io.out(1))
      printf("(wen:%d)", gridReg(x)(y).io.w.en);
    }
    printf("\n")
  }
  printf("FU State\n")
  for (x <- 0 until p.lane) {
    printf("Lane %d", x.U)
    for (y <- 0 until p.stage) {
      printf("|%d,%d", gridFuncUnit(x)(y).io.A, gridFuncUnit(x)(y).io.B)
      printf("(out:%d)", gridFuncUnit(x)(y).io.out);
    }
    printf("\n")
  }
}
