// import Mill dependency
import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
import mill.scalalib.TestModule.Utest
import mill.scalalib.publish._
// support BSP
import mill.bsp._

object playground extends common

trait common extends ScalaModule with ScalafmtModule with PublishModule { m =>
  override def scalaVersion = "2.12.13"
  override def scalacOptions = Seq(
    "-Xsource:2.11",
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    // Enables autoclonetype2 in 3.4.x (on by default in 3.5)
    "-P:chiselplugin:useBundlePlugin"
  )
  def chisel3Module: Option[PublishModule] = None
  override def ivyDeps =
    if (chisel3Module.isEmpty) Agg(ivy"edu.berkeley.cs::chisel3:3.4.3")
    else Agg.empty[Dep]
  override def scalacPluginIvyDeps =
    Agg(ivy"org.scalamacros:::paradise:2.1.1") ++
      (if (chisel3Module.isEmpty)
         Agg(ivy"edu.berkeley.cs:::chisel3-plugin:3.4.3")
       else Agg())

  def utestModule: Option[PublishModule] = None
  def chiseltestModule: Option[PublishModule] = None
  object test extends Tests with Utest {
    override def ivyDeps =
      m.ivyDeps() ++
        (if (utestModule.isEmpty) Agg(ivy"com.lihaoyi::utest:0.7.10")
         else Agg()) ++
        (if (chiseltestModule.isEmpty)
           Agg(ivy"edu.berkeley.cs::chiseltest:0.3.3")
         else Agg())
  }

  override def moduleDeps =
    Seq(chisel3Module, utestModule, chiseltestModule).flatten

  override def artifactName = "hermes-hwgen"
  def publishVersion = T { "0.0" }
  def pomSettings = T {
    PomSettings(
      description = artifactName(),
      organization = "moe.jsteward",
      url = "https://github.com/KireinaHoro/hermes-hwgen",
      licenses = Seq(License.`LGPL-3.0+`),
      versionControl = VersionControl.github("KireinaHoro", "hermes-hwgen"),
      developers = Seq.empty
    )
  }
}
