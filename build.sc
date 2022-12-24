import $ivy.`com.goyeau::mill-scalafix::0.2.11`
import com.goyeau.mill.scalafix.ScalafixModule
import mill._, mill.scalalib._, mill.scalajslib._
import mill.scalajslib.api.{ModuleKind, ModuleSplitStyle, Report}
import mill.scalalib.scalafmt._
import mill.define.Task
import coursier.maven.MavenRepository

val thisScalaVersion = "2.13.8"
val thisScalaJSVersion = "1.12.0"

// plugins
val kindProjectorVersion = "0.13.2"

// scala deps
// my libs
val jjmVersion = "0.2.3"
// other deps
val circeVersion = "0.13.0"
val declineVersion = "1.0.0"
// testing
val munitVersion = "0.7.29"
val munitCatsEffectVersion = "1.0.7"

// jvm deps
val logbackVersion = "1.2.3"
val osLibVersion = "0.8.0"

// scalajs deps
val scalajsDomVersion = "1.1.0"
val scalajsJqueryVersion = "1.0.0"
val scalacssVersion = "0.7.0"
val scalajsMacrotaskExecutorVersion = "1.0.0"

def public(jsTask: Task[Report]): Task[Map[String, os.Path]] = T.task {
  Map("@public" -> jsTask().dest.path)
}

trait CommonModule extends ScalaModule with ScalafmtModule with ScalafixModule {
  def scalaVersion = thisScalaVersion

  def platformSegment: String

  override def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }

  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ymacro-annotations",
    "-Ywarn-unused"
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion"
  )

  override def ivyDeps = Agg(
    // most of the FP dependencies are pulled in by JJM
    ivy"org.julianmichael::jjm-core::$jjmVersion",
    ivy"org.julianmichael::jjm-io::$jjmVersion",
    ivy"io.circe::circe-generic-extras::$circeVersion",
    ivy"io.github.cquiroz::scala-java-time::2.3.0"
  )

  trait CommonTestModule extends CommonModule with TestModule.Munit {
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit::$munitVersion",
      ivy"org.typelevel::munit-cats-effect-2::$munitCatsEffectVersion"
    )
  }
}

object debate extends Module {

  object js extends CommonModule with ScalaJSModule {
    def millSourcePath = build.millSourcePath / "debate"
    def platformSegment = "js"
    def scalaJSVersion = thisScalaJSVersion

    override def moduleKind = ModuleKind.ESModule
    override def moduleSplitStyle =
      ModuleSplitStyle.SmallModulesFor(List("debate"))

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::jjm-ui::$jjmVersion",
      ivy"com.github.japgolly.scalacss::core::$scalacssVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalacssVersion",
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"org.scala-js::scala-js-macrotask-executor::$scalajsMacrotaskExecutorVersion"
    )

    def publicDev = T {
      public(fastLinkJS)()
    }

    def publicProd = T {
      public(fullLinkJS)()
    }
    object test extends super.Tests with CommonTestModule {
      def platformSegment = "js"
      override def scalaVersion = thisScalaVersion
      def scalaJSVersion = T(thisScalaJSVersion)
      def moduleKind = T(ModuleKind.ESModule)
    }
  }

  object jvm extends CommonModule {
    def millSourcePath = build.millSourcePath / "debate"
    def platformSegment = "jvm"

    override def mainClass = T(Some("debate.Serve"))

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::jjm-corenlp::$jjmVersion",
      ivy"com.lihaoyi::os-lib:$osLibVersion",
      ivy"com.lihaoyi::scalatags:0.8.2",
      ivy"com.monovore::decline::$declineVersion",
      ivy"com.monovore::decline-effect::$declineVersion",
      // java dependencies
      ivy"ch.qos.logback:logback-classic:$logbackVersion",
      ivy"io.circe::circe-generic-extras::$circeVersion"
    )

    object test extends super.Tests with CommonTestModule {
      def platformSegment = "jvm"
      override def scalaVersion = thisScalaVersion
    }
  }
}

