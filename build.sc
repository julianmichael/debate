import $ivy.`com.goyeau::mill-scalafix::0.2.11`
import com.goyeau.mill.scalafix.ScalafixModule
import mill.define.{Command, Target, Task}
import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import os._

import mill.scalajslib.api.{ModuleKind, ModuleSplitStyle}

// val thisPublishVersion = "0.1.0-SNAPSHOT"
val thisScalaVersion = "2.13.10"
val thisScalaJSVersion = "1.12.0"

// plugins
val kindProjectorVersion = "0.13.2"

// scala deps
// my libs
val jjmVersion = "0.3.0-SNAPSHOT"
// other deps
val circeVersion = "0.14.3"
val declineVersion = "2.4.0"
// testing
val munitVersion = "0.7.29"
val munitCatsEffectVersion = "1.0.7"

// jvm deps
val logbackVersion = "1.2.3"
val osLibVersion = "0.8.0"

// scalajs deps
val jqueryFacadeVersion = "2.1"
val scalajsMacrotaskExecutorVersion = "1.0.0"

// raw JS
val jsCookieVersion = "3.0.1"
val jqueryVersion = "2.1.4"
val reactVersion = "17.0.2"

trait CommonModule extends ScalaModule with ScalafmtModule with ScalafixModule {

  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }

  def scalaVersion = thisScalaVersion

  def platformSegment: String

  override def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ymacro-annotations",
    "-Ywarn-unused"
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion"
  )

  override def ivyDeps = Agg(
    // most of the FP dependencies are pulled in by JJM
    ivy"org.julianmichael::jjm-core::$jjmVersion",
    ivy"org.julianmichael::jjm-io::$jjmVersion",
    ivy"io.circe::circe-generic-extras::$circeVersion",
    // ivy"org.typelevel::kittens::$kittensVersion",
    ivy"io.github.cquiroz::scala-java-time::2.3.0"
  )

  trait CommonTestModule extends CommonModule with TestModule.Munit {
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit::$munitVersion",
      ivy"org.typelevel::munit-cats-effect-2::$munitCatsEffectVersion"
    )
  }
}

trait JsPlatform extends CommonModule with ScalaJSModule {
  def scalaJSVersion = T(thisScalaJSVersion)
  def platformSegment = "js"

  import mill.scalajslib.{ScalaJSWorkerApi}
  import mill.scalajslib.api.{OptimizeMode, FastOpt}
  // copied from
  // https://github.com/com-lihaoyi/mill/blob/0.10.3/scalajslib/src/ScalaJSModule.scala
  // TODO: move this to a non-deprecated API when possible.
  // the point is to give us a `fastestOpt` target that uses no optimization,
  // even when it's set to true in the module.
  private def linkTaskCustom(mode: OptimizeMode): Task[PathRef] = T.task {
    link(
      worker = ScalaJSWorkerApi.scalaJSWorker(),
      toolsClasspath = scalaJSToolsClasspath(),
      runClasspath = runClasspath(),
      mainClass = finalMainClassOpt().toOption,
      testBridgeInit = false,
      mode = mode,
      moduleKind = moduleKind(),
      esFeatures = esFeatures()
    )
  }

  def fastestOpt: Target[PathRef] = T {
    linkTaskCustom(mode = FastOpt)()
  }

  trait Tests extends super.Tests with CommonTestModule {
    override def scalaVersion = thisScalaVersion
    def scalaJSVersion = T(thisScalaJSVersion)
    def platformSegment = "js"
    def moduleKind = T(mill.scalajslib.api.ModuleKind.CommonJSModule)
  }
}

trait JvmPlatform extends CommonModule {
  def platformSegment = "jvm"

  trait Tests extends super.Tests with CommonTestModule {
    override def scalaVersion = thisScalaVersion
    def platformSegment = "jvm"
  }
}

import $file.`build-scripts`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps

object debate extends Module {
  trait DebateModule extends CommonModule {
    def millSourcePath = build.millSourcePath / "debate"
  }

  trait JvmBase extends DebateModule with JvmPlatform {

    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::os-lib:$osLibVersion",
      // ivy"com.lihaoyi::scalatags:0.8.2",
      ivy"com.lihaoyi::scalatags:0.8.2",
      ivy"com.monovore::decline::$declineVersion",
      ivy"com.monovore::decline-effect::$declineVersion",
      // java dependencies
      ivy"ch.qos.logback:logback-classic:$logbackVersion"
    )

    def runMainFn = T.task { (mainClass: String, args: Seq[String]) =>
      import mill.api.Result
      import mill.modules.Jvm
      try
        Result.Success(
          Jvm.runSubprocess(
            mainClass,
            runClasspath().map(_.path),
            forkArgs(),
            forkEnv(),
            args,
            workingDir = forkWorkingDir(),
            useCpPassingJar = runUseArgsFile()
          )
        )
      catch {
        case e: Exception =>
          Result.Failure("subprocess failed")
      }
    }

  }

  object jvm extends JvmBase {
    object test extends Tests
  }

  object dev extends Module {
    def serve(args: String*) = T.command {
      val runMain = jvm.runMainFn()
      // Turn off optimization in the scalajs linker
      // so that we can get fast incremental compilation
      // of the Scala.js code
      runMain(
        "debate.Serve",
        (Seq(
          "--js",
          js.fastestOpt().path.toString,
          "--jsDeps",
          js.aggregatedJSDeps().path.toString
        ) ++ args)
      )
    }
  }

  // deploy using fullOpt JS, it's MUCH more compact
  object prod extends Module {
    def serve(args: String*) = T.command {
      val runMain = jvm.runMainFn()
      runMain(
        "debate.Serve",
        Seq(
          "--js",
          js.fullOpt().path.toString,
          "--jsDeps",
          js.aggregatedJSDeps().path.toString
        ) ++ args
      )
    }
  }

  object js extends DebateModule with JsPlatform with SimpleJSDeps {

    // override def moduleSplitStyle = ModuleSplitStyle.SmallModulesFor(List("debate"))
    // override def moduleKind = ModuleKind.CommonJSModule

    def mainClass = T(Some("debate.App"))

    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::jjm-ui::$jjmVersion",
      ivy"org.querki::jquery-facade::$jqueryFacadeVersion",
      ivy"org.scala-js::scala-js-macrotask-executor::$scalajsMacrotaskExecutorVersion",
    )

    def jsDeps = Agg(
      s"https://cdn.jsdelivr.net/npm/js-cookie@$jsCookieVersion/dist/js.cookie.min.js",
      s"https://code.jquery.com/jquery-$jqueryVersion.min.js",
      s"https://cdnjs.cloudflare.com/ajax/libs/react/$reactVersion/react.js",
      s"https://cdnjs.cloudflare.com/ajax/libs/react/$reactVersion/react-dom.js"
    )
  }
}
