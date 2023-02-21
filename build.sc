import $ivy.`com.goyeau::mill-scalafix::0.2.11`
import com.goyeau.mill.scalafix.ScalafixModule
import mill._, mill.scalalib._, mill.scalajslib._
import mill.scalajslib.api.{ModuleKind, ModuleSplitStyle, Report}
import mill.scalalib.scalafmt._
import mill.define.Task
import mill.define.Target
import coursier.maven.MavenRepository

val thisScalaVersion   = "2.13.8"
val thisScalaJSVersion = "1.12.0"

// plugins etc.
val kindProjectorVersion   = "0.13.2"
val organizeImportsVersion = "0.6.0"

// scala deps
// my libs
val jjmVersion = "0.2.3"
// other deps
val circeVersion         = "0.13.0"
val declineVersion       = "1.0.0"
val scalaJavaTimeVersion = "2.3.0"
val scalatagsVersion     = "0.8.2"
val http4sVersion        = "0.21.18"
// testing
val munitVersion           = "0.7.29"
val munitCatsEffectVersion = "1.0.7"

// jvm deps
val scalaCSVVersion = "1.3.10"
val logbackVersion  = "1.2.3"
val osLibVersion    = "0.8.0"

// scalajs deps
val scalajsDomVersion               = "1.1.0"
val scalajsJqueryVersion            = "1.0.0"
val scalacssVersion                 = "0.7.0"
val scalajsMacrotaskExecutorVersion = "1.0.0"

// raw JS
val jqueryVersion = "2.2.1"
val reactVersion  = "17.0.2"

// for some reason the $file import doesn't work anymore?
// import $file.`scripts-build`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps
trait SimpleJSDeps extends Module {
  def jsDeps = T {
    Agg.empty[String]
  }
  def downloadedJSDeps = T {
    for (url <- jsDeps())
      yield {
        val filename = url.substring(url.lastIndexOf("/") + 1)
        os.proc("curl", "-o", filename, url).call(cwd = T.ctx().dest)
        T.ctx().dest / filename
      }
  }
  def aggregatedJSDeps = T {
    val targetPath = T.ctx().dest / "jsdeps.js"
    os.write.append(targetPath, "")
    downloadedJSDeps().foreach { path =>
      os.write.append(targetPath, os.read(path))
      os.write.append(targetPath, "\n")
    }
    PathRef(targetPath)
  }
}

trait CommonModule extends ScalaModule with ScalafmtModule with ScalafixModule {
  def scalaVersion = thisScalaVersion

  def platformSegment: String

  override def sources = T.sources(millSourcePath / "src", millSourcePath / s"src-$platformSegment")

  override def repositoriesTask = T.task {
    super.repositoriesTask() ++
      Seq(MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"))
  }

  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ymacro-annotations",
    "-Ywarn-unused"
  )

  override def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++ Agg(ivy"org.typelevel:::kind-projector:$kindProjectorVersion")

  def scalafixIvyDeps = Agg(ivy"com.github.liancheng::organize-imports:$organizeImportsVersion")

  override def ivyDeps = Agg(
    // most of the FP dependencies are pulled in by JJM
    ivy"org.julianmichael::jjm-core::$jjmVersion",
    ivy"org.julianmichael::jjm-io::$jjmVersion",
    ivy"io.circe::circe-generic-extras::$circeVersion",
    ivy"io.github.cquiroz::scala-java-time::$scalaJavaTimeVersion"
  )

  trait CommonTestModule extends CommonModule with TestModule.Munit {
    override def scalaVersion = thisScalaVersion
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit::$munitVersion",
      ivy"org.typelevel::munit-cats-effect-2::$munitCatsEffectVersion"
    )
  }
}

object debate extends Module {
  object jvm extends CommonModule {
    def runMainFn = T.task { (mainClass: String, args: Seq[String]) =>
      import mill.api.Result
      import mill.modules.Jvm
      try Result.Success(
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

    def millSourcePath  = build.millSourcePath / "debate"
    def platformSegment = "jvm"

    override def mainClass = T(Some("debate.Serve"))

    override def ivyDeps =
      super.ivyDeps() ++
        Agg(
          ivy"org.julianmichael::jjm-corenlp::$jjmVersion",
          ivy"com.lihaoyi::os-lib:$osLibVersion",
          ivy"com.monovore::decline::$declineVersion",
          ivy"com.monovore::decline-effect::$declineVersion",
          ivy"com.lihaoyi::scalatags:$scalatagsVersion",
          ivy"org.http4s::http4s-blaze-client::$http4sVersion",
          ivy"com.github.tototoshi::scala-csv::$scalaCSVVersion",
          // java dependencies
          ivy"ch.qos.logback:logback-classic:$logbackVersion",
          ivy"io.circe::circe-generic-extras::$circeVersion"
        )

    object test extends super.Tests with CommonTestModule {
      def platformSegment = "jvm"
    }
  }

  object js extends CommonModule with ScalaJSModule with SimpleJSDeps {
    def millSourcePath  = build.millSourcePath / "debate"
    def platformSegment = "js"
    def scalaJSVersion  = thisScalaJSVersion

    // override def moduleKind = ModuleKind.ESModule
    // override def moduleSplitStyle = ModuleSplitStyle.FewestModules

    def mainClass = T(Some("debate.App"))

    import mill.scalajslib.ScalaJSWorkerApi
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

    override def ivyDeps =
      super.ivyDeps() ++
        Agg(
          ivy"org.julianmichael::jjm-ui::$jjmVersion",
          ivy"com.github.japgolly.scalacss::core::$scalacssVersion",
          ivy"com.github.japgolly.scalacss::ext-react::$scalacssVersion",
          ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
          ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
          ivy"org.scala-js::scala-js-macrotask-executor::$scalajsMacrotaskExecutorVersion"
        )

    def jsDeps = Agg(
      s"https://code.jquery.com/jquery-$jqueryVersion.min.js",
      s"https://unpkg.com/react@$reactVersion/umd/react.development.js",
      s"https://unpkg.com/react-dom@$reactVersion/umd/react-dom.development.js"
    )

    object test extends super.Tests with CommonTestModule {
      def platformSegment = "js"
      def scalaJSVersion  = T(thisScalaJSVersion)
      def moduleKind      = T(ModuleKind.ESModule)
    }
  }

  object dev extends Module {
    def serve(args: String*) = T.command {
      val runMain = jvm.runMainFn()
      runMain(
        "debate.Serve",
        Seq(
          "--js",
          js.fastestOpt().path.toString,
          "--jsDeps",
          js.aggregatedJSDeps().path.toString
        ) ++ args
      )
    }
  }

  object prod extends Module {
    def serve(args: String*) = T.command {
      val runMain = jvm.runMainFn()
      runMain(
        "debate.Serve",
        Seq("--js", js.fullOpt().path.toString, "--jsDeps", js.aggregatedJSDeps().path.toString) ++
          args
      )
    }
  }
}
