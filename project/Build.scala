import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "fudim_play"
  val appVersion = "dev-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    anorm,
    "org.scalaz" %% "scalaz-core" % "7.0.2",
    "com.github.nscala-time" %% "nscala-time" % "0.4.0")

  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalaVersion := "2.10.3",

    autoCompilerPlugins := true,
    libraryDependencies += compilerPlugin("org.scala-lang.plugins" % "continuations" % scalaVersion.value),
    scalacOptions += "-P:continuations:enable",

    routesImport += "support.PointDefinition",
    routesImport += "support.Bindables._",
    lessEntryPoints <<= baseDirectory(customLessEntryPoints))

  def customLessEntryPoints(base: File): PathFinder = (base / "app" / "assets" / "stylesheets" * "*.less")
}
