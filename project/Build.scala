import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "fudim_play"
  val appVersion = "1.1-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    anorm,
    "org.scalaz" %% "scalaz-core" % "7.0.2")

  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
    routesImport += "cube._",
    routesImport += "models._",
    routesImport += "support.Bindables._",
    lessEntryPoints <<= baseDirectory(customLessEntryPoints))

  def customLessEntryPoints(base: File): PathFinder = (base / "app" / "assets" / "stylesheets" * "*.less")
}
