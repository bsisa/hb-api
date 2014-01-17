import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "hb-api"
  val appVersion = "5.0.0-SNAPSHOT"

  val appDependencies = Seq(
    "net.liftweb" %% "lift-json" % "2.5",
    "securesocial" %% "securesocial" % "2.1.2")

  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers += Resolver.url(
        "sbt-plugin-releases", 
        new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns))
}
