import sbt._
import Keys._
// Play plugin
import play.Project._

// Scalaxb plugin
import sbtscalaxb.Plugin._
import ScalaxbKeys._

object ApplicationBuild extends Build {

  val appName = "hb-api"
  val appVersion = "5.4.17"

  val appDependencies = Seq(
    "io.github.cloudify" %% "spdf" % "1.3.1",
    "org.apache.poi" % "poi-ooxml" % "3.17",
    "org.apache.poi" % "ooxml-schemas" % "1.4",
    "commons-io" % "commons-io" % "2.4",
    "org.jsoup" % "jsoup" % "1.7.3",
    "net.liftweb" %% "lift-json" % "2.5",
    "ws.securesocial" % "securesocial_2.10" % "2.1.3",
    "org.apache.commons" % "commons-email" % "1.4"
  )

  //     "org.scalatestplus" %% "play" % "1.0.0" % "test"
  //libraryDependencies += "org.apache.commons" % "commons-email" % "1.3.3"


  val main = play.Project(
    appName,
    appVersion,
    appDependencies,
    settings = Defaults.defaultSettings ++ scalaxbSettings).settings(

    // Configure this on a per spec basis
    //parallelExecution in Test := false

    // Making test output logs
    javaOptions in Test += "-Dlogger.file=conf/test-logger.xml",

    testOptions in Test += sbt.Tests.Exclude(Seq("test.ch.bsisa.hyperbird.mail.SendMailSpec", "test.ch.bsisa.hyperbird.report.PdfFileMergingHelperSpec")),
    // ========================================================================
    // Repositories settings for:
    //  * sbt-plugins
    //  * ws.securesocial now available at Maven repository (OSS Sonatype)
    // ========================================================================

    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.url(
        "sbt-plugin-releases",
        new URL("https://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
    ),
    // ========================================================================
    // Scalaxb settings
    // ========================================================================
    // Warning: Only uncomment the next line (sourceGenerators config)
    // when willing to activate Scalaxb source generation. For instance
    // when working on geoXml XSD changes or testing the model classes
    // generation process through Scalaxb is still effective.
    // Otherwise having it active is terribly heavy with continuous
    // IDE compilation for instance.
    // ========================================================================
    //sourceGenerators in Compile <+= scalaxb in Compile,
    packageName in scalaxb in Compile := "ch.bsisa.hyperbird.model",
    protocolFileName in scalaxb in Compile := "geoXmlProtocol",
    protocolPackageName in scalaxb in Compile := Option("ch.bsisa.hyperbird.model.proto"))

}
