import sbt._
import Keys._
// Play plugin
import play.Project._

// Scalaxb plugin
import sbtscalaxb.Plugin._
import ScalaxbKeys._

object ApplicationBuild extends Build {

  val appName = "hb-api"
  val appVersion = "5.0.0-SNAPSHOT"

  val appDependencies = Seq(
    "org.apache.poi" % "poi" % "3.10-FINAL",
    "org.apache.poi" % "poi-ooxml" % "3.10-FINAL",
    "commons-io" % "commons-io" % "2.4",
    "org.jsoup" % "jsoup" % "1.7.3",
    "net.liftweb" %% "lift-json" % "2.5",
    "securesocial" %% "securesocial" % "2.1.2"
    )
    
  val main = play.Project(
    appName,
    appVersion,
    appDependencies,
    settings = Defaults.defaultSettings ++ scalaxbSettings).settings(
      
      // Making test output logs
      javaOptions in Test += "-Dlogger.file=conf/test-logger.xml",
      // ========================================================================        
      // Sbt-plugin settings
      // ========================================================================        
      resolvers += Resolver.url(
        "sbt-plugin-releases",
        new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
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
