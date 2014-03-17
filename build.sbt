name := "hb-api"

version := "5.0.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  cache
)     

// Prevents File too long exception. 
// Noticed after last formatter addition to:
// ch.bsisa.hyperbird.model.format.Implicits
// https://issues.scala-lang.org/browse/SI-8199
// https://github.com/scala/pickling/issues/10
// https://github.com/spray/spray/issues/811
// Should be solved with Scala 2.11.0 
scalacOptions ++= Seq("-Xmax-classfile-name", "100") 

play.Project.playScalaSettings
