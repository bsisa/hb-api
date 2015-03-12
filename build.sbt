name := "hb-api"

version := "5.0.10"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  "io.github.cloudify" %% "spdf" % "1.0.0",
  "commons-codec" % "commons-codec" % "1.6"
)     

// Control which files are packaged with dist task.
// We exclude rsync backup files ending in ~, as well 
// as all dev and example configuration files found in conf folder.
mappings in (Compile, packageBin) ~= { _.filterNot { case (_, name) =>
      //if (name.endsWith("~") || name.endsWith(".conf.example") || name.endsWith("_dev.conf") || name.endsWith("application.conf")) { println(" EXCLUDE: name = " + name ) }
      name.endsWith("~") || name.endsWith(".conf.example") || name.endsWith("_dev.conf") || name.endsWith("application.conf")
}}

// Prevents File too long exception. 
// Noticed after last formatter addition to:
// ch.bsisa.hyperbird.model.format.Implicits
// https://issues.scala-lang.org/browse/SI-8199
// https://github.com/scala/pickling/issues/10
// https://github.com/spray/spray/issues/811
// Should be solved with Scala 2.11.0 
scalacOptions ++= Seq("-Xmax-classfile-name", "100") 

play.Project.playScalaSettings
