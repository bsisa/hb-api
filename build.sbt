name := "hb-api"

version := "5.0.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  cache
)     

play.Project.playScalaSettings
