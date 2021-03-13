// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers ++= Seq( 
				"Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
				"sonatype-public" at "https://oss.sonatype.org/content/groups/public/")

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.2.6")

// Use the Scalaxb sbt plugin to generate Scala classes from XSD
addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "1.1.2")
 
