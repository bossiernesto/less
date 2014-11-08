name := "less"

description := ""

scalaVersion := "2.11.1"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val cacao = FDProject(
 		"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "org.scala-lang" % "scala-swing" % "2.11.0-M7", 
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"