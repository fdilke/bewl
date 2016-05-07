name := "bewl"

version := "1.0"

scalaVersion := "2.12.0-M4"

scalacOptions ++= Seq("-feature", "-deprecation", "-Xexperimental")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.0-M4",
  "org.scala-lang.modules" % "scala-xml_2.12.0-M4" % "1.0.5",
  "junit" % "junit" % "4.12" % "test",
  "org.scalatest" % "scalatest_2.12.0-M4" % "2.2.6"
) map {
  _ withSources() withJavadoc()
}



