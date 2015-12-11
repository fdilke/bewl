name := "bewl"

version := "1.0"

scalaVersion := "2.12.0-M3"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.0-M3",
  "org.scala-lang.modules" % "scala-xml_2.12.0-M3" % "1.0.5",
  "junit" % "junit" % "4.12" % "test",
  "org.scalamock" % "scalamock-scalatest-support_2.11" % "3.1.1" % "test",
  "org.mockito" % "mockito-all" % "1.10.19" % "test"
) map { _ withSources() withJavadoc() }



