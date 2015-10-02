name := "bewl"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.4",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.2",
  "junit" % "junit" % "4.11" % "test",
  "org.scalamock" % "scalamock-scalatest-support_2.11" % "3.1.1" % "test",
  "org.mockito" % "mockito-all" % "1.9.5" % "test"
) map { _ withSources() withJavadoc() }



