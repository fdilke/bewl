name := "bewl"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.11" % "test" withSources() withJavadoc(),
  "org.scalamock" % "scalamock-scalatest-support_2.11" % "3.1.1" % "test" withSources() withJavadoc(),
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.2" withSources() withJavadoc(),
  "org.mockito" % "mockito-all" % "1.9.5" % "test" withSources() withJavadoc()
)

