name := "bewl"

version := "1.0"

scalaVersion := "2.12.0"

scalacOptions ++= Seq("-feature", "-deprecation", "-Xexperimental")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.0",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "junit" % "junit" % "4.12" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1"
) map {
  _ withSources() withJavadoc()
}



