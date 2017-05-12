name := "bewl"

version := "1.0"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-feature", "-deprecation", "-Xexperimental")

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.12" % "test" exclude("org.scala-lang.modules", "scala-xml_2.12"),
  "org.scalatest" %% "scalatest" % "3.0.1" exclude("org.scala-lang.modules", "scala-xml_2.12"),
  "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"
) map {
  _ withSources() withJavadoc()
}



