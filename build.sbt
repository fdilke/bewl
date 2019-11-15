name := "bewl"

version := "1.0"

scalaVersion := "2.13.1"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += "Maven central" at "http://repo1.maven.org/maven2/"
resolvers += Resolver.sonatypeRepo("releases")

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.12" % "test" exclude(
    "org.scala-lang.modules", "scala-xml_2.13"
  ),
  "org.scalatest" %% "scalatest" % "3.2.0-M1" excludeAll(
    ExclusionRule("org.scala-lang.modules", "scala-xml_2.13"),
    ExclusionRule("org.scala-lang.modules", "scala-parser-combinators_2.13")
  ),
  "org.scala-lang.modules" %% "scala-xml" % "2.0.0-M1",
  "com.danielasfregola" %% "twitter4s" % "6.2" excludeAll(
    ExclusionRule("com.typesafe.akka", "akka-http_2.13"),
    ExclusionRule("com.typesafe.akka", "akka-stream_2.13"),
    ExclusionRule("com.typesafe", "config")
  ),
  "com.typesafe.akka" %% "akka-http" % "10.1.10",
  "com.typesafe.akka" %% "akka-stream" % "2.6.0"
) map {
  _ withSources() withJavadoc()
}