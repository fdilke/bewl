lazy val root = project
  .in(file("."))
  .settings(
    name := "bewl2",
    description := "Experimental version of topos DSL us Scala 3",
    version := "0.1.0",
    scalaVersion := "3.2.0",
    scalacOptions ++= Seq(
        // "-Yrangepos",
        "-Yindent-colons",
//        "-language:experimental.fewerBraces"
    ),
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.0.0-M3" % Test,
        "org.typelevel" % "cats-core_3" % "2.7.0"
    )
  )

testFrameworks += new TestFramework("munit.Framework")
