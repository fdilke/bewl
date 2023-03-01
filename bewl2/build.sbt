lazy val root = project
  .in(file("."))
  .settings(
    name := "bewl2",
    description := "Experimental version of topos DSL us Scala 3",
    version := "0.1.0",
    scalaVersion := "3.3.0-RC3", // "3.2.2",
    scalacOptions ++= Seq(
        // "-Yrangepos",
        "-Yindent-colons",
        "-explain",
//        "-language:experimental.fewerBraces"
    ),
    ThisBuild/scalacOptions ++= Seq("-unchecked", "-deprecation" /*, "-rangepos" */),
    // Test / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8000",
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.0.0-M3" % Test,
        "org.typelevel" % "cats-core_3" % "2.7.0"
    )
  )

testFrameworks += new TestFramework("munit.Framework")
