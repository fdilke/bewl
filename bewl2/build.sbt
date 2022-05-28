lazy val root = project
  .in(file("."))
  .settings(
    name := "bewl2",
    description := "Experimental version of topos DSL us Scala 3",
    version := "0.1.0",
    scalaVersion := "3.1.2",
    scalacOptions ++= Seq(
        // "-Yrangepos",
        "-Yindent-colons",
//        "-language:experimental.fewerBraces"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
