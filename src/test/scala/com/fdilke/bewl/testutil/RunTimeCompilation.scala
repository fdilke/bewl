package com.fdilke.bewl.testutil

import java.net.URLClassLoader

import org.scalatest.FunSpec
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.util.ClassPath

trait RunTimeCompilation { self: FunSpec =>
  private lazy val engine = {
    val settings = new Settings
    // add the Scala library to our classpath
    val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    val entries = loader.getURLs map(_.getPath)
    val sclpath = entries find(_.endsWith("scala-compiler.jar")) map {
      _.replaceAll("scala-compiler.jar", "scala-library.jar")
    }
    settings.classpath.value = ClassPath.join((entries ++ sclpath) : _*)
    new IMain(settings)
  }

  def inContextOf(imports: Seq[String])(executeBlock: => Unit) =
    engine.beSilentDuring {
      engine.reset
      for (importArgument <- imports)
        engine.interpret(s"import $importArgument")
      executeBlock
    }

  val compile = Matcher { code: String =>
      MatchResult(
        try {
          engine.eval(code) != null
        } catch {
          case ex: Exception =>
            false
        },
        s""""$code" did not compile""",
        s""""$code" compiled ok"""
      )
    }

  // Some other things we can do:
  //  println (engine.put("n", 10))
  //  println(engine.put("@", 5))
  //  val result = engine.eval("1 to n.asInstanceOf[Int] foreach println")
  //  println(s"result = $result")
}
