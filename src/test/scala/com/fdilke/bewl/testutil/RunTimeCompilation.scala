package com.fdilke.bewl.testutil

import java.io.File
import java.net.URLClassLoader

import com.fdilke.bewl.topos.Topos
import org.scalatest.FunSpec
import org.scalatest.matchers.{Matcher, MatchResult}

import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.{Settings, Interpreter}
import scala.tools.nsc.interpreter.IMain

trait RunTimeCompilation { self: FunSpec =>
  private lazy val engine = {
    val settings = new Settings

    val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    val entries = loader.getURLs map(_.getPath)
    // annoyingly, the Scala library is not in our classpath, so we have to add it manually
    val sclpath = entries find(_.endsWith("scala-compiler.jar")) map(
      _.replaceAll("scala-compiler.jar", "scala-library.jar"))
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
