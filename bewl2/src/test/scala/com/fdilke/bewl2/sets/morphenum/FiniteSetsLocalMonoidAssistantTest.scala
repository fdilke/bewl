package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.FastSets
import FastSets.{
  withDot,
  withDotMask,
  Monoid,
  Dot,
  FindGeneratorAnalysis,
  GeneratorFinder,
  PresentationFinder,
  ActionAnalyzer,
  LocalMonoidAssistant
}
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import scala.Function.untupled
import scala.language.{existentials, postfixOps, reflectiveCalls}
import com.fdilke.bewl2.utility.StockStructures._

class FiniteSetsLocalMonoidAssistantTest extends FunSuite:

  private val Seq(i, x, y) =
    Seq[String]("i", "x", "y").map { Symbol(_) }

  withMonoidOf3(FastSets):
    (_: Dot[Symbol]) ?=> (monoidOf3: FastSets.Monoid[Symbol]) ?=>

    monoidOf3.withRegularAction:
      (regularAction: monoidOf3.Action[Symbol]) ?=>

      val generatorFinder: GeneratorFinder[Symbol, monoidOf3.Action] =
        GeneratorFinder.forMonoid(monoidOf3)
      val presentationFinder: PresentationFinder[Symbol, monoidOf3.Action] =
        PresentationFinder.forMonoid(monoidOf3, generatorFinder)

      val actionTopos: Topos[
        monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap
      ] =
        monoidOf3.actionTopos

      val omega: monoidOf3.Action[monoidOf3.RightIdeal] =
        actionTopos.pretopos.omegaDot

      val analyzer: ActionAnalyzer[monoidOf3.Action, monoidOf3.InternalMap] =
        LocalMonoidAssistant.actionAnalyzer(monoidOf3)

      withDot(Set[String]("x", "y")):
        (barDot: Dot[String]) ?=>

        val scalarMultiply: ((String, Symbol)) => String =
          (s, m) => monoidOf3.multiply(Symbol(s), m).name

        monoidOf3.withAction(scalarMultiply):
          (bar: monoidOf3.Action[String]) ?=>

          test("Analyzer enumerates the morphisms for the trivial action on Unit to itself"):
            monoidOf3.withTrivialAction[Unit, Unit]:
              (trivialAction: monoidOf3.Action[Unit]) ?=>
              enumeratesMorphisms(
                trivialAction,
                trivialAction,
                thorough = true
              )

          test("Analyzer enumerates the morphisms for the regular action to itself"):
            enumeratesMorphisms(
              regularAction,
              regularAction,
              thorough = false // true passes, but takes too long
            )

          test("Analyzer enumerates the morphisms for regularAction x bar to bar"):
            enumeratesMorphisms(
              regularAction x bar,
              bar,
              thorough = true
            )

          test("Analyzer enumerates the morphisms for bar x bar to bar"):
            enumeratesMorphisms(
              bar x bar,
              bar,
              thorough = true
            )

          test("Analyzer enumerates the morphisms for bar x bar to regularAction"):
            enumeratesMorphisms(
              bar x bar,
              regularAction,
              thorough = true
            )

          test("Analyzer enumerates the morphisms for omega to itself"):
            enumeratesMorphisms(
              omega,
              omega,
              thorough = false
            )

          test("Analyzer enumerates the morphisms for omega squared to itself"):
            given Dot[monoidOf3.RightIdeal] = omega.dot
            val o2 = omega x omega
            enumeratesMorphisms(
              o2,
              o2,
              thorough = false
            )

          test("Analyzer enumerates the morphisms for one-plus-one to itself"):
            monoidOf3.withTrivialAction[Boolean, Unit]:
              (onePlusOne: monoidOf3.Action[Boolean]) ?=>
              enumeratesMorphisms(
                onePlusOne,
                onePlusOne,
                thorough = true
              )

            test("Analyzer can calculate raw exponentials"):
              withDotMask(Set[String]("i", "x", "y")):
                [ROPE] => (bazDot: Dot[ROPE]) ?=> (_: ROPE =:= String) ?=> (_: String =:= ROPE) ?=>

                  val ropeScalarMultiply: ((ROPE, Symbol)) => ROPE =
                    (s, m) => monoidOf3.multiply(Symbol(s), m).name

                  monoidOf3.withAction(ropeScalarMultiply):
                    (baz: monoidOf3.Action[ROPE]) ?=>

                    val barAnalysis: analyzer.ACTION_ANALYSIS[String] =
                      analyzer.analyze(bar)

                    val rawExponential: monoidOf3.Action[monoidOf3.InternalMap[String, ROPE]] =
                      analyzer.makeExponential(
                        barAnalysis,
                        analyzer.analyze(baz)
                      )
                    rawExponential.sanityTest
                    // given actionTopos.Dot[String] = bar
                    given Dot[monoidOf3.InternalMap[String, ROPE]] = rawExponential.dot
                    given monoidOf3.actions.Algebra[(monoidOf3.InternalMap[String, ROPE], String)] =
                      rawExponential x bar
                    given monoidOf3.actions.Algebra[monoidOf3.InternalMap[String, ROPE]] =
                      rawExponential
                    actionTopos.withDots(regularAction, rawExponential, bar, baz):
                      monoidOf3.actions.isMorphism(
                        actionTopos.evaluation[String, ROPE]
                      ) is true

                      val foo = regularAction
                      val foobar2baz: ((Symbol, String)) => ROPE =
                          Map(
                            (i, "x") -> "x",
                            (x, "x") -> "x",
                            (y, "x") -> "y",
                            (i, "y") -> "y",
                            (x, "y") -> "x",
                            (y, "y") -> "y"
                          )

                      val foo2bar2baz: Symbol => monoidOf3.InternalMap[String, ROPE] = 
                        actionTopos.transpose(foobar2baz)
                      monoidOf3.actions.isMorphism(foo2bar2baz)

    def enumeratesMorphisms[X, Y](
      sourceAction: monoidOf3.Action[X],
      targetAction: monoidOf3.Action[Y],
      thorough: Boolean
    ): Unit =
      CheckLocalMorphismEnumerator(monoidOf3)(
        sourceAction,
        targetAction,
        thorough
      )

