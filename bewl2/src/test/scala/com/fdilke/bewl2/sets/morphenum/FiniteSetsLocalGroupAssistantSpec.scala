package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.Sets
import Sets.{
  withDot,
  withDotMask,
  Group,
  Dot,
  GroupActionGeneratorFinder,
  GroupActionAnalyzer,
  LocalGroupAssistant
}
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import scala.Function.untupled
import scala.language.{existentials, postfixOps, reflectiveCalls}
import com.fdilke.bewl2.utility.StockStructures._
import com.fdilke.bewl2.apps.CommutingOpsSubtle.theNext

abstract class FiniteSetsLocalGroupAssistantSpec extends FunSuite:

  private val Seq(e, a, b, c, r, s) =
    Seq[String]("e", "a", "b", "c", "r", "s").map { Symbol(_) }

  with_S_3:
    (_: Dot[Symbol]) ?=> (s3: Group[Symbol]) ?=>

    s3.withRegularAction:
      (regularAction: s3.Action[Symbol]) ?=>

      val generatorFinder: GroupActionGeneratorFinder[s3.Action] =
        GroupActionGeneratorFinder.forGroup(s3)

      val actionTopos: Topos[
        s3.Action, [A] =>> A, Void, Unit, Boolean, Map
      ] =
        s3.actionTopos

      val omega: s3.Action[Boolean] =
        actionTopos.pretopos.omegaDot

      val analyzer: GroupActionAnalyzer[s3.Action] =
        LocalGroupAssistant.actionAnalyzer(s3)

      withDot(Set[String]("x", "y")):
        (barDot: Dot[String]) ?=>

        val scalarMultiply: ((String, Symbol)) => String =
          (x, g) => 
            if Set(e, r, s).contains(g) then
              x
            else if x == "x" then
              "y"
            else
              "x"

        s3.withAction(scalarMultiply):
          (bar: s3.Action[String]) ?=>

          test("Analyzer enumerates the morphisms for the trivial action on Unit to itself"):
            s3.withTrivialAction[Unit, Unit]:
              (trivialAction: s3.Action[Unit]) ?=>
              enumeratesMorphisms(
                trivialAction,
                trivialAction,
                thorough = true
              )

          test("Analyzer enumerates the morphisms for the regular action to itself"):
            enumeratesMorphisms(
              regularAction,
              regularAction,
              thorough = false // true passes, but takes too long // does it? try it!
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
            given Dot[Boolean] = omega.dot
            val o2 = omega x omega
            enumeratesMorphisms(
              o2,
              o2,
              thorough = false
            )

          test("Analyzer enumerates the morphisms for one-plus-one to itself"):
            s3.withTrivialAction[Boolean, Unit]:
              (onePlusOne: s3.Action[Boolean]) ?=>
              enumeratesMorphisms(
                onePlusOne,
                onePlusOne,
                thorough = true
              )

            test("Analyzer enumerates the morphisms for more complex examples"):
              val groupElements: Set[Symbol] = s3.dot.dot
              val subgroup: Set[Symbol] = Set(e, a)
              def rightCoset(x: Symbol) =
                groupElements.map { s3.multiply(_, x) }
              def rightCosets: Set[Set[Symbol]] =
                Set(e, r, s) map rightCoset
              println("right cosets = " + rightCosets)
              withDot(rightCosets):
                (bazDot: Dot[Set[Symbol]]) ?=>

                  val cosetScalarMultiply: ((Set[Symbol], Symbol)) => Set[Symbol] =
                    (coset, g) => coset.map { s3.multiply(_, g) }

                  s3.withAction(cosetScalarMultiply):
                    (baz: s3.Action[Set[Symbol]]) ?=>

                    enumeratesMorphisms(
                      baz x baz,
                      baz,
                      thorough = true
                    )

    def enumeratesMorphisms[X, Y](
      sourceAction: s3.Action[X],
      targetAction: s3.Action[Y],
      thorough: Boolean
    ): Unit =
      CheckLocalMorphismEnumerator(s3)(
        sourceAction,
        targetAction,
        thorough
      )

