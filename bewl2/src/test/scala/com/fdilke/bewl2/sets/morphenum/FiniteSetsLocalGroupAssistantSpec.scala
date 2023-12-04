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
import LocalGroupAssistant.findCandidates
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import scala.Function.untupled
import scala.language.{existentials, postfixOps, reflectiveCalls}
import com.fdilke.bewl2.utility.StockStructures._
import com.fdilke.bewl2.apps.CommutingOpsSubtle.theNext

class FiniteSetsLocalGroupAssistantSpec extends FunSuite:

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
        (actionOf2Dot: Dot[String]) ?=>

        val scalarMultiply: ((String, Symbol)) => String =
          (x, g) => 
            if Set(e, r, s).contains(g) then
              x
            else if x == "x" then
              "y"
            else
              "x"

        s3.withAction(scalarMultiply):
          (actionOf2: s3.Action[String]) ?=>

          test("Assistant finds candidates for action morphisms"):
            s3.withTrivialAction[Unit, Unit]:
              (trivialAction: s3.Action[Unit]) ?=>
              findCandidates(s3)((), trivialAction, actionOf2) is Set.empty
              findCandidates(s3)("x", actionOf2, trivialAction) is Set(())
              findCandidates(s3)((), trivialAction, trivialAction) is Set(())
              findCandidates(s3)("x", actionOf2, actionOf2) is Set("x", "y")
              findCandidates(s3)("x", actionOf2, actionOf2 x actionOf2) is Set(
                "x" -> "x", "y" -> "x", "x" -> "y", "y" -> "y"
              )
              findCandidates(s3)((), trivialAction, regularAction) is Set.empty
              findCandidates(s3)(a, regularAction, regularAction) is s3.dot.dot

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
              regularAction x actionOf2,
              actionOf2,
              thorough = true
            )

          test("Analyzer enumerates the morphisms for bar x bar to bar"):
            enumeratesMorphisms(
              actionOf2 x actionOf2,
              actionOf2,
              thorough = true
            )

          test("Analyzer enumerates the morphisms for bar x bar to regularAction"):
            enumeratesMorphisms(
              actionOf2 x actionOf2,
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
                (actionOf3Dot: Dot[Set[Symbol]]) ?=>

                  val cosetScalarMultiply: ((Set[Symbol], Symbol)) => Set[Symbol] =
                    (coset, g) => coset.map { s3.multiply(_, g) }

                  s3.withAction(cosetScalarMultiply):
                    (actionOf3: s3.Action[Set[Symbol]]) ?=>

                    enumeratesMorphisms(
                      actionOf3 x actionOf3,
                      actionOf3,
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

