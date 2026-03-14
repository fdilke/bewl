package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{
  >,
  bifunctionAsBiArrow,
  functionAsArrow,
  LocalMonoidAssistant,
  ToposOfMonoidActions
}
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl.helper.StandardSymbols.{i, source, target, x, y}

import scala.Function.untupled
import scala.language.{existentials, postfixOps, reflectiveCalls}

class FiniteSetsLocalMonoidAssistantTest extends AnyFreeSpec {

  import monoidOf3.regularAction

  private val analyzer: monoidOf3.ActionAnalyzer =
    LocalMonoidAssistant.actionAnalyzer(
      monoidOf3
    )

  import analyzer.analyze

  private val actionTopos =
    ToposOfMonoidActions.of(monoidOf3, FiniteSets.DefaultMonoidAssistant)

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  "The action analyzer" - {
    "enumerates the morphisms into another action" - {
      "for the trivial action to itself" in {
        val trivialAction =
          monoidOf3.trivialAction(dot(()))

        enumeratesMorphisms(
          regularAction,
          regularAction,
          thorough = true
        )
      }

      "for the regular action to itself" in {
        enumeratesMorphisms(
          regularAction,
          regularAction,
          thorough = false // true passes, but takes too long
        )
      }

      "for regularAction x bar to bar" in {
        enumeratesMorphisms(
          regularAction.x(bar),
          regularAction,
          thorough = true
        )
      }

      "for bar x bar to bar" in {
        enumeratesMorphisms(
          bar.x(bar),
          bar,
          thorough = true
        )
      }

      "and for bar x bar to regularAction" in {
        enumeratesMorphisms(
          bar.x(bar),
          regularAction,
          thorough = true
        )
      }

      "for omega to itself" in {
        val o2 =
          actionTopos.unwrap(actionTopos.omega)

        enumeratesMorphisms(
          o2,
          o2,
          thorough = false
        )
      }

      "for omega squared to itself" ignore { // too slow
        val o2 =
          actionTopos.unwrap(
            actionTopos.omega.squared
          )

        enumeratesMorphisms(
          o2,
          o2,
          thorough = false
        )
      }

      "for one-plus-one to itself" in {
        val onePlusOne =
          monoidOf3.trivialAction(
            dot(true, false)
          )

        enumeratesMorphisms(
          onePlusOne,
          onePlusOne,
          thorough = true
        )
      }
    }

    "can calculate raw exponentials" in {
      val bazDot = dot("i", "x", "y")

      val baz = monoidOf3.action(bazDot)(scalarMultiply)
      val barAnalysis =
        analyze(bar)

      val rawExponential =
        barAnalysis.rawExponential(
          analyzer.analyze(
            baz
          )
        )
      rawExponential.exponentialAction.sanityTest
      rawExponential.evaluation.arrow should have(
        source(rawExponential.exponentialAction.actionCarrier.x(barDot)),
        target(bazDot)
      )
      monoidOf3.actions.isMorphism(
        rawExponential.exponentialAction.x(bar),
        baz,
        rawExponential.evaluation.arrow
      ) shouldBe true

      val foo = regularAction
      val foobar2baz = bifunctionAsBiArrow(
        foo.actionCarrier,
        barDot,
        bazDot
      )(
        untupled(
          Map(
            (i, "x") -> "x",
            (x, "x") -> "x",
            (y, "x") -> "y",
            (i, "y") -> "y",
            (x, "y") -> "x",
            (y, "y") -> "y"
          )
        )
      )

      val exponentialDot =
        rawExponential.exponentialAction.actionCarrier
      val foo2bar2baz = rawExponential.transpose(foo, foobar2baz)

      foo2bar2baz.sanityTest
      foo2bar2baz should have(
        source(foo.actionCarrier),
        target(exponentialDot)
      )
      foo.actionCarrier.x(barDot)(bazDot) {
        case f ⊕ b =>
          rawExponential.evaluation(
            foo2bar2baz(f),
            b
          )
      } shouldBe foobar2baz.arrow
    }
  }

  def enumeratesMorphisms[X, Y](
    sourceAction: monoidOf3.Action[X],
    targetAction: monoidOf3.Action[Y],
    thorough: Boolean
  ): Unit =
    CheckLocalMorphismEnumerator(
      monoidOf3
    )(
      sourceAction,
      targetAction,
      thorough
    )
}
