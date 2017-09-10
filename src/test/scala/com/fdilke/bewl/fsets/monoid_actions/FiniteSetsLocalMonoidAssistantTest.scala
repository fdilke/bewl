package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{>, LocalMonoidAssistant, ToposOfMonoidActions, bifunctionAsBiArrow, functionAsArrow}
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import com.fdilke.bewl.testutil.CustomMatchers._

import scala.Function.untupled
import scala.language.{existentials, postfixOps, reflectiveCalls, higherKinds}

class FiniteSetsMonoidAssistantTest extends FreeSpec {
  
  private val (i, x, y) = ('i, 'x, 'y)

  import monoidOf3.regularAction

  val analyzerHolder: {
    type ANALYSIS[A] <: monoidOf3.ActionAnalysis[A, ANALYSIS]
    val analyzer: monoidOf3.ActionAnalyzer[ANALYSIS]
  } =
    LocalMonoidAssistant.actionAnalyzer(
      monoidOf3
    )

  private val analyzer =
    analyzerHolder.analyzer

  import analyzer.analyze

  private val regularAnalysis =
    analyze(
      regularAction
    )

  private val actionTopos = 
    ToposOfMonoidActions of(
      monoidOf3,
      FiniteSets.DefaultMonoidAssistant
    )

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  "The action analyzer" - {
    "can enumerate the morphisms into another action" - {
      "for the trivial action to itself" in {
        val trivialAction: monoidOf3.Action[actionTopos.UNIT] =
          actionTopos.unwrap(
            actionTopos.I
          )
        val morphisms =
          regularAnalysis.morphismsTo(
            analyzer.analyze(
              trivialAction
            )
          ) 
          
        morphisms should have size 1
        val morphism: Symbol > actionTopos.UNIT =
          morphisms.head
        morphism should have {
          'source(regularAction.actionCarrier)
          'target(
            trivialAction.actionCarrier
          )
        }
        monoidOf3.actions.isMorphism(
          regularAction, 
          trivialAction, 
          morphism
        ) shouldBe true
        morphism.sanityTest
      }

      "for the regular action to itself" in {
        val morphisms =
          regularAnalysis.morphismsTo(
            analyzer.analyze(
              regularAction
            )
          )

        morphisms should have size 3

        def leftMultiplication(
          l: Symbol
        ) =
          functionAsArrow[Symbol, Symbol](
            regularAction.actionCarrier,
            regularAction.actionCarrier,
            { regularAction.actionMultiply(l, _) }
          )

        morphisms.toSet shouldBe {
          elementsOf(monoidOf3.carrier).toSet map leftMultiplication
        }

        morphisms.forall {
          monoidOf3.actions.isMorphism(
            regularAction,
            regularAction,
            _
          )
        } shouldBe true
      }

      "for the regular action to itself, another way" in {
        canEnumerateMorphisms(
          regularAction,
          regularAction,
          thorough=false // true passes, but takes too long
        )
      }

      "for regularAction x bar to bar" in {
        canEnumerateMorphisms(
          regularAction x bar,
          regularAction,
          thorough=true
        )
      }

      "for bar x bar to bar" in {
        canEnumerateMorphisms(
          bar x bar,
          bar,
          thorough=true
        )
      }

      "and for bar x bar to regularAction" in {
        canEnumerateMorphisms(
          bar x bar,
          regularAction,
          thorough=true
        )
      }

      "for omega to itself" in {
        val o2 =
          actionTopos.unwrap(actionTopos.omega)

        canEnumerateMorphisms(
          o2,
          o2,
          thorough=false
        )
      }

      "for omega squared to itself" ignore { // too slow
        val o2 =
          actionTopos.unwrap(
            actionTopos.omega.squared
          )

        canEnumerateMorphisms(
          o2,
          o2,
          thorough=false
        )
      }

      "for one-plus-one to itself" in {
        val onePlusOne =
          actionTopos.unwrap(
            actionTopos.I + actionTopos.I
          )

        canEnumerateMorphisms(
          onePlusOne,
          onePlusOne,
          thorough=true
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
      rawExponential.exponentialAction.sanityTest()
      rawExponential.evaluation.arrow should have(
        'source (rawExponential.exponentialAction.actionCarrier x barDot),
        'target (bazDot)
      )
      monoidOf3.actions.isMorphism(
        rawExponential.exponentialAction x bar,
        baz,
        rawExponential.evaluation.arrow
      ) shouldBe true

      val foo = regularAction
      val foobar2baz = bifunctionAsBiArrow(
        foo.actionCarrier,
        barDot,
        bazDot
      )(untupled(Map(
        (i, "x") -> "x", (x, "x") -> "x", (y, "x") -> "y",
        (i, "y") -> "y", (x, "y") -> "x", (y, "y") -> "y"
      )))

      val exponentialDot =
        rawExponential.exponentialAction.actionCarrier
      val foo2bar2baz = rawExponential.transpose(foo, foobar2baz)

      foo2bar2baz.sanityTest()
      foo2bar2baz should have(
        'source(foo.actionCarrier),
        'target(exponentialDot)
      )
      (foo.actionCarrier x barDot)(bazDot) {
        case f ⊕ b =>
          rawExponential.evaluation(
            foo2bar2baz(f), 
            b
          )
      } shouldBe foobar2baz.arrow
    }
  }

  def canEnumerateMorphisms[X, Y](
    sourceAction: monoidOf3.Action[X],
    targetAction: monoidOf3.Action[Y],
    thorough: Boolean
  ) {

  val morphisms =
    analyze(
      sourceAction
    ).morphismsTo(
      analyzer.analyze(
        targetAction
      )
    )

    morphisms.forall {
      monoidOf3.actions.isMorphism(
        sourceAction,
        targetAction,
        _
      )
    } shouldBe true

    if (thorough) {
      morphisms should not(containDuplicates)
      morphisms.toSet map { (morphism: X > Y) =>
        actionTopos.makeArrow(
          new monoidOf3.ActionPreArrow[X, Y](
            sourceAction,
            targetAction,
            x => morphism(x)
          )
        )
      } shouldBe {
        (
          actionTopos.makeDot(sourceAction) >>
            actionTopos.makeDot(targetAction)
          ) toSet
      }
    }
  }
}
