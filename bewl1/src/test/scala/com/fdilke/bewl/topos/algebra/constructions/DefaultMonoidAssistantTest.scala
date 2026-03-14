package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.bifunctionAsBiArrow
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.Function.untupled
import scala.language.{existentials, reflectiveCalls}
import com.fdilke.bewl.helper.StandardSymbols.{i, x, y, source, target}

class DefaultMonoidAssistantTest extends AnyFreeSpec {

  import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
  import monoidOf3.regularAction

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  private val analyzer: monoidOf3.ActionAnalyzer =
    FiniteSets.DefaultMonoidAssistant.actionAnalyzer(
      monoidOf3
    )

  private val regularAnalysis =
    analyzer.analyze(regularAction)

  "The default monoid assistant" - {
    "can enumerate morphisms" in {
      regularAnalysis
        .morphismsTo(
          analyzer.analyze(
            bar
          )
        )
        .toSet shouldBe {
        elementsOf(barDot).toSet map { (a: String) =>
          regularAction.actionCarrier(barDot) { m => scalarMultiply(a, m) }
        }
      }
    }

    "can calculate raw exponentials" in {
      val bazDot = dot("i", "x", "y")
      val baz = monoidOf3.action(bazDot)(scalarMultiply)
      val barAnalysis = analyzer.analyze(bar)

      val rawExponential =
        barAnalysis.rawExponential(
          analyzer.analyze(
            baz
          )
        )
      rawExponential.exponentialAction.sanityTest
      rawExponential.evaluation.arrow should have(
        source(rawExponential.exponentialAction.actionCarrier x barDot),
        target(bazDot)
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
      (foo.actionCarrier x barDot)(bazDot) {
        case f ⊕ b =>
          rawExponential.evaluation(
            foo2bar2baz(f),
            b
          )
      } shouldBe foobar2baz.arrow
    }
  }
}
