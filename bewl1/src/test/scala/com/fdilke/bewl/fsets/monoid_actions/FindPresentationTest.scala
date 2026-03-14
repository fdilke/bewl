package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{
  >,
  FindGeneratorAnalysis,
  FindGenerators,
  ToposOfMonoidActions
}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.dot
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import com.fdilke.bewl.helper.StandardSymbols.{i, x, y}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.{postfixOps, reflectiveCalls}

class FindPresentationTest extends AnyFreeSpec {

  import monoidOf3.{regularAction, Action}

  private val findGenerators: {
    def apply[A](
      action: Action[A]
    ): FindGeneratorAnalysis[Symbol, A]
  } =
    FindGenerators.forMonoid(
      monoidOf3
    )

  private val findPresentation =
    FiniteSets.FindPresentation.forMonoid(
      monoidOf3
    )

  def findPresentation[A](
    action: Action[A]
  ): Seq[GeneratorWithRelators[Symbol, A]] =
    findPresentation(
      action,
      findGenerators(
        action
      ) generators
    )

  private val actionTopos =
    ToposOfMonoidActions.of(monoidOf3, FiniteSets.DefaultMonoidAssistant)

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  "The presentation finder can extract a presentation" - {
    "for the regular monoid action" in {
      canExtractPresentation(monoidOf3.regularAction)
    }
    "for an empty monoid action" in {
      canExtractPresentation(
        monoidOf3.voidAction
      )
    }
    "for a right ideal action" in {
      canExtractPresentation(
        bar
      )
    }
    "for a right ideal squared action" in {
      canExtractPresentation(
        bar.x(bar)
      )
    }
    "for the truth object monoid action" in {
      canExtractPresentation(
        actionTopos.unwrap(
          actionTopos.omega
        )
      )
    }
    "for a more fancy monoid action" in {
      canExtractPresentation(
        actionTopos.unwrap(
          actionTopos.omega.x(
            actionTopos.makeDot(
              monoidOf3.regularAction
            )
          )
        )
      )
    }
  }

  private def canExtractPresentation[A](
    action: monoidOf3.Action[A]
  ) =
    CheckExtractPresentation(monoidOf3)(action)
}
