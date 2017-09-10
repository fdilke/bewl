package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{>, ToposOfMonoidActions, FindGenerators, FindGeneratorAnalysis }
import com.fdilke.bewl.fsets.FiniteSetsUtilities.dot
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.language.{ reflectiveCalls, postfixOps }

class FindPresentationTest extends FreeSpec {

  private val (i, x, y) = ('i, 'x, 'y)

  import monoidOf3.{ Action, regularAction }

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

  private val regularAnalysis =
    findPresentation(
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
          bar x bar
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
            actionTopos.omega x
              actionTopos.makeDot(
                monoidOf3.regularAction
              )
          )
        )
      }
    }

  private def canExtractPresentation[A](
    action: monoidOf3.Action[A]
  ) {
    val generatorsWithRelators: Seq[GeneratorWithRelators[Symbol, A]] =
      findPresentation(
        action
      )

    for {
      (g, index) <- generatorsWithRelators.zipWithIndex
    } {
      g.relators should not contain Relator('i, index, 'i)
    }

    val presentedAction =
      FiniteSetsPresentedAction(
        monoidOf3
      )(
        generatorsWithRelators
      )
    presentedAction.sanityTest

    // Check this presents the original action
    val theProjection: Int > A =
      presentedAction.project(
        action,
        generatorsWithRelators map {
          _.generator
        }
      )

    monoidOf3.actions.isMorphism(
      presentedAction.action,
      action,
      theProjection
    ) shouldBe true
  }
}
