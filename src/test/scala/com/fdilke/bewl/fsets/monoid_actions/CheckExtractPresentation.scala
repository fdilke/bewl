package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{>, ToposOfMonoidActions, FindGenerators, FindGeneratorAnalysis, Monoid, FindPresentation }
import com.fdilke.bewl.fsets.FiniteSetsUtilities.dot
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.language.{ reflectiveCalls, postfixOps }

object CheckExtractPresentation {
  def apply[M, A](
    monoid: Monoid[M]
  ) (
    action: monoid.Action[A]
  ) {
    val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
      FindPresentation.forMonoid(
        monoid
      )(
        action,
        FindGenerators.forMonoid(
          monoid
        )(
          action
        ).generators
      )

    for {
      (g, index) <- generatorsWithRelators.zipWithIndex
    } {
      g.relators should not contain Relator('i, index, 'i)
    }

    val presentedAction =
      FiniteSetsPresentedAction(
        monoid
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

    monoid.actions.isMorphism(
      presentedAction.action,
      action,
      theProjection
    ) shouldBe true

    theProjection shouldBe 'iso
  }
}
