package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{
  >,
  FindGeneratorAnalysis,
  FindGenerators,
  FindPresentation,
  Monoid,
  ToposOfMonoidActions
}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.dot
import com.fdilke.bewl.helper.StandardSymbols
import com.fdilke.bewl.helper.StandardSymbols.{i, iso}
import org.scalatest.matchers.should.Matchers._

import scala.language.{postfixOps, reflectiveCalls}

object CheckExtractPresentation {
  def apply[M, A](
    monoid: Monoid[M]
  )(
    action: monoid.Action[A]
  ): Unit = {
    val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
      FindPresentation.forMonoid(
        monoid
      )(
        action,
        FindGenerators
          .forMonoid(
            monoid
          )(
            action
          )
          .generators
      )

    for {
      (g, index) <- generatorsWithRelators.zipWithIndex
    } {
      g.relators should not contain Relator(i, index, i)
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
        generatorsWithRelators.map {
          _.generator
        }
      )

    monoid.actions.isMorphism(
      presentedAction.action,
      action,
      theProjection
    ) shouldBe true

    theProjection shouldBe iso
  }
}
