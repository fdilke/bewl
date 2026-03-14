package com.fdilke.bewl.apps.music

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{
  >,
  bifunctionAsBiArrow,
  functionAsArrow,
  LocalMonoidAssistant,
  ToposOfMonoidActions
}
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕
import TriadicFixtures._
import com.fdilke.bewl.fsets.monoid_actions.{CheckExtractPresentation, CheckLocalMorphismEnumerator}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class TriadicFixturesTest extends AnyFreeSpec {

  def enumeratesMorphisms[X, Y](
    sourceAction: triadicMonoid.Action[X],
    targetAction: triadicMonoid.Action[Y],
    thorough: Boolean
  ): Unit =
    CheckLocalMorphismEnumerator(
      triadicMonoid
    )(
      sourceAction,
      targetAction,
      thorough
    )

  val omegaAction =
    triadicTopos.unwrap(
      triadicTopos.omega
    )

  "the subobjects of the chord can be enumerated" in {
    enumeratesMorphisms(
      chordAction,
      omegaAction,
      thorough = true
    )
  }

  "the chord can be presented" in {
    CheckExtractPresentation(triadicMonoid)(chordAction)
  }
}
