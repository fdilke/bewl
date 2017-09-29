package com.fdilke.bewl.apps.music

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{>, LocalMonoidAssistant, ToposOfMonoidActions, bifunctionAsBiArrow, functionAsArrow}
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕
import TriadicFixtures._
import com.fdilke.bewl.fsets.monoid_actions.CheckLocalMorphismEnumerator
import org.scalatest.{FreeSpec, Ignore}
import org.scalatest.Matchers._

@Ignore
class TriadicFixturesTest extends FreeSpec {

  def enumeratesMorphisms[X, Y](
    sourceAction: triadicMonoid.Action[X],
    targetAction: triadicMonoid.Action[Y],
    thorough: Boolean
  ): Unit =
    CheckLocalMorphismEnumerator(
      triadicMonoid
    ) (
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
}
