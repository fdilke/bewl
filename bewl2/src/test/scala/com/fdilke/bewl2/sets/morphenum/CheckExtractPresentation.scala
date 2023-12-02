package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.Sets
import Sets.{ Dot, Monoid, ActionComponent, ActionSplitter, MonoidActionGeneratorFinder, PresentationFinder, RichArrow }

import com.fdilke.bewl2.utility.StockStructures._
import scala.language.{postfixOps, reflectiveCalls}

import com.fdilke.bewl2.utility.RichFunSuite._

import scala.language.{postfixOps, reflectiveCalls}

object CheckExtractPresentation:
  def apply[M, A](
    monoid: Monoid[M]
  )(
    action: monoid.Action[A]
  ): Unit =
    val generatorFinder: MonoidActionGeneratorFinder[monoid.Action] =
      MonoidActionGeneratorFinder.forMonoid(monoid)
    val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
      PresentationFinder.forMonoid(
        monoid,
        generatorFinder
      )(
        action,
        generatorFinder(action)
      )

    val i: M = monoid.unit(())
    for
      (g, index) <- generatorsWithRelators.zipWithIndex
    do
      g.relators.contains(Relator(i, index, i)) is false

    val presentedAction: PresentedMonoidAction[Int, monoid.Action] =
      FiniteSetsPresentedMonoidAction(monoid)(generatorsWithRelators)
    presentedAction.sanityTest

    // Check this presents the original action
    val theProjection: Int => A =
      presentedAction.project(
        action,
        generatorsWithRelators.map { _.generator }
      )

    given monoid.Action[Int] = presentedAction.action
    given Dot[Int] = presentedAction.action.dot
    given monoid.Action[A] = action
    given Dot[A] = action.dot
    monoid.actions.isMorphism(
      theProjection
    ) is true

    theProjection.isIso is true
