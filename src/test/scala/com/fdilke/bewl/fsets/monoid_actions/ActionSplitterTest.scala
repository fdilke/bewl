package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.ActionComponent
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{dot, elementsOf}
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3.Action
import org.scalatest.matchers.should.Matchers._

import org.scalatest.freespec.AnyFreeSpec
import com.fdilke.bewl.helper.StandardSymbols.{i, x, y}
import scala.language.{postfixOps, reflectiveCalls}

class ActionSplitterTest extends AnyFreeSpec {
  import monoidOf3.regularAction

  private val splitter: FiniteSets.ActionSplitter[
    Symbol,
    ({ type λ[T] = monoidOf3.Action[T] })#λ
  ] =
    FiniteSets.ActionSplitter.forMonoid(
      monoidOf3
    )

  def components[A](
    action: Action[A]
  ): Seq[
    ActionComponent[
      Symbol,
      A,
      ({ type λ[T] = monoidOf3.Action[T] })#λ
    ]
  ] =
    splitter
      .splitAction(
        action
      ) components

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  "The action splitter can extract a coproduct decomposition" - {
    "for the empty monoid action" in {
      components(
        monoidOf3.voidAction
      ) shouldBe empty
    }

    "for the regular monoid action" in {
      val regularSplitting =
        components(
          regularAction
        )

      regularSplitting should have size 1
      elementsOf(
        regularSplitting.head.componentAction.actionCarrier
      ) should have size 3
    }

    "for regularAction x bar" in {
      val regbarSplitting =
        components(
          regularAction.x(bar)
        )

      regbarSplitting should have size 1

      elementsOf(
        regbarSplitting.head.componentAction.actionCarrier
      ).size shouldBe 12
    }
  }
}
