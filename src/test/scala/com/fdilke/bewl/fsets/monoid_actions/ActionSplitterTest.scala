package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{ dot, elementsOf, setEmptyAction }
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import monoidOf3.Action
import scala.language.{ reflectiveCalls, postfixOps }

class ActionSplitterTest extends FreeSpec {

  private val (i, x, y) = ('i, 'x, 'y)

  import monoidOf3.regularAction

  private val splitter =
    FiniteSets.ActionSplitter.forMonoid(
      monoidOf3
    )

  def components[A](
    action: Action[A]
  ): Seq[
    {
      val componentAction: monoidOf3.Action[A]
      val componentGenerators: Seq[A]
    }
  ] =
    splitter splitAction(
      action
    ) components

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  "The action splitter can extract a coproduct decomposition" - {
    "for the empty monoid action" in {
        components(
          setEmptyAction(monoidOf3)
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
  }
}
