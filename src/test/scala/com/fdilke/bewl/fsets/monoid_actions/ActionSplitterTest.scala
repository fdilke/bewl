package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{ dot, elementsOf }
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.language.reflectiveCalls

class ActionSplitterTest extends FreeSpec {

  private val (i, x, y) = ('i, 'x, 'y)

  import monoidOf3.regularAction

  private val splitter =
    FiniteSets.ActionSplitter.forMonoid(
      monoidOf3
    )

  import splitter.splitAction

  val regularAnalysis =
    splitAction(
      regularAction
    )

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name

  private val barDot: FiniteSets.DOT[String] = dot("x", "y")

  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  "The action splitter can extract a coproduct decomposition" - {
    "for the regular monoid action" in {
      val regularSplitting =
        splitAction(regularAction)

      regularSplitting should have size 1
      elementsOf(regularSplitting.head.actionCarrier)
    }
    // TODO add for empty - which should be in AlgebraicConstructions
    // and check the 0 object in topos-of-actions is this
  }
}
