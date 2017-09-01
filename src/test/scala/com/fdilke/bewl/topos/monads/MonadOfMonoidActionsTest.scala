package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import FiniteSets._

class MonadOfMonoidActionsTest extends FreeSpec {
  private val (i, x, y) = ('i, 'x, 'y)

  private val two = dot('x, 'y)

  private val actionsMonad = monadOfActions(monoidOf3)

  "For the monad of monoid actions" - {
    "values at a dot are cached" in {
      (actionsMonad(O) eq actionsMonad(O)) shouldBe true
    }
  }
}
