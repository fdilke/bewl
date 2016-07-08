package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class MonadOfMonoidActionsTest extends FreeSpec {
  private val (i, x, y) = ('i, 'x, 'y)

  val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

  private val two = dot('x, 'y)

  import FiniteSets._

  private val actionsMonad = monadOfActions(monoidOf3)

  "For the monad of monoid actions" - {
    "values at a dot are cached" in {
      (actionsMonad(O) eq actionsMonad(O)) shouldBe true
    }
  }
}
