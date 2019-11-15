package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import FiniteSets._
import org.scalatest.freespec.AnyFreeSpec
// import com.fdilke.bewl.helper.StandardSymbols.{i, x, y}

class MonadOfMonoidActionsTest extends AnyFreeSpec {
//  private val two = dot(x, y)
// TODO: add more tests

  private val actionsMonad = monadOfActions(monoidOf3)

  "For the monad of monoid actions" - {
    "values at a dot are cached" in {
      (actionsMonad(O) eq actionsMonad(O)) shouldBe true
    }
  }
}
