package com.fdilke.bewl.diagrammatic.algebra

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.DiagrammaticFiniteSets._
import org.scalatest.{Matchers, FunSpec}
import Matchers.{all => _, _}

class QuantifierTests extends FunSpec {
  describe("The 'all' quantifier") {
    it("is calculated correctly for sets, at least") {
      val foo = set('o, 'x)
      val trueEverywhere = arrow(foo, omega, 'o -> true, 'x -> true)
      val fooAll = all(foo)
      fooAll.source shouldBe omega ^ foo
      fooAll.target shouldBe omega
      fooAll.source.map { f =>
        (f('o), f('x), fooAll.function(f))
      }.toSet shouldBe Set(
        (true,  true,  true),
        (false, true,  false),
        (true,  false, false),
        (false, false, false)
      )
    }
  }

  describe("The 'exists' quantifier") {
    it("is calculated correctly for sets, at least") {
      val foo = set('o, 'x)
      val trueEverywhere = arrow(foo, omega, 'o -> true, 'x -> true)
      val fooExists = exists(foo)
      fooExists.source shouldBe omega ^ foo
      fooExists.target shouldBe omega
      fooExists.source.map { f =>
        (f('o), f('x), fooExists.function(f))
      }.toSet shouldBe Set(
        (true,  true,  true),
        (false, true,  true),
        (true,  false, true),
        (false, false, false)
      )
    }
  }
}
