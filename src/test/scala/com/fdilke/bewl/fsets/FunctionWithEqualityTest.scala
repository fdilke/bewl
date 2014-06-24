package com.fdilke.bewl.fsets

import org.scalatest.{Matchers, FunSpec}
import Matchers._

// User: Felix Date: 07/06/2014 Time: 10:56

class FunctionWithEqualityTest extends FunSpec {
  describe("functions with equality") {
    it("pass through the function argument and return value") {
      val f = (x: Int) => x + x
      val domain = Set(1,2,3)
      val fE = FunctionWithEquality(domain,f)
      fE(1) shouldBe 2
      fE(2) shouldBe 4
    }
  }

  describe("the semantics of equality") {
    it("identify functions calculating the same values") {
      val f = (x: Int) => x + x
      val g = (x: Int) => 2 * x
      (f == g) shouldBe false
      val domain = Set(1,2,3)
      (FunctionWithEquality(domain,f) ==
        FunctionWithEquality(domain, g)) shouldBe true
    }

    it("identify functions according to the domain") {
      val f = (x: Int) => x * x + 1
      val g = (x: Int) => 2
      val domain = Set(1,-1)
      (FunctionWithEquality(domain,f) ==
        FunctionWithEquality(domain, g)) shouldBe true
      val biggerDomain = Set(1,-1,3)
      (FunctionWithEquality(biggerDomain,f) ==
        FunctionWithEquality(biggerDomain, g)) shouldBe false
    }
  }
}
