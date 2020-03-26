package com.fdilke.bewl2.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

object MySingleton
class MyNonSingleton

class FindSingletonTest extends AnyFunSpec {
  describe("FindEnum") {
    it("finds a singleton object given its type") {
      FindSingleton[MySingleton.type] shouldBe MySingleton
    }
    it("correctly diagnoses non-singletons") {
      intercept[IllegalArgumentException] {
        FindSingleton[MyNonSingleton]
      }.getMessage shouldBe
        "class MyNonSingleton is not a singleton"
    }
  }
}
