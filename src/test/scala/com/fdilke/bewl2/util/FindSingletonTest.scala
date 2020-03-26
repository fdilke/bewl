package com.fdilke.bewl2.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

object MySingleton

class FindSingletonTest extends AnyFunSpec {
  describe("FindEnum") {
    it("finds the enclosing enumeration for a value type") {
      FindSingleton[MySingleton.type] shouldBe MySingleton
    }
  }
}
