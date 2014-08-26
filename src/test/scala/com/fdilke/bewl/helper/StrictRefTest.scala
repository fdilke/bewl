package com.fdilke.bewl.helper

import org.scalatest.{Matchers, FunSpec}
import Matchers._

class StrictRefTest extends FunSpec {
  describe("A StrictRef") {
    it("should impose the semantics of reference equality") {
      StrictRef("a") shouldBe StrictRef("a")
      StrictRef("a") should not be StrictRef("b")
      StrictRef("a") should not be StrictRef("ab".take(1))
    }
  }
}
