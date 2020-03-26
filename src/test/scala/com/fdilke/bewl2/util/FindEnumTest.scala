package com.fdilke.bewl2.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

object BunnyEnum extends Enumeration {
  val bunny1, bunny2 = Value
  type Bunny = Value
}

class FindEnumTest extends AnyFunSpec {
  describe("FindEnum") {
    it("finds the enclosing enumeration for a value type") {
      FindEnum[BunnyEnum.type] shouldBe BunnyEnum
    }
  }
}
