package com.fdilke.bewl2.cantorians

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CryptomorphTest extends AnyFunSpec {
  describe("The co-cantorian cryptomorph") {
    it("can be constructed from a co-cantorian and regarded as a dyad") {
      val coC: Cantorian => Int =
        c =>
          Set(0, 1, 2).map { c }.size

      Cryptomorph(coC).asDyad shouldBe
        Dyad(1, 2, 2, 2, 2, 2, 2, 1)
    }
  }
}
