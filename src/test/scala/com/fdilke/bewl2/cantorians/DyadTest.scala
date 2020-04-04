package com.fdilke.bewl2.cantorians

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

class DyadTest extends AnyFunSpec {
  describe("Dyad helper functions") {
    it("can tell if a number is a power of 2") {
      Seq(
        0, 1, 2, 3, 4, 5, 8, 17, 32
      ) map(Dyad.isPowerOf2) shouldBe Seq(
        false, true, true, false, true, false, true, false, true
      )
    }
  }
  describe("Dyads") {
    it("can't be instantiated using 'new'") {
      "new Dyad(2)" shouldNot compile
    }
    it("can be instantiated using a companion factory method") {
      Dyad(2).getClass shouldBe classOf[Dyad[_]]
    }
    it("can be instantiated only with sequences of length a power of 2") {
      Dyad(1)
      Dyad(4, 4, 4, 4)
      Dyad(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)
      intercept[IllegalArgumentException] {
        Dyad(7, 7, 7, 7, 7, 7, 7)
      }
    }
  }
}
