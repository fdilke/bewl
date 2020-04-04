package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Dyad.{canonical, isPowerOf2}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

class DyadTest extends AnyFunSpec {
  describe("Dyad helper functions") {
    it("can tell if a number is a power of 2") {
      Seq(
        0, 1, 2, 3, 4, 5, 8, 17, 32
      ) map isPowerOf2 shouldBe Seq(
        false, true, true, false, true, false, true, false, true
      )
    }
    it("can get a dyadic sequence into canonical form") {
      canonical("x") shouldBe Seq("x")
      canonical(true, true) shouldBe Seq(true)
      canonical(1.0, 2.0) shouldBe Seq(1.0, 2.0)
      canonical(1.0, 2.0, 1.0, 2.0) shouldBe Seq(1.0, 2.0)
      canonical(1.0, 2.0, 3.0, 4.0) shouldBe Seq(1.0, 2.0, 3.0, 4.0)
      canonical(4.0, 4.0, 4.0, 4.0) shouldBe Seq(4.0)
      canonical(10, 15, 10, 15, 10, 15, 10, 15) shouldBe Seq(10, 15)
      canonical(6, 5, 0, 2, 6, 5, 0, 2) shouldBe Seq(6, 5, 0, 2)
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
