package com.fdilke.bewl.helper

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import ⊕._

class SmartPairTest extends AnyFreeSpec {

  "Smart pairs" - {
    "allow their components to be accessed" in {
      val x23: Int ⊕ String = 2 ⊕ "a"
      x23._1 shouldBe 2
      x23._2 shouldBe "a"
    }

    "have sane equality semantics" in {
      (2 ⊕ "a") shouldBe (2 ⊕ "a")
      (2 ⊕ "a") should not be (1 ⊕ "a")
      (2 ⊕ "a") should not be (2 ⊕ "b")
    }

    "can have their components extracted" in {
      val a ⊕ b = 2 ⊕ "t"
      a shouldBe 2
      b shouldBe "t"
    }

    "can be matched" in {
      2 ⊕ "t" match {
        case a ⊕ b =>
          a shouldBe 2
          b shouldBe "t"
      }
    }

    "allow tupling of functions" in {
      def bifunc(n: Int, text: String) =
        text(n)

      val tupledBifunc = ⊕.tupled(bifunc)

      tupledBifunc(2 ⊕ "felix") shouldBe 'l'
    }
  }
}
