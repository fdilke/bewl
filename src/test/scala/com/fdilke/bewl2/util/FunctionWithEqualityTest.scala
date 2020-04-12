package com.fdilke.bewl2.util

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl2.fsets.FiniteSets._

class FunctionWithEqualityTest extends AnyFreeSpec {
  implicit private val domain: Iterable[Int] = 0 to 10
  implicit private val range: Iterable[Boolean] = Iterable(true, false)

  "a function with equality" - {
    "has the same effect on values" in {
      val even: Int => Boolean = { n => (n % 2) == 0 }
      val wrappedEven = new FunctionWithEquality(even)

      wrappedEven(2) shouldBe true
      wrappedEven(17) shouldBe false
    }

    "has the right semantics of equality" in {
      val even: Int => Boolean = { n => (n % 2) == 0 }
      val altEven: Int => Boolean = { n => n == 2 * (n / 2) }
      val odd: Int => Boolean = { n => (n % 2) != 0 }

      val wrappedEven = new FunctionWithEquality(even)
      val wrappedAltEven = new FunctionWithEquality(altEven)
      val wrappedOdd = new FunctionWithEquality(odd)

      wrappedEven shouldBe wrappedEven
      wrappedEven shouldBe wrappedAltEven
      wrappedEven should not be (wrappedOdd)
    }

    "can be compared to objects of a different type" in {
      val even: Int => Boolean = { n => (n % 2) == 0 }
      val wrappedEven = new FunctionWithEquality(even)

      wrappedEven should not be (38)
    }
  }
}
