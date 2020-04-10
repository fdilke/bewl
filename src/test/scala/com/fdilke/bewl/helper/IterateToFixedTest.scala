package com.fdilke.bewl.helper

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.ListBuffer

class IterateToFixedTest extends AnyFreeSpec {

  "Iterating to a fixed point" - {
    "works" in {
      val argumentsSeen = new ListBuffer[Int]
      def collatz(n: Int) = {
        argumentsSeen += n
        if (n == 1)
          1
        else if (n % 2 == 0)
          n / 2
        else
          3 * n + 1
      }
      IterateToFixed(6)(collatz) shouldBe 1
      argumentsSeen.toList shouldBe Seq(
        6, 3, 10, 5, 16, 8, 4, 2, 1
      )
    }
  }
}
