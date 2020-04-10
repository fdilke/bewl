package com.fdilke.bewl.testutil

import org.scalatest.matchers.should.Matchers._
import CustomMatchers.containDuplicates
import org.scalatest.freespec.AnyFreeSpec

class CustomMatchersTest extends AnyFreeSpec {
  "The custom matcher's" - {
    "containDuplicates should perform as advertised" in {
      Seq() shouldNot containDuplicates
      Seq(1, 2, 1) should containDuplicates
      Seq(1, 2, 3) shouldNot containDuplicates
    }
  }
}
