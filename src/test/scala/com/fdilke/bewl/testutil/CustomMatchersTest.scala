package com.fdilke.bewl.testutil

import org.scalatest.FreeSpec
import CustomMatchers.containDuplicates
import org.scalatest.Matchers._

class CustomMatchersTest extends FreeSpec {
  "The custom matcher's" - {
    "containDuplicates should perform as advertised" in {
      Seq() shouldNot containDuplicates
      Seq(1,2,1) should containDuplicates
      Seq(1,2,3) shouldNot containDuplicates
    }
  }
}
