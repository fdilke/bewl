package com.fdilke.bewl.helper

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class VerifyLengthTest extends AnyFreeSpec {

  "Verifying a sequence length" - {
    "works on empty sequences" in {
      VerifyLength(
        Seq(),
        0
      ) shouldBe true

      VerifyLength(
        Seq(),
        1
      ) shouldBe false
    }
    "takes no more than it needs" in {
      lazy val N: LazyList[Int] =
        0 #::
          N.map(_ + 1)

      VerifyLength(N, 3) shouldBe false
      VerifyLength(Seq(1, 2, 3), 3) shouldBe true
      VerifyLength(Seq(1, 2, 3, 4), 3) shouldBe false
    }
  }
}
