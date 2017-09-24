package com.fdilke.bewl.helper

import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.collection.mutable.ListBuffer

class VerifyLengthTest extends FreeSpec {

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
      lazy val N: Stream[Int] =
        0 #:: (
          N map { _ + 1 }
        )

      VerifyLength(N, 3) shouldBe false
      VerifyLength(Seq(1,2,3), 3) shouldBe true
      VerifyLength(Seq(1,2,3, 4), 3) shouldBe false
    }
  }
}
