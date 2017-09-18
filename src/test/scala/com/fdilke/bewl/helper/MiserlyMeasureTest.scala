package com.fdilke.bewl.helper

import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import com.fdilke.bewl.helper.{ MiserlyMeasure => MM }
import scala.language.{existentials, reflectiveCalls}

class MiserlyMeasureTest extends FreeSpec {
  "The miserly measure" - {
    "works on empty sequences" in {
      MM(Traversable.empty, 0) shouldBe true
      MM(Traversable.empty, 1) shouldBe false
    }
    "takes no more than it needs" in {
      lazy val N: Stream[Int] =
        0 #:: (
          N map { _ + 1 }
        )

      MM(N, 3) shouldBe false
      MM(Seq(1,2,3), 3) shouldBe true
      MM(Seq(1,2,3, 4), 3) shouldBe false
    }
  }
}
