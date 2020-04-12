package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.apps.permutations.Permutations.topos.DOT
import com.fdilke.bewl.apps.permutations.Permutations.{π, RichPermutation}
import com.fdilke.bewl.fsets.FiniteSets
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

class ParityTest extends AnyFunSpec {

  describe("Permutations expressed as maps") {
    it("can be tested on their parity") {
      Parity.of(Map()) shouldBe Parity.EVEN
      Parity.of(
        Map(
          1 -> 2,
          2 -> 1
        )
      ) shouldBe Parity.ODD
      Parity.of(
        Map(
          1 -> 2,
          2 -> 3,
          3 -> 1
        )
      ) shouldBe Parity.EVEN
      Parity.of(
        Map(
          1 -> 2,
          2 -> 1,
          3 -> 4,
          4 -> 3
        )
      ) shouldBe Parity.EVEN
      Parity.of(
        Map(
          1 -> 2,
          2 -> 3,
          3 -> 4,
          4 -> 1
        )
      ) shouldBe Parity.ODD
    }
  }
}
