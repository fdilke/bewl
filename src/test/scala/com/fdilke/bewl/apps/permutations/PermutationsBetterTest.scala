package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.apps.permutations.Parity.{ODD, EVEN}
import com.fdilke.bewl.apps.permutations.Permutations.topos.{DOT, WRAPPER}
import com.fdilke.bewl.apps.permutations.Permutations.{SmartPermutation, π}
import com.fdilke.bewl.fsets.FiniteSets
import org.scalatest.FunSpec
import org.scalatest.Matchers._

import scala.language.postfixOps

class PermutationsBetterTest extends FunSpec {

  describe("Permutations") {
    it("can be defined, unwrapped, and examined as maps") {
      val perm: DOT[WRAPPER[Int]] = π(1,2)(3)π

      perm.sanityTest

      val expectedCarrier = FiniteSets.makeDot(Set(1, 2, 3))

      perm.asArrow shouldBe expectedCarrier(expectedCarrier) {
        Map(
          1 -> 2, 2 -> 1, 3 -> 3
        )
      }

      perm.asMap shouldBe Map(
        1 -> 2, 2 -> 1, 3 -> 3
      )
    }

    it("can be tested on their parity") {
      (π π).parity shouldBe EVEN
      (π(1,2)π).parity shouldBe ODD
      (π(1,2,3)π).parity shouldBe EVEN
      (π(1,2)(3,4)π).parity shouldBe EVEN
      (π(1,2,3,4)π).parity shouldBe ODD
    }
  }
}
