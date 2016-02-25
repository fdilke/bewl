package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.apps.permutations.Permutations.topos.{DOT, WRAPPER, unwrap}
import com.fdilke.bewl.apps.permutations.Permutations.π
import com.fdilke.bewl.fsets.FiniteSets
import org.scalatest.FunSpec
import org.scalatest.Matchers._

import scala.language.postfixOps

class PermutationsBetterTest extends FunSpec {

  describe("Permutations") {
    it("can be defined") {
      val perm: DOT[WRAPPER[Int]] = π(1,2)(3)π

      perm.sanityTest

      val carrier = FiniteSets.makeDot(Set(1, 2, 3))

      unwrap(perm) shouldBe carrier(carrier) {
        Map(
          1 -> 2, 2 -> 1, 3 -> 3
        )
      }
    }
  }
}
