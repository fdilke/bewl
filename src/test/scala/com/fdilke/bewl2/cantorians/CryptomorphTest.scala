package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Hausdorff
import com.fdilke.bewl2.topology.Hausdorff.equalH
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CryptomorphTest extends AnyFunSpec {
  describe("The co-cantorian cryptomorph") {
    it("can be constructed from a co-cantorian and regarded as a dyad") {
      val coC: Cantorian => Int =
        c => Set(0, 1, 2).map(c).size

      val cryp: Cryptomorph[Int] =
        Cryptomorph(coC)

      cryp.dyad shouldBe
        Dyad(1, 2, 2, 2, 2, 2, 2, 1)

      equalH(
        cryp.coCantorian,
        coC
      ) shouldBe true
    }
    it("can be constructed from a dyad and regarded as a co-cantorian") {
      val dyad: Dyad[Int] =
        Dyad(3, 2)

      val cryp: Cryptomorph[Int] =
        Cryptomorph(dyad)

      equalH[Cantorian => Int](
        cryp.coCantorian,
        c => if (c.head) 2 else 3
      ) shouldBe true

      cryp.dyad shouldBe dyad
    }
    it("can be constructed from another catcher-of-boolean type and regarded as a dyad") {
      val tree: GroundedTree[Int] =
        GroundedTree(
          GroundedTree(1),
          GroundedTree(
            GroundedTree(2),
            GroundedTree(3)
          )
        )

      val cryp: Cryptomorph[Int] =
        Cryptomorph(tree)

      equalH[Cantorian => Int](
        cryp.coCantorian,
        c => if (c.head) (if (c.tail.head) 3 else 2) else 1
      ) shouldBe true

      cryp.dyad shouldBe Dyad(
        1,
        2,
        1,
        3
      )
    }
  }
}
