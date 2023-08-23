package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Cryptomorph.Cryptomorph
import com.fdilke.bewl2.topology.Hausdorff.equalH
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Catcher._

class CryptomorphTest extends AnyFunSpec {
  describe("The co-cantorian cryptomorph") {
    it("can be constructed from a co-cantorian and regarded as a dyad") {
      val coC: Cantorian => Int =
        c => Set(0, 1, 2).map(c).size

      val cryp: Cryptomorph[Int] =
        Cryptomorph(coC)

      cryp.as[Dyad[Int]] shouldBe
        Dyad(1, 2, 2, 2, 2, 2, 2, 1)

      equalH(
        cryp.function,
        coC
      ) shouldBe true
    }
    it("can be constructed from a dyad and regarded as a co-cantorian") {
      val dyad: Dyad[Int] =
        Dyad(3, 2)

      val cryp: Cryptomorph[Int] =
        Cryptomorph(dyad)

      equalH[Cantorian => Int](
        cryp.function,
        c => if (c.head) 2 else 3
      ) shouldBe true

      cryp.as[Dyad[Int]] shouldBe dyad
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
        cryp.function,
        c => if (c.head) (if (c.tail.head) 3 else 2) else 1
      ) shouldBe true

      cryp.as[Dyad[Int]] shouldBe Dyad(
        1,
        2,
        1,
        3
      )
    }

    it("can be recast as any other catcher-of-boolean type") {
      Cryptomorph
        .apply[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        )
        .as[GroundedTree[Int]] shouldBe
        GroundedTree[Int](
          GroundedTree(
            GroundedTree(6),
            GroundedTree(0)
          ),
          GroundedTree(
            GroundedTree(5),
            GroundedTree(2)
          )
        )
    }

    it("is implicitly Hausdorff") {
      equalH[Cryptomorph[Int]](
        Cryptomorph[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        ),
        Cryptomorph[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        )
      ) shouldBe true
      equalH[Cryptomorph[Int]](
        Cryptomorph[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        ),
        Cryptomorph[Dyad[Int], Int](
          Dyad(1, 7)
        )
      ) shouldBe false
    }

    it("can be applied to Cantorians") {
      val cryp = Cryptomorph(6, 5, 0, 2)
      val cantorianTFT = Cantorian.cycle(true, false, true)
      cryp.function(cantorianTFT) shouldBe 5
      cryp(cantorianTFT) shouldBe 5
    }

    it("has sane semantics of equality") {
      val cryp: Cryptomorph[Int] =
        Cryptomorph[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        )
      val cryp2: Cryptomorph[Int] =
        Cryptomorph[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        )
      val cryp3: Cryptomorph[Int] =
        Cryptomorph[Dyad[Int], Int](
          Dyad(8, 8, 0, 8, 9, 9, 0, 9)
        )
      cryp shouldBe cryp
      cryp shouldBe cryp2
      cryp shouldNot be(cryp3)
    }
    it("can be instantiated from an argument list like the equivalent Dyad") {
      Cryptomorph[Int](
        6,
        5,
        0,
        2
      ) shouldBe
        Cryptomorph[Dyad[Int], Int](
          Dyad(6, 5, 0, 2)
        )
    }
  }
}
