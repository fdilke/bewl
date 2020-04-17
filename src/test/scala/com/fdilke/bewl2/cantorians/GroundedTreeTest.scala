package com.fdilke.bewl2.cantorians

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Catcher._
import com.fdilke.bewl2.cantorians.Cantorian.cycle

class GroundedTreeTest extends AnyFunSpec{
  describe("Grounded trees") {
    it("have an implicit Catcher") {
      val catcherTude = Catcher[
        GroundedTree[Int],
        Boolean,
        Int
      ]

      catcherTude.construct(
        Left(7)
      ) shouldBe GroundedTree(7)

      catcherTude.construct(
        Right { bool =>
          if (bool)
            GroundedTree(3)
          else
            GroundedTree(
              GroundedTree(7),
              GroundedTree(8)
            )
        }
      ) shouldBe GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(8)
          ),
          GroundedTree(3)
        )
    }
    it("operate correctly on Cantorians") {
      val tree: GroundedTree[Int] =
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(8)
          ),
          GroundedTree(3)
        )
      tree(cycle(true)) shouldBe 3
      tree(cycle(true, false)) shouldBe 3
      tree(cycle(false)) shouldBe 7
      tree(cycle(false, true)) shouldBe 8
    }
  }
}
