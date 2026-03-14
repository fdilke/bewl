package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Cantorian.cycle
import com.fdilke.bewl2.topology.Hausdorff
import com.fdilke.bewl2.topology.Hausdorff.equalH
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class GroundedTreeTest extends AnyFunSpec {
  describe("Grounded trees") {
    it("have an implicit Hausdorff") {
      val hausdorffTude = Hausdorff[
        GroundedTree[Int]
      ]
      hausdorffTude.equalH(
        GroundedTree(7),
        GroundedTree(7)
      ) shouldBe true
      hausdorffTude.equalH(
        GroundedTree(1),
        GroundedTree(7)
      ) shouldBe false
      hausdorffTude.equalH(
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(8)
          ),
          GroundedTree(3)
        ),
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(8)
          ),
          GroundedTree(3)
        )
      ) shouldBe true
      hausdorffTude.equalH(
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(
              GroundedTree(8),
              GroundedTree(8)
            )
          ),
          GroundedTree(3)
        ),
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(8)
          ),
          GroundedTree(3)
        )
      ) shouldBe true
      hausdorffTude.equalH(
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(
              GroundedTree(8),
              GroundedTree(9)
            )
          ),
          GroundedTree(3)
        ),
        GroundedTree(
          GroundedTree(
            GroundedTree(7),
            GroundedTree(8)
          ),
          GroundedTree(3)
        )
      ) shouldBe false
    }
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
    it("operate correctly on Cantorians - leaf case") {
      GroundedTree(7).apply(cycle(true)) shouldBe 7
    }
    it("operate correctly on Cantorians - simple branch node case") {
      val tree: GroundedTree[Int] =
        GroundedTree(
          GroundedTree(7),
          GroundedTree(1)
        )

      tree(cycle(true)) shouldBe 1
      tree(cycle(false)) shouldBe 7
      tree(cycle(false, true)) shouldBe 7
    }
    it("operate correctly on Cantorians - complex branch node case") {
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
    it("have consistent equality") {
      (GroundedTree(2) == GroundedTree(3)) shouldBe false
      equalH(GroundedTree(2), GroundedTree(3)) shouldBe false

      val tree: GroundedTree[Int] =
        GroundedTree(1)
      val tree2: GroundedTree[Int] =
        GroundedTree(tree, tree)
      (tree == tree2) shouldBe true
      equalH(tree, tree2) shouldBe true
    }
    it("can be represented sanely as strings") {
      GroundedTree(
        GroundedTree(
          GroundedTree(7),
          GroundedTree(8)
        ),
        GroundedTree(3)
      ).toString shouldBe
        "<<7, 8>, 3>"
    }
  }
}
