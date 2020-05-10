package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Dyad._
import com.fdilke.bewl2.topology.Hausdorff.equalH
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Catcher._

class CatcherTest extends AnyFunSpec {
  describe("Catchers") {
    it("can be recast as a catcher of any other type and back again") {
      val tree: GroundedTree[Int] =
        GroundedTree(
          GroundedTree(1),
          GroundedTree(
            GroundedTree(2),
            GroundedTree(3)
          )
        )
      val dyad: Dyad[Int] =
        tree2dyad(tree)

      dyad shouldBe
        Dyad(1, 2, 1, 3)
      equalH(
        dyad,
        Dyad(1, 2, 1, 3)
      ) shouldBe true

      val tree2: GroundedTree[Int] =
        dyad2tree(dyad)

      tree2 shouldBe tree
      equalH(
        tree,
        tree2
      ) shouldBe true
    }
    it("can be recast, preserving equality semantics") {
      val sampleTrees: Seq[GroundedTree[Int]] = Seq(
        GroundedTree(
          GroundedTree(1),
          GroundedTree(
            GroundedTree(2),
            GroundedTree(3)
          )
        ),
        GroundedTree(1),
        GroundedTree(
          GroundedTree(1),
          GroundedTree(2)
        )
      )
      val sampleDyads: Seq[Dyad[Int]] = Seq(
        Dyad(1, 2, 1, 3),
        Dyad(1),
        Dyad(2, 1),
        Dyad(1, 2)
      )
      for {
        tree <- sampleTrees
        dyad <- sampleDyads
      } {
        equalH(
          tree,
          dyad2tree((dyad))
        ) shouldBe equalH(
          dyad,
          tree2dyad(tree)
        )
      }

      for {
        tree <- sampleTrees
      } equalH(
        dyad2tree(tree2dyad(tree)),
        tree
      ) shouldBe true

      for {
        dyad <- sampleDyads
      } equalH(
        tree2dyad(dyad2tree(dyad)),
        dyad
      ) shouldBe true
    }
  }

  private def dyad2tree(dyad: Dyad[Int]): GroundedTree[Int] =
    dyad.as[GroundedTree[Int]]

  private def tree2dyad(tree: GroundedTree[Int]): Dyad[Int] =
    tree.as[Dyad[Int]]
}
