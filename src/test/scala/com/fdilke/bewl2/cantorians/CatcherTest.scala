package com.fdilke.bewl2.cantorians

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Dyad._

class CatcherTest extends AnyFunSpec {
  describe("Catchers") {
    it("can be recast as a catcher of any other type, preserving equality") {
      val tree: GroundedTree[Int] =
        GroundedTree(
          GroundedTree(1),
          GroundedTree(
            GroundedTree(2),
            GroundedTree(3)
          )
        )
      val dyad: Dyad[Int] =
        Catcher.recast[GroundedTree[Int], Dyad[Int], Boolean, Int](tree)

      dyad shouldBe
        Dyad(1, 2, 1, 3)

      Catcher.recast[Dyad[Int], GroundedTree[Int], Boolean, Int](dyad) shouldBe tree
    }
  }
}
