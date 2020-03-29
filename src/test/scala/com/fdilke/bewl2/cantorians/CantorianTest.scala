package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Cantorian.cycle
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CantorianTest extends AnyFunSpec {

  private val allTrue: Cantorian =
    cycle(true)

  private val allFalse: Cantorian =
    cycle(false)

  private val trueFalseAlternate: Cantorian =
    cycle(true, false)

  private val falseTrueAlternate: Cantorian =
    cycle(false, true)

  private val tree35: GroundedTree[Int] =
    GroundedTree[Int](
      GroundedTree(3),
      GroundedTree(5)
    )

  describe("Cantorians") {
    it("can be converted to iterators") {
      object cantorianTF extends Cantorian {
        override val head: Boolean = true

        override def tail: Cantorian =
          new Cantorian {
            override val head: Boolean = false

            override def tail: Cantorian = cantorianTF
          }
      }
      cantorianTF.asIterable.take(5).toList shouldBe Seq(
        true, false, true, false, true
      )
    }
  }

  describe("Cyclic Cantorians") {
    it("work as expected, and can be converted to iterators") {
      val cantorianTFT: Cantorian =
        cycle(true, false, false)

      cantorianTFT.asIterable.take(5).toList shouldBe Seq(
        true, false, false, true, false
      )
    }
  }

  describe("Cantorians") {
    it("can be operated on by trees -leaf node case") {
      GroundedTree[Int](2)(allTrue) shouldBe 2
      GroundedTree[Int](2)(allFalse) shouldBe 2
      GroundedTree[Int](2)(falseTrueAlternate) shouldBe 2
    }

    it("can be operated on by trees - simple branch node case") {
      tree35(allTrue) shouldBe 3
      tree35(trueFalseAlternate) shouldBe 3
      tree35(falseTrueAlternate) shouldBe 5
    }

    it("can be operated on by trees - complex branch node case") {
      val complexTree =
        BranchNode(
          BranchNode(
            LeafNode("x"),
            LeafNode("y")
          ),
          LeafNode("z")
        )
      complexTree(allTrue) shouldBe "x"
      complexTree(trueFalseAlternate) shouldBe "y"
      complexTree(allFalse) shouldBe "z"
      complexTree(falseTrueAlternate) shouldBe "z"
    }
  }

////  describe("Analyzing co-cantorians") {
////    it("works on trivial cases") {
////      Tree.from { f => true } shouldBe Tree(true)
////    }
////  }
}
