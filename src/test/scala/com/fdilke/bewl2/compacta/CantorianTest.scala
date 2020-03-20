package com.fdilke.bewl2.compacta

import com.fdilke.bewl2.compacta.Cantorian.cycle
import com.fdilke.bewl2.compacta.CantorianADTs.GroundedTree
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
        cycle(true, false, true)

      cantorianTFT.asIterable.take(5).toList shouldBe Seq(
        true, false, true, true, false
      )
    }
  }

//  describe("Cantorians") {
//    it("can operate on trees -leaf node case") {
//      allTrue(GroundedTree[Int](2)) shouldBe 2
//      allFalse(GroundedTree[Int](2)) shouldBe 2
//      falseTrueAlternate(GroundedTree[Int](2)) shouldBe 2
//    }
//
//    it("can operate on trees - branch node case") {
//      allTrue(tree35) shouldBe 3
//      falseTrueAlternate(tree35) shouldBe 5
//      trueFalseAlternate(tree35) shouldBe 3
//    }
//  }
//
////  describe("Analyzing co-cantorians") {
////    it("works on trivial cases") {
////      Tree.from { f => true } shouldBe Tree(true)
////    }
////  }
}
