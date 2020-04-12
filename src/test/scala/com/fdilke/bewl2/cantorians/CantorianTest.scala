package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Cantorian.cycle
import com.fdilke.bewl2.topology.StrontiumDogEnumeration.StrontiumDog
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl2.topology.Compact._
import Pitcher._

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

  describe("Cyclic Cantorians") {
    it("work as expected, and can be converted to iterators") {
      val cantorianTFT: Cantorian =
        cycle(true, false, false)

      cantorianTFT.asIterable.take(5).toList shouldBe Seq(
        true,
        false,
        false,
        true,
        false
      )
    }
  }

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
        true,
        false,
        true,
        false,
        true
      )
    }

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

    it("can be treated as Int => Bool") {
      val cantorianTFT: Cantorian =
        cycle(true, false, false)
      val cantorianAsFn: Int => Boolean =
        cantorianTFT

      cantorianAsFn(0) shouldBe true
      cantorianAsFn(2) shouldBe false
      cantorianAsFn(77) shouldBe false
      cantorianAsFn(78) shouldBe true
      cantorianAsFn(2009) shouldBe false
    }

//    it("are compact - can solve predicates when a solution exists") {
//      val samplePredicates: Seq[Cantorian => Boolean] = Seq(
//        _ => true,
//        _.head,
//        c => c.head && c.tail.head
//      )
//      samplePredicates.foreach {
//        determine[Cantorian](_) match {
//          case None => fail("no solution found")
//          case Some(cantorian) =>
//            predicate(cantorian) shouldBe true
//        }
//      }
//    }
  }

////  describe("Analyzing co-cantorians") {
////    it("works on trivial cases") {
////      Tree.from { f => true } shouldBe Tree(true)
////    }
////  }
}
