package com.fdilke.bewl2.compacta

import com.fdilke.bewl2.compacta.CantorianADTs.GroundedTree
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CantorianTest extends AnyFunSpec {

  if (false) {
//    println("VVV 01")
//
//    private val allTrue: Cantorian =
//    Cantorian.cycle(true)
//
//    println("VVV 02")
//
//    private val allFalse: Cantorian =
//    Cantorian.cycle(false)
//
//    println("VVV 03")
//
//    private val trueFalseAlternate: Cantorian =
//    Cantorian.cycle(true, false)
//    //    lazy val loop: LazyList[Boolean] =
//    //      true #:: false #:: loop
//    //    loop
//    //  }
//
//    println("VVV 04")
//
//    private val falseTrueAlternate: Cantorian =
//    Cantorian.cycle(false, true)
//    //    lazy val loop: LazyList[Boolean] =
//    //      false #:: true #:: loop
//    //    loop
//    //  }
  }

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
    ignore("work as expected, and can be converted to iterators") {
      println("VVV 1")

      val cantorianTFT: Cantorian =
        Cantorian.cycle(true, false) // , true)
      println("VVV 2")
      val a = cantorianTFT.asIterable
      println("VVV 3")
      val b = a.take(6)
      println("VVV 4")

      val c = b.toList

      cantorianTFT.asIterable.take(6).toList shouldBe Seq(
        true, false, true, false, true
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
