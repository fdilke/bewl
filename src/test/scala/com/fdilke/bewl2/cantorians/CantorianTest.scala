package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Cantorian.cycle
import com.fdilke.bewl2.cantorians.JonssonTarski._
import com.fdilke.bewl2.topology.Compact._
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
    it("can be sampled with take, drop, and slice") {
      trueFalseAlternate.take(7) shouldBe Seq(
        true, false, true, false, true, false, true
      )
      cycle(true, false, false).drop(5).take(3) shouldBe Seq(
        false,
        true,
        false
      )
      falseTrueAlternate.slice(3, 8) shouldBe Seq(
        true,
        false,
        true,
        false,
        true
      )
    }
    it("can be converted to iterators") {
      lazy val cantorianTF: Cantorian =
        Cantorian(
          true,
          Cantorian(
            false,
            cantorianTF
          )
        )

      cantorianTF.asIterable.take(5).toList shouldBe Seq(
        true,
        false,
        true,
        false,
        true
      )
    }

    it("can be operated on by trees -leaf node case") {
      GroundedTree[Int](2).apply(allTrue) shouldBe 2
      GroundedTree[Int](2).apply(allFalse) shouldBe 2
      GroundedTree[Int](2).apply(falseTrueAlternate) shouldBe 2
    }

    it("can be operated on by trees - simple branch node case") {
      tree35(allTrue) shouldBe 5
      tree35(trueFalseAlternate) shouldBe 5
      tree35(falseTrueAlternate) shouldBe 3
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

    it("are compact - can solve predicates when a solution exists") {
      val solvablePredicates: Seq[Cantorian => Boolean] = Seq(
        _ => true,
        _.head,
        c => c.head && c.tail.head
      )
      solvablePredicates.foreach { predicate =>
        determine[Cantorian](predicate) match {
          case None => fail("no solution found")
          case Some(cantorian) =>
            predicate(cantorian) shouldBe true
        }
      }
    }

    it("are compact - can detect when a predicate is unsolvable") {
      val unsolvablePredicates: Seq[Cantorian => Boolean] = Seq(
        _ => false,
        x => Set(0, 1, 2).map(x(_)).size == 3
      )
      unsolvablePredicates.foreach {
        determine[Cantorian](_) match {
          case Some(_) => fail("no solution found")
          case None    =>
        }
      }
    }

    it("have a Jonsson-Tarski structure that obeys the axioms") {
      val trueReally: Cantorian =
        left(trueFalseAlternate)
      trueReally.take(7) shouldBe Seq.fill(7)(true)

      val falseReally: Cantorian =
        right(trueFalseAlternate)
      falseReally.take(7) shouldBe Seq.fill(7)(false)

      join(trueFalseAlternate, falseTrueAlternate).take(8) shouldBe Seq(
        true, false, false, true, true, false, false, true
      )

      val cantorianTFT: Cantorian =
        cycle(true, false, true)

      join(left(cantorianTFT), right(cantorianTFT)).take(10) shouldBe
        cantorianTFT.take(10)

      left(join(cantorianTFT, trueFalseAlternate)).take(10) shouldBe
        cantorianTFT.take(10)
      right(join(falseTrueAlternate, cantorianTFT)).take(10) shouldBe
        cantorianTFT.take(10)
    }
  }

////  describe("Analyzing co-cantorians") {
////    it("works on trivial cases") {
////      Tree.from { f => true } shouldBe Tree(true)
////    }
////  }
}
