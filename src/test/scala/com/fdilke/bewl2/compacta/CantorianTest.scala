package com.fdilke.bewl2.compacta

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Cantorians._

class CantorianTest extends AnyFunSpec {

  private val allTrue: Cantorian =
    LazyList.continually(true)

  private val trueFalseAlternate: Cantorian = {
    lazy val loop: LazyList[Boolean] =
      true #:: false #:: loop
    loop
  }

  private val falseTrueAlternate: Cantorian = {
    lazy val loop: LazyList[Boolean] =
      false #:: true #:: loop
    loop
  }

  private val tree35: GroundedTree[Int] =
    GroundedTree[Int](
      GroundedTree(3),
      GroundedTree(5)
    )

  //  private lazy val allTrue2: LazyList[Boolean] =
//    true #:: allTrue2

  describe("Cantorians") {
    it("can operate on trees -leaf node case") {
      allTrue(GroundedTree[Int](2)) shouldBe 2
    }

    it("can operate on trees - branch node case") {
      allTrue(tree35) shouldBe 3
      falseTrueAlternate(tree35) shouldBe 5
      trueFalseAlternate(tree35) shouldBe 3
    }
  }

//  describe("Analyzing co-cantorians") {
//    it("works on trivial cases") {
//      Tree.from { f => true } shouldBe Tree(true)
//    }
//  }
}
