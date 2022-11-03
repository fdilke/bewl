package com.fdilke.bewl2.topos

import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.Direction
import Direction.*
import com.fdilke.bewl2.algebra.AlgebraicConstructions.*
import com.fdilke.bewl2.utility.StockSymbols

import scala.Function.tupled
import scala.language.postfixOps
import com.fdilke.bewl2.utility.RichFunSuite

class AlgebraicConstructionsSpec extends RichFunSuite:

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos.StandardTermsAndOperators._
  import topos.StandardTermsAndOperators.~
  import topos.StandardTermsAndOperators.**
  import topos.StandardTermsAndOperators.***
  import topos._

  test("Construct cyclic groups") {
    withCyclicGroup(order = 17) {
      [Int17] => (_: Set[Int17]) ?=> (_: Int17 =:= Int) ?=> (_: Int =:= Int17) ?=> (group17: Group[Int17]) ?=>
        summon[Set[Int17]].size is 17
        group17.sanityTest
    }
  }

  test("Construct a monoid from a table") {
    import StockSymbols._
    val h: Symbol = e
    implicit val _: Set[Symbol] = Set(e, a)
    withMonoidFromTable(
      e, a,
      a, e
    ) {
      (_: Set[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
      monoid.sanityTest
      monoid.unit(()) is e
      monoid.multiply(a, a) is e
    }
  }

  test("Construct a group from a table") {
    import StockSymbols._
    val h: Symbol = e
    implicit val _: Set[Symbol] = Set(e, a)
    withGroupFromTable(
      e, a,
      a, e
    ) {
      (_: Set[Symbol]) ?=> (group: Group[Symbol]) ?=>
      group.sanityTest
      group.unit(()) is e
      group.multiply(a, a) is e
      group.inverse(a) is a
    }
  }

  test("Construct symmetric groups") {
    import StockSymbols._
    val h: Symbol = e
    implicit val _: Set[Symbol] = Set(e, a)
    withSymmetricGroup(1) {
      (carrier: Set[Seq[Int]]) ?=> (group: Group[Seq[Int]]) ?=>
      carrier is Set(Seq(0))
      group.sanityTest
      group.unit(()) is Seq(0)
      group.multiply(Seq(0), Seq(0)) is Seq(0)
    }
    withSymmetricGroup(2) {
      (carrier: Set[Seq[Int]]) ?=> (group: Group[Seq[Int]]) ?=>
      carrier is Set(Seq(0, 1), Seq(1, 0))
      group.sanityTest
      group.unit(()) is Seq(0, 1)
      group.multiply(Seq(1, 0), Seq(1, 0)) is Seq(0, 1)
      group.isCommutative is true
    }
    withSymmetricGroup(3) {
      (carrier: Set[Seq[Int]]) ?=> (group: Group[Seq[Int]]) ?=>
      carrier is Set(
        Seq(0, 1, 2),
        Seq(1, 0, 2), Seq(0, 2, 1), Seq(2, 1, 0),
        Seq(1, 2, 0), Seq(2, 0, 1)
      )
      group.sanityTest
      group.unit(()) is Seq(0, 1, 2)
      group.multiply(Seq(1, 0, 2), Seq(1, 2, 0)) is Seq(2, 1, 0)
      group.isCommutative is false
    }
    withSymmetricGroup(4) { // even 5 is a stretch :(
      (carrier: Set[Seq[Int]]) ?=> (group: Group[Seq[Int]]) ?=>
      carrier.size is 24
      group.sanityTest
      group.isCommutative is false
    }
  }

  test("Construct the monoid of endomorphisms") {
    import StockSymbols._
    implicit val _: Set[Symbol] = Set(e, a, b)
    Sets.withEndomorphismMonoid[Symbol, Unit] {
      [E] => (carrier: Set[E]) ?=> (eMonoid: EndomorphismMonoid[E, Symbol]) ?=>
      eMonoid.sanityTest
      carrier.size is 27
      val standardAction: eMonoid.Action[Symbol] = eMonoid.standardAction
      standardAction.sanityTest
    }
  }
