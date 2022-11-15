package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.algebra.AlgebraicConstructions.*
import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.Direction.*
import com.fdilke.bewl2.utility.StockStructures.*
import com.fdilke.bewl2.utility.{Direction, RichFunSuite, StockSymbols}
import munit.Clue.generate
import munit.FunSuite

import scala.Function.tupled
import scala.collection.immutable.Set
import scala.language.postfixOps

class AlgebraicConstructionsSpec extends RichFunSuite:

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos.*
  import topos.StandardTermsAndOperators.*

  test("Construct cyclic groups") {
    withCyclicGroup(order = 17) {
      [Int17] => (_: Set[Int17]) ?=> (_: Int17 =:= Int) ?=> (_: Int =:= Int17) ?=> (group17: Group[Int17]) ?=>
        summon[Set[Int17]].size is 17
        group17.sanityTest
    }
  }

  test("Construct a monoid from a table") {
    import StockSymbols.*
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
    import StockSymbols.*
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
    import StockSymbols.*
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
    import StockSymbols.*
    implicit val _: Set[Symbol] = Set(e, a, b)
    withEndomorphismMonoid[Symbol, Unit] {
      [E] => (carrier: Set[E]) ?=> (eMonoid: EndomorphismMonoid[E, Symbol]) ?=>
      eMonoid.sanityTest
      carrier.size is 27
      val standardAction: eMonoid.Action[Symbol] = eMonoid.standardAction
      standardAction.sanityTest
    }
  }

  test("Construct the group of units") {
    withMonoid_1_0 {
      (_: Set[Int]) ?=> (_: Sets.Monoid[Int]) ?=>
        withGroupOfUnits[Int, Unit] {
          [U] => (carrier: Set[U]) ?=> (groupU: Group[U]) ?=> (embed: U => Int) =>
          groupU.sanityTest
          carrier.size is 1
          implicit val _: Monoid[U] = groupU.asMonoid
          monoids.isMorphism(embed) is true
        }
    }
    implicit val _: Set[Int] = Set(1, 2, 3)
    withEndomorphismMonoid[Int, Unit] {
      [E] => (_: Set[E]) ?=> (largerMonoid: EndomorphismMonoid[E, Int]) ?=>
        withGroupOfUnits[E, Unit] {
          [U] => (symmetries: Set[U]) ?=> (groupU: Group[U]) ?=> (embed: U => E) =>
            symmetries.size is 6
            groupU.sanityTest
            groupU.isCommutative is false
        }
    }
  }
