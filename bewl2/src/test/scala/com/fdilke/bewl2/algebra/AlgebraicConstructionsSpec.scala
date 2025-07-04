package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.algebra.AlgebraicConstructions.*
import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.StockStructures.*
import com.fdilke.bewl2.utility.StockSymbols
import munit.Clue.generate
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._

import scala.Function.tupled
import scala.collection.immutable.Set
import scala.language.postfixOps
import com.fdilke.bewl2.sets.Sets
import Sets.*
import Sets.StandardTermsAndOperators.*

class AlgebraicConstructionsSpec extends FunSuite:
  
  test("Construct cyclic groups"):
    withCyclicGroup(order = 17) {
      [Int17] => (_: Dot[Int17]) ?=> (_: Int17 =:= Int) ?=> (_: Int =:= Int17) ?=> (group17: Group[Int17]) ?=>
        dot[Int17].size is 17
        group17.sanityTest
      }
      
  test("Construct a monoid from a table"):
    import StockSymbols.*
    val h: Symbol = e
    given Set[Symbol] = Set(e, a)
    withMonoidFromTable(
      e, a,
      a, e
    ):
      (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
      monoid.sanityTest
      monoid.unit(()) is e
      monoid.multiply(a, a) is e

  test("Construct a group from a table"):
    import StockSymbols.*
    Sets.withGroupFromTable(
      e, a,
      a, e
    ):
      (_: Dot[Symbol]) ?=> (group: Group[Symbol]) ?=>
        group.sanityTest
        group.unit(()) is e
        group.multiply(a, a) is e
        group.inverse(a) is a

  test("Construct symmetric groups"):
    import StockSymbols.*

    withSymmetricGroup(1) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      dot[Seq[Int]] is Set(Seq(0))
      group.sanityTest
      group.unit(()) is Seq(0)
      group.multiply(Seq(0), Seq(0)) is Seq(0)
      action.sanityTest
      action.actionMultiply(0, Seq(0)) is 0
    }
    withSymmetricGroup(2) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      dot[Seq[Int]] is Set(Seq(0, 1), Seq(1, 0))
      group.sanityTest
      group.unit(()) is Seq(0, 1)
      group.multiply(Seq(1, 0), Seq(1, 0)) is Seq(0, 1)
      dot[Int] is Set(0, 1)
      action.sanityTest
      action.actionMultiply(1, Seq(1, 0)) is 0
      group.isCommutative is true
    }
    withSymmetricGroup(3) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      dot[Seq[Int]] is Set(
        Seq(0, 1, 2),
        Seq(1, 0, 2), Seq(0, 2, 1), Seq(2, 1, 0),
        Seq(1, 2, 0), Seq(2, 0, 1)
      )
      group.sanityTest
      group.unit(()) is Seq(0, 1, 2)
      group.multiply(Seq(1, 0, 2), Seq(1, 2, 0)) is Seq(2, 1, 0)
      dot[Int] is Set(0, 1, 2)
      action.sanityTest
      action.actionMultiply(0, Seq(1, 0, 2)) is 1
      group.isCommutative is false
    }
    withSymmetricGroup(4) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      dot[Seq[Int]].size is 24
      group.sanityTest
      dot[Int] is Set(0, 1, 2, 3)
      action.sanityTest
      action.actionMultiply(2, Seq(1, 0, 3, 2)) is 3
      group.isCommutative is false
    }

  if (false)
    test("(Benchmark, sanity test S_5)"):
      withSymmetricGroup(5) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
        dot[Seq[Int]].size is 120
        group.sanityTest
        action.sanityTest
        action.actionMultiply(2, Seq(1, 0, 4, 3, 2)) is 4
        group.isCommutative is false
      }

  test("Construct the monoid of endomorphisms"):
    import StockSymbols.*
    withDot(Set[Symbol](e, a, b)):
      withEndomorphismMonoid[Symbol, Unit]:
        [E] => (_: Dot[E]) ?=> (monoid: Monoid[E]) ?=> (standardAction: monoid.Action[Symbol]) =>
        monoid.sanityTest
        dot[E].size is 27
        standardAction.sanityTest

  test("Construct the group of units"):
    withMonoid_1_0 { (_: Dot[Int]) ?=> (_: Monoid[Int]) ?=>
      withGroupOfUnits[Int, Unit] {
        [U] => (_: Dot[U]) ?=> (groupU: Group[U]) ?=> (embed: U => Int) =>
        groupU.sanityTest
        dot[U].size is 1
        groupU.withMonoid {
          monoids.isMorphism[U, Int](embed) is true
        }
      }
    }
    withDot(Set[Int](1, 2, 3)):
      withEndomorphismMonoid[Int, Unit] {
        [E] => (_: Dot[E]) ?=> (monoid: Monoid[E]) ?=> (standardAction: monoid.Action[Int]) =>
          withGroupOfUnits[E, Unit] {
            [U] => (_: Dot[U]) ?=> (groupU: Group[U]) ?=> (embed: U => E) =>
              dot[U].size is 6
              groupU.sanityTest
              groupU.isCommutative is false
          }
      }

  test("Construct the internal automorphism group"):
    import StockSymbols.*
    withDot(Set[Symbol](e, a, b, c)):
      withAutomorphismGroup[Symbol, Unit] {
        [G] => (_: Dot[G]) ?=> (group: Group[G]) ?=> (action: group.Action[Symbol]) =>
        group.sanityTest
        action.sanityTest
        dot[G].size is 24
      }
