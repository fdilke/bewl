package com.fdilke.bewl2.topos

import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*

import scala.Function.tupled
import scala.language.postfixOps
import com.fdilke.bewl2.utility.RichFunSuite

import scala.collection.immutable.Set
import com.fdilke.bewl2.sets.Sets
import Sets._
import com.fdilke.bewl2.utility.StockSymbols.*

class RichDotAndArrowSpec extends RichFunSuite:
  test("Can tell if an arrow is monic"):
    withDot(Set[Int](1, 2, 3)) {
      withDot(Set[Char]('A', 'B', 'C', 'D')) {
        val nonMonic: Int ~> Char = Map(
          1 -> 'B',
          2 -> 'B',
          3 -> 'C',
        )
        nonMonic.isMonic is false
        val monic: Int ~> Char = Map(
          1 -> 'B',
          2 -> 'D',
          3 -> 'C',
        )
        monic.isMonic is true
      }
    }

  test("Can tell if an arrow is epic"):
    withDot(Set[Int](1, 2, 3)) {
      withDot(Set[Char]('A', 'B', 'C', 'D')) {
        val nonEpic: Char ~> Int = Map(
          'A' -> 1,
          'B' -> 2,
          'C' -> 2,
          'D' -> 1,
        )
        nonEpic.isEpic is false
        val epic: Char ~> Int = Map(
          'A' -> 1,
          'B' -> 3,
          'C' -> 2,
          'D' -> 1,
        )
        epic.isEpic is true
      }
    }

  test("performance test for epic"):
    withDot((1 to 8).toSet[Int]) {
      π0[Int, Int].isEpic is true
    }

  test("Can tell if an arrow is iso"):
    withDot(Set[Int](1, 2, 3)) {
      withDot(Set[Char]('A', 'B', 'C', 'D')) {
        val epicNotMonic: Char ~> Int = Map(
          'A' -> 1,
          'B' -> 2,
          'C' -> 2,
          'D' -> 3,
        )
        epicNotMonic.isIso is false
        val monicNotEpic: Int ~> Char = Map(
          1 -> 'A',
          2 -> 'C',
          3 -> 'B'
        )
        monicNotEpic.isIso is false
        val iso: Int ~> Int = Map(
          1 -> 2,
          2 -> 3,
          3 -> 1
        )
        iso.isIso is true
      }
    }

  test("Singletons are interpreted correctly for sets"):
    withDot(
      Set[Int](0, 1)
    ) {
      val singInt: Int => Map[Int, Boolean] = singleton[Int]
      singInt isArrow Map[Int, Map[Int, Boolean]](
        0 -> Map[Int, Boolean](0 -> true,  1 -> false),
        1 -> Map[Int, Boolean](0 -> false, 1 -> true)
      )
    }

  test("Partial arrow classifier has correct attributes"):
    withDots(
      Set[Int](0, 1),
      Set[Symbol](a)
    ) {
      dot[OPTION[Int]].size is 3
      some[Int].isMonic is true
      none[Int].isMonic is true
      extendAlong(toUnit[Void], fromZero[Int]) isArrow (none[Int] : Unit => OPTION[Int])

      val monic: Symbol ~> Boolean = { _ => true }
      val symbolToInt: Symbol ~> Int = { _ => 0 }
      extendAlong(monic, symbolToInt) isArrow Map[Boolean, OPTION[Int]](
        true -> (some[Int] : Int => OPTION[Int])(0),
        false -> (none[Int]: Unit => OPTION[Int])(())
      )
    }

