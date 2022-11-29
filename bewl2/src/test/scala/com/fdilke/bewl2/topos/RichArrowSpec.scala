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

class RichArrowSpec extends RichFunSuite:
  test("Can tell if an arrow is monic") {
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
  }
  test("Can tell if an arrow is epic") {
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
  }

  test("Can tell if an arrow is iso") {
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
  }
