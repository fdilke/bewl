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
