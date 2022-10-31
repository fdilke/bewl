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

  test("We can construct cyclic groups") {
    withCyclicGroup(order = 17) {
      [Int17] => (_: Set[Int17]) ?=> (_: Int17 =:= Int) ?=> (_: Int =:= Int17) ?=> (group17: Group[Int17]) ?=>
        summon[Set[Int17]].size is 17
        group17.sanityTest
    }
  }

  test("We can construct a monoid from a table") {
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
