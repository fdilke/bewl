package com.fdilke.bewl2.utility

import com.fdilke.bewl2.algebra.AlgebraicConstructions._
import StockSymbols._
import com.fdilke.bewl2.sets.Sets

object StockStructures:
  // TODO: I'm sure this could have a better name, or be constructed differently: right dominant
  def withMonoidOf3[RESULT](
    block: Set[Symbol] ?=> Sets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    withMonoidFromTable(
      e, a, b,
      a, a, b,
      b, a, b
    )(block)

  // TODO: I'm sure this could have a better name, or be constructed differently: left dominant
  def withMonoidOf3a[RESULT](
    block: Set[Symbol] ?=> Sets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    withMonoidFromTable(
      e, a, b,
      a, a, a,
      b, b, b
    )(block)
    
  def withMonoid_1_0[RESULT](
    block: Set[Int] ?=> Sets.Monoid[Int] ?=> RESULT
  ): RESULT =
    withMonoidFromTable(
      1, 0,
      0, 0
    )(block)
