package com.fdilke.bewl2.utility

import com.fdilke.bewl2.algebra.AlgebraicConstructions._
import StockSymbols._
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.sets.BaseSets

object StockStructures:
  // right dominant
  def withMonoidOf3[RESULT](
    baseSets: BaseSets
  )(
    block: baseSets.Dot[Symbol] ?=> baseSets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    baseSets.withMonoidFromTable(
      e, a, b,
      a, a, b,
      b, a, b
    )(block)

  def withMonoidOf3[RESULT](
    block: Sets.Dot[Symbol] ?=> Sets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    withMonoidOf3(Sets)(block)

  // left dominant
  def withMonoidOf3a[RESULT](
    block: Sets.Dot[Symbol] ?=> Sets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    Sets.withMonoidFromTable(
      e, a, b,
      a, a, a,
      b, b, b
    )(block)
    
  def withMonoid_1_0[RESULT](
    block: Sets.Dot[Int] ?=> Sets.Monoid[Int] ?=> RESULT
  ): RESULT =
    Sets.withMonoidFromTable(
      1, 0,
      0, 0
    )(block)
    
  def with_S_3[RESULT](
    block: Sets.Dot[Symbol] ?=> Sets.Group[Symbol] ?=> RESULT
  ): RESULT =
    withGroupFromTable(
      e, a, b, c, r, s,
      a, e, s, r, c, b,
      b, r, e, s, a, c,
      c, s, r, e, b, a,
      r, b, c, a, s, e,
      s, c, a, b, e, r
    )(block)
