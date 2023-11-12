package com.fdilke.bewl2.utility

import com.fdilke.bewl2.algebra.AlgebraicConstructions._
import StockSymbols._
import com.fdilke.bewl2.sets.FastSets
import com.fdilke.bewl2.sets.BaseSets

object StockStructures:
  // right dominant
  def withMonoidOf3[RESULT](
    baseSets: BaseSets
  )(
    block: baseSets.Dot[Symbol] ?=> baseSets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    baseSets.withMonoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    )(block)

  def withMonoidOf3[RESULT](
    block: FastSets.Dot[Symbol] ?=> FastSets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    withMonoidOf3(FastSets)(block)

  // left dominant
  def withMonoidOf3a[RESULT](
    block: FastSets.Dot[Symbol] ?=> FastSets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    FastSets.withMonoidFromTable(
      e, a, b,
      a, a, a,
      b, b, b
    )(block)
    
  def withMonoid_1_0[RESULT](
    block: FastSets.Dot[Int] ?=> FastSets.Monoid[Int] ?=> RESULT
  ): RESULT =
    FastSets.withMonoidFromTable(
      1, 0,
      0, 0
    )(block)
    
  def with_S_3[RESULT](
    block: FastSets.Dot[Symbol] ?=> FastSets.Group[Symbol] ?=> RESULT
  ): RESULT =
    withGroupFromTable(
      e, a, b, c, r, s,
      a, e, s, r, c, b,
      b, r, e, s, a, c,
      c, s, r, e, b, a,
      r, b, c, a, s, e,
      s, c, a, b, e, r
    )(block)
