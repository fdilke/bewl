package com.fdilke.bewl2.utility

import com.fdilke.bewl2.algebra.AlgebraicConstructions._
import StockSymbols._
import com.fdilke.bewl2.sets.Sets
import Sets._

object StockStructures:
  // TODO: I'm sure this could have a better name, or be constructed differently: right dominant
  def withMonoidOf3[RESULT](
    block: Dot[Symbol] ?=> Monoid[Symbol] ?=> RESULT
  ): RESULT =
    withMonoidFromTable(
      e, a, b,
      a, a, b,
      b, a, b
    )(block)

  // TODO: I'm sure this could have a better name, or be constructed differently: left dominant
  def withMonoidOf3a[RESULT](
    block: Dot[Symbol] ?=> Monoid[Symbol] ?=> RESULT
  ): RESULT =
    withMonoidFromTable(
      e, a, b,
      a, a, a,
      b, b, b
    )(block)
    
  def withMonoid_1_0[RESULT](
    block: Dot[Int] ?=> Monoid[Int] ?=> RESULT
  ): RESULT =
    withMonoidFromTable(
      1, 0,
      0, 0
    )(block)
    
  def with_S_3[RESULT](
    block: Dot[Symbol] ?=> Group[Symbol] ?=> RESULT
  ): RESULT =
    withGroupFromTable(
      e, a, b, c, r, s,
      a, e, s, r, c, b,
      b, r, e, s, a, c,
      c, s, r, e, b, a,
      r, b, c, a, s, e,
      s, c, a, b, e, r
    )(block)
