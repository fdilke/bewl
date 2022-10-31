package com.fdilke.bewl2.utility

import com.fdilke.bewl2.algebra.AlgebraicConstructions._
import StockSymbols._
import com.fdilke.bewl2.sets.Sets

object StockStructures:
  // TODO: I'm sure this could have a better name, or be constructed differently
  def withMonoidOf3[RESULT](
    block: Set[Symbol] ?=> Sets.Monoid[Symbol] ?=> RESULT
  ): RESULT =
    monoidFromTable(
      e, a, b,
      a, a, b,
      b, a, b
    )(block)
