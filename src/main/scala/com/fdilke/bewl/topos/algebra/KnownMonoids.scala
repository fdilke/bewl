package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSetsUtilities.monoidFromTable
import com.fdilke.bewl.helper.StandardSymbols.{i, x, y}

object KnownMonoids {

  val monoidOf3 =
    monoidFromTable(
      i,
      x,
      y,
      x,
      x,
      y,
      y,
      x,
      y
    ) // right-dominant on two generators
}
