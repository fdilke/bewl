package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSetsUtilities.monoidFromTable

object KnownMonoids {

  private val (i, x, y) = ('i, 'x, 'y)

  val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

}
