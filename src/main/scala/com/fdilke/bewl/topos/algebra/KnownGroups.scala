package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities.monoidFromTable

object KnownGroups {

  private val (i, a) = ('i, 'a)

  val twoGroup = FiniteSets.groupOfUnits(
    monoidFromTable(
      i, a,
      a, i
    )
  )._1
}
