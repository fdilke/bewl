package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities.monoidFromTable
import com.fdilke.bewl.helper.StandardSymbols.{i, a}

object KnownGroups {

  val twoGroup = FiniteSets
    .groupOfUnits(
      monoidFromTable(
        i,
        a,
        a,
        i
      )
    )
    ._1
}
