package com.fdilke.bewl2.actions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.ProductMappable
import com.fdilke.bewl2.Topos

trait MonoidActions[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >]:
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  class MonoidActions[M: Dot](
    monoid: Monoid[M]
  ):
    val pig = 1
