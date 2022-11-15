package com.fdilke.bewl2.logic

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.{Mappable, Topos}

trait LogicalOperations[
  DOT[_],
  CTXT[_] : Mappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >] :
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>
