package com.fdilke.bewl2.topos.constructions

import com.fdilke.bewl2.topos.{ProductMappable, Topos}

trait ToposOfPermutations[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
]:
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  lazy val toposOfPermutations: Topos[
    [A] =>> (A => A, A => A),
    CTXT,
    VOID, 
    UNIT, 
    BEWL,
    >
  ] =
    ???

