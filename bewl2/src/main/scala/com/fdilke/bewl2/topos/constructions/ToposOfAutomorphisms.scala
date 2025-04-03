package com.fdilke.bewl2.topos.constructions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.topos.{ProductMappable, Topos}
import ProductMappable._

trait ToposOfAutomorphisms[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
]:
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  case class Automorphism[A : Dot](
    arrow: A ~> A,
    inverse: A ~> A
  ) //:
//    ()
// weak if we can't do this!    
//    def this(arrow: A ~> A) =
//      this(arrow, arrow.inverse)

  object Automorphism:    
    def apply[A : Dot](arrow: A ~> A) =
      new Automorphism[A](arrow, arrow.inverse)

  lazy val toposOfAutomorphisms: Topos[
    Automorphism,
    CTXT,
    VOID, 
    UNIT, 
    BEWL,
    >
  ] =
    ???

