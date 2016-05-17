package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.topos.{ToposStructures, BaseTopos}

trait MonadicPlumbing {

  Ɛ: BaseTopos with ToposStructures =>

  def associator[
  A <: ~,
  B <: ~,
  C <: ~
  ](
    a: DOT[A],
    b: DOT[B],
    c: DOT[C]
  ): ((A x B) x C) > (A x (B x C)) = {
    val ab_c = (a x b) x c
    ((a x b).π0 o ab_c.π0) x (
      ((a x b).π1 o ab_c.π0) x
      ab_c.π1
      )
  }

  def coassociator[
    A <: ~,
    B <: ~,
    C <: ~
  ](
    a: DOT[A],
    b: DOT[B],
    c: DOT[C]
  ): (A x (B x C)) > ((A x B) x C) = {
    val a_bc = a x (b x c)
    (a_bc.π0 x
      ((b x c).π0 o a_bc.π1)
    ) x
      ((b x c).π1 o a_bc.π1)
  }
}
