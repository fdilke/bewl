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
}
