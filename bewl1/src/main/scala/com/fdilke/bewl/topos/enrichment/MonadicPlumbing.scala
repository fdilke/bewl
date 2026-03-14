package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}

trait MonadicPlumbing {

  Ɛ: BaseTopos with ToposStructures =>

  def twist[
    A <: ~,
    B <: ~
  ](
    a: DOT[A],
    b: DOT[B]
  ): (A x B) > (B x A) =
    (a -* b).x(a *- b)

  def associator[
    A <: ~,
    B <: ~,
    C <: ~
  ](
    a: DOT[A],
    b: DOT[B],
    c: DOT[C]
  ): ((A x B) x C) > (A x (B x C)) = {
    val ab_c = (a.x(b)).x(c)
    ((a *- b)
      .o(ab_c.π0))
      .x(
        ((a -* b).o(ab_c.π0)).x(ab_c.π1)
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
    val a_bc = a.x(b.x(c))
    (a_bc.π0.x((b *- c).o(a_bc.π1))).x((b -* c).o(a_bc.π1))
  }
}
