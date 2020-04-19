package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.CoPitcher.functionAsCatcher
import com.fdilke.bewl2.topology.Hausdorff

sealed class Cryptomorph[H](
  val coCantorian: Cantorian => H
)(
  implicit catcherTude: Catcher[
    Cantorian => H,
    Boolean,
    H
  ],
  hausdorffTude: Hausdorff[H]
) {
  lazy val dyad: Dyad[H] =
    Catcher.recast[Cantorian => H, Dyad[H], Boolean, H](
      coCantorian
    )

  def as[CT[_]](
    implicit ctCatcherTude: Catcher[CT[H], Boolean, H]
  ): CT[H] =
    Catcher.recast[Cantorian => H, CT[H], Boolean, H](
      coCantorian
    )
}

object Cryptomorph {
  def apply[
    H: Hausdorff
  ](
    coC: Cantorian => H
  ): Cryptomorph[H] = {
    implicit val fnCatcherTude: Catcher[Cantorian => H, Boolean, H] =
      functionAsCatcher[Cantorian, Boolean, H]
    new Cryptomorph(
      coC
    )
  }

  def apply[
    C,
    H: Hausdorff
  ](
    catcher: C
  )(
    implicit catcherTude: Catcher[C, Boolean, H]
  ): Cryptomorph[H] = {
    implicit val fnCatcherTude: Catcher[Cantorian => H, Boolean, H] =
      functionAsCatcher[Cantorian, Boolean, H]
    new Cryptomorph(
      Catcher.recast[C, Cantorian => H, Boolean, H](
        catcher
      )
    )
  }
}
