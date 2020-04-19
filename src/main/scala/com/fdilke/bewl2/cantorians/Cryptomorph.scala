package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.CoPitcher.functionAsCatcher
import com.fdilke.bewl2.topology.Hausdorff

sealed class Cryptomorph[H: Hausdorff](
  val coCantorian: Cantorian => H
) {
  implicit val fnCatcherTude: Catcher[Cantorian => H, Boolean, H] =
    functionAsCatcher[Cantorian, Boolean, H]

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
  ): Cryptomorph[H] =
    new Cryptomorph(
      coC
    )

  def apply[
    C,
    H: Hausdorff
  ](
    catcher: C
  )(
    implicit catcherTude: Catcher[C, Boolean, H]
  ): Cryptomorph[H] =
    new Cryptomorph(
      Catcher.applyCatcher(catcher)(_)
    )
}
