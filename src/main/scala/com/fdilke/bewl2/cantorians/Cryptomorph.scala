package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.CoPitcher.functionAsCatcher
import com.fdilke.bewl2.topology.Hausdorff

case class Cryptomorph[H: Hausdorff](
  coCantorian: Cantorian => H
) {
  implicit val fnCatcherTude: Catcher[Cantorian => H, Boolean, H] =
    functionAsCatcher[Cantorian, Boolean, H]

  def recastAs[CT[_]](
    implicit ctCatcherTude: Catcher[CT[H], Boolean, H]
  ): CT[H] =
    Catcher.recast[Cantorian => H, CT[H], Boolean, H](
      coCantorian
    )
}

object Cryptomorph {
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
