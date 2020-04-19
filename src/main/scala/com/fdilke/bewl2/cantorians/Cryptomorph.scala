package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Hausdorff

sealed abstract class Cryptomorph[H: Hausdorff] {

  implicit val coCantorianCatcherTude: Catcher[Cantorian => H, Boolean, H] =
    CoPitcher.functionAsCatcher[Cantorian, Boolean, H]

  def coCantorian: Cantorian => H =
    ???

  def asDyad: Dyad[H] =
    Catcher.recast[Cantorian => H, Dyad[H], Boolean, H](
      coCantorian
    )
}

object Cryptomorph {
  def apply[
    H: Hausdorff
  ](
    coC: Cantorian => H
  ): Cryptomorph[H] =
    new Cryptomorph {
      override def coCantorian: Cantorian => H =
        coC
    }
}
