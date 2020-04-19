package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Hausdorff

sealed abstract class Cryptomorph[H: Hausdorff] {

  private implicit val coCantorianCatcherTude: Catcher[
    Cantorian => H,
    Boolean,
    H
  ] =
    CoPitcher.functionAsCatcher[Cantorian, Boolean, H]

  def coCantorian: Cantorian => H =
    Catcher.recast[Dyad[H], Cantorian => H, Boolean, H](
      dyad
    )

  def dyad: Dyad[H] =
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

  def apply[
    H: Hausdorff
  ](
     dyad0: Dyad[H]
   ): Cryptomorph[H] =
    new Cryptomorph {
      override def dyad: Dyad[H] =
        dyad0
    }
}
