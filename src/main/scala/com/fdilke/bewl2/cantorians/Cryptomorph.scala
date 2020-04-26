package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Hausdorff

object Cryptomorph {

  type Cryptomorph[H] = CoPitcher[Cantorian, Boolean, H]

  def apply[
    H: Hausdorff
  ](
    coCantorian: Cantorian => H
  ): Cryptomorph[H] =
    new CoPitcher[Cantorian, Boolean, H](
      coCantorian
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
