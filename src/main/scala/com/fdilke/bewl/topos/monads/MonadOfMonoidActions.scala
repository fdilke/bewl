package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.topos.algebra.AlgebraicStructures
import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}

trait MonadOfMonoidActions {
  Ɛ: BaseTopos with
    ToposStructures with
    AlgebraicStructures =>

  def monadOfActions[
    M <: ~
  ](
    monoid: Monoid[M]
  ) =
    0
}
