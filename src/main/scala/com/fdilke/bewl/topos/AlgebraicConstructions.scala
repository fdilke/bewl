package com.fdilke.bewl.topos

trait AlgebraicConstructions { topos: BaseTopos with AlgebraicMachinery with AlgebraicStructures  =>
  def endomorphismMonoid[T <: ~](dot: DOT[T]) = {
    val endos = dot > dot
    Monoid(endos, dot.identity.name,
      BiArrow(endos x endos, endos.transpose(endos x endos) {
        case ((f, g), x) => g(f(x))
      })
    )
  }
}
