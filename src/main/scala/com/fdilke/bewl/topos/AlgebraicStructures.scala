package com.fdilke.bewl.topos


trait AlgebraicStructures { topos: BaseTopos with AlgebraicMachinery  =>

  import StandardTermsAndOperators._

  lazy val monoids = AlgebraicTheory(StandardTermsAndOperators.O)($plus)() // TODO: make this multiplicative

  case class Monoid[M <: ~](
    override val carrier: DOT[M],
    unit: NullaryOp[M],
    multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](carrier)(StandardTermsAndOperators.O := unit, $plus := multiply) {

  }
}
