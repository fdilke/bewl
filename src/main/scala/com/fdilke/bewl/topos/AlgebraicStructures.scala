package com.fdilke.bewl.topos


trait AlgebraicStructures { topos: BaseTopos with AlgebraicMachinery  =>

  import StandardTermsAndOperators._
  import NamedLaws._

  lazy val monoids = AlgebraicTheory(ι)(*)(
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  case class Monoid[M <: ~](
    override val carrier: DOT[M],
    unit: NullaryOp[M],
    multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](carrier)(
    ι := unit,
    * := multiply
  )
}
