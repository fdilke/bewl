package com.fdilke.bewl.topos


trait AlgebraicStructures { topos: BaseTopos with AlgebraicMachinery  =>

  import StandardTermsAndOperators._
  import NamedLaws._

  lazy val monoids = AlgebraicTheory(ι)(*)(
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  trait CommutativityCriterion { algebra: Algebra =>
    def isCommutative = satisfies(
      α * β := β * α
    )
  }

  case class Monoid[M <: ~](
    override val carrier: DOT[M],
    unit: NullaryOp[M],
    multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](carrier)(
    ι := unit,
    * := multiply
  ) with CommutativityCriterion

  lazy val groups = AlgebraicTheory(ι)($minus, *)( // TODO try ~
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "left inverse" law ( (-α) * α := ι ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  case class Group[G <: ~](
    override val carrier: DOT[G],
    unit: NullaryOp[G],
    multiply: BinaryOp[G],
    inverse: UnaryOp[G]
  ) extends groups.Algebra[G](carrier)(
    ι := unit,
    * := multiply,
    $minus := inverse
  ) with CommutativityCriterion
}
