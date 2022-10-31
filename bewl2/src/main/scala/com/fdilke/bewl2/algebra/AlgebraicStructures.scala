package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.Topos
import com.fdilke.utility.Shortcuts.*

import scala.annotation.targetName
import scala.language.{dynamics, postfixOps}
import Mappable._

trait AlgebraicStructures[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  →[_, _]
] extends AlgebraicTheories[DOT, CTXT, VOID, UNIT, BEWL, →] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, →] =>

  import StandardTermsAndOperators._
  import StandardTermsAndOperators.!
  import StandardTermsAndOperators.*

  val groups: AlgebraicTheory[UNIT] =
    AlgebraicTheory(ι, !, *)(
      "left unit" law { ι * α := α },
      "right unit" law { α * ι := α },
      "left inverse" law { (!α) * α := ι },
      "associative" law { (α * β) * γ := α * (β * γ) }
    )

  class Group[G : DOT](
   unit: NullaryOp[G],
   multiply: BinaryOp[G],
   inverse: UnaryOp[G]
  ) extends groups.Algebra[G](
    ι := unit,
    * := multiply,
    (!) := inverse
  ):
    def x[H : DOT]( // product sugar
      that: Group[H]
    ): Group[(G, H)] = {
      val product = (this: groups.Algebra[G]) x that
      new Group[(G, H)](
        product.operatorAssignments.lookup(ι).get,
        product.operatorAssignments.lookup(*).get,
        product.operatorAssignments.lookup(!).get
      )
    }

  val monoids: AlgebraicTheory[UNIT] =
    AlgebraicTheory(ι, *)(
      "left unit" law (ι * α := α),
      "right unit" law (α * ι := α),
      "associative" law ((α * β) * γ := α * (β * γ))
    )

  class Monoid[M: DOT](
    val unit: NullaryOp[M],
    val multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](
    ι := unit,
    * := multiply
  ) // with Actions[M] with CommutativityCriterion
}
