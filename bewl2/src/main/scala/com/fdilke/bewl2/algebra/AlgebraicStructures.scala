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

  val localGroups: AlgebraicTheory[UNIT] =
    AlgebraicTheory(ι, !, *)(
      "left unit" law { ι * α := α },
      "right unit" law { α * ι := α },
      "left inverse" law { (!α) * α := ι },
      "associative" law { (α * β) * γ := α * (β * γ) }
    )

  class LocalGroup[G : DOT](
   unit: NullaryOp[G],
   multiply: BinaryOp[G],
   inverse: UnaryOp[G]
  ) extends localGroups.Algebra[G](
    ι := unit,
    * := multiply,
    (!) := inverse
  ) {
    // Formalism to handle direct products more elegantly with added sugar
    def x[H : DOT](
      that: LocalGroup[H]
    ): LocalGroup[(G, H)] = {
      val product = (this: localGroups.Algebra[G]) x that
      new LocalGroup[(G, H)](
        product.operatorAssignments.lookup(ι).get,
        product.operatorAssignments.lookup(*).get,
        product.operatorAssignments.lookup(!).get
      )
//      ???
    }
  }
}
