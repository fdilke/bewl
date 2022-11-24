package com.fdilke.bewl2.logic

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.{Mappable, Topos}
import Mappable._

trait LogicalOperations[
  DOT[_],
  CTXT[_] : Mappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >] :
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  trait LogicalOperations {
    val and: BiArrow[BEWL, BEWL, BEWL]
    val implies: BiArrow[BEWL, BEWL, BEWL]
    val or: BiArrow[BEWL, BEWL, BEWL]
    val falsity: NullaryOp[BEWL]
  }

  class DefaultLogicalOperations extends LogicalOperations {

    override val and: BiArrow[BEWL, BEWL, BEWL] =
      truth.x(truth).chi

    override val implies: BiArrow[BEWL, BEWL, BEWL] =
      =?=[BEWL].apply[(BEWL, BEWL)](
        and : BiArrow[BEWL, BEWL, BEWL],
        π0[BEWL, BEWL] : BiArrow[BEWL, BEWL, BEWL]
      )

    override val or: BiArrow[BEWL, BEWL, BEWL] =
      ∀[(BEWL, BEWL), BEWL] {
        case (a ⊕ b) ⊕ ω =>
//          val ab = ab_ω.map { _._1 }
//          val a = ab.map { _._1 }
//          val b = ab.map { _._2 }
//          val ω = ab_ω.map { _._2 } // TODO: refactor using tensorial extractors "(x)"
          (a → ω ∧ (b → ω)) → ω
      }

    override val falsity: NullaryOp[BEWL] =
      ∀[UNIT, BEWL] {
        case u_ω => // again, refactor
          val ω = u_ω.map { _._2 }
          ω
      }
  }

  lazy val logicalOperations: LogicalOperations =
    new DefaultLogicalOperations

  lazy implicit val omega: HeytingAlgebra[BEWL] =
    new HeytingAlgebra[BEWL](
      logicalOperations.falsity,
      truth,
      logicalOperations.and,
      logicalOperations.or,
      logicalOperations.implies
    )

  lazy val falsity: NullaryOp[BEWL] =
    logicalOperations.falsity

//  def isBoolean =
//    (truth + falsity).isIso

  implicit class BewlOps(ω: CTXT[BEWL]):
    inline def →(that: CTXT[BEWL]) =
      omega.implies(ω, that)

    inline def ∧(that: CTXT[BEWL]) =
      omega.meet(ω, that)

    inline def ∨(that: CTXT[BEWL]) =
      omega.join(ω, that)
