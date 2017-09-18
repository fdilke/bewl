package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos.{BaseTopos, ToposEnrichments, ToposStructures}
import com.fdilke.bewl.helper.⊕

import scala.language.{higherKinds, postfixOps}

trait RelationalAlgebra extends
  BaseTopos with
  ToposEnrichments with
  ToposStructures with
  AlgebraicMachinery {

  Ɛ: AlgebraicStructures =>

  case class Relation[S <: ~, T <: ~](
    source: DOT[S],
    target: DOT[T],
    criterion: BiArrow[S, T, TRUTH]
  ) {
    def apply(s: S, t: T) =
      criterion(s, t)

    def inverse: Relation[T, S] =
      Relation(
        target,
        source,
        (t, s) =>
          criterion(s, t)
      )

    def o[U <: ~](
      that: Relation[T, U]
    ) =
      Relation[S, U](
        source,
        that.target,
        (source x that.target).existsMid(
          that.source
        ) { (s : S, t: T, u: U) =>
          criterion(s, t) ∧
            that.criterion(t, u)
        }
      )
  }

  implicit class EndoRelation[S <: ~](
    relation: Relation[S, S]
  ) {
    import relation._

    def isReflexive: Boolean =
      (criterion.arrow o
        source.diagonal
      ) toBool

    def isSymmetric: Boolean =
      inverse == relation
  }
  
  object Relation {
    def diagonalRelation[S <: ~](
      carrier: DOT[S]
    ): Relation[S, S] =
      Relation[S, S](
        carrier,
        carrier,
        carrier.=?=
      )

    def apply[S <: ~, T <: ~](
     source: DOT[S],
     target: DOT[T],
     bifunc: (S, T) => TRUTH
   ): Relation[S, T] =
      Relation(
        source,
        target,
        (source x target).biArrow(
          omega
        )(
          bifunc
        )
      )
  }
}
