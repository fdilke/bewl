package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos.{BaseTopos, ToposEnrichments, ToposStructures}

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
