package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.{BaseTopos, ToposEnrichments, ToposStructures}

import scala.Function.tupled
import scala.language.{higherKinds, postfixOps}
import scala.language.higherKinds

trait RelationalAlgebra extends
  BaseTopos with
  ToposEnrichments with
  ToposStructures with
  AlgebraicMachinery {

  builder: AlgebraicStructures =>

  object Relation {
    def diagonalRelation[S <: ~](
      carrier: DOT[S]
    ) =
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

  case class Relation[S <: ~, T <: ~](
    source: DOT[S],
    target: DOT[T],
    criterion: BiArrow[S, T, TRUTH]
  ) {
    def apply(s: S, t: T) =
      criterion(s, t)
  }
}
