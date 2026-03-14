package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.helper.IterateToFixed
import com.fdilke.bewl.topos.{BaseTopos, ToposEnrichments, ToposPrerequisites, ToposStructures}
import scala.language.postfixOps

trait RelationalAlgebra extends BaseTopos with ToposEnrichments {

  Ɛ: ToposPrerequisites =>

  import Relation._

  case class Relation[S <: ~, T <: ~](
      source: DOT[S],
      target: DOT[T],
      criterion: BiArrow[S, T, TRUTH]
  ) {
    lazy val subobject: EQUALIZER[S x T] =
      criterion.arrow.whereTrue

    def apply(s: S, t: T) =
      criterion(s, t)

    def <=(that: Relation[S, T]): Boolean =
      that.criterion.arrow o
        subobject.inclusion toBool

    def inverse: Relation[T, S] =
      (target x source) relation { (t, s) => criterion(s, t) }

    def ∨(
        that: Relation[S, T]
    ) =
      criterion.product relation { (s, t) =>
        criterion(s, t) ∨
          that.criterion(s, t)
      }

    def o[U <: ~](
        that: Relation[T, U]
    ) =
      Relation[S, U](
        source,
        that.target,
        (source x that.target).existsMidWithImages(
          that.source
        ) { (s, t, u) =>
          criterion(s, t) ∧
            that.criterion(t, u)
        }
      )
  }

  implicit class EndoRelation[S <: ~](
      relation: Relation[S, S]
  ) {
    import relation._

    def toEquivalence: Relation[S, S] =
      IterateToFixed(
        relation ∨
          relation.inverse ∨
          diagonalRelation(source)
      ) { r => r o r }

    def isReflexive: Boolean =
      (criterion.arrow o
        source.diagonal) toBool

    def isSymmetric: Boolean =
      inverse == relation

    def isTransitive: Boolean =
      o(relation) <= relation

    def isIdempotent: Boolean =
      o(relation) == relation

    def isEquivalence: Boolean =
      isReflexive && isSymmetric && isIdempotent
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
  }
}
