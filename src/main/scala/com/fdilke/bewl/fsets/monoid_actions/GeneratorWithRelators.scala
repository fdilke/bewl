package com.fdilke.bewl.fsets.monoid_actions

import scala.language.postfixOps

case class GeneratorWithRelators[M, A](
  generator: A,
  relators: Seq[Relator[M]]
)

case class Relator[M](
  selfScalar: M,
  otherIndex: Int,
  otherScalar: M
)

object CullRelators {
  def apply[M](
    index: Int,
    relators: Seq[Relator[M]]
  ): Seq[Relator[M]] = {
    val (iRelators, otherRelators) =
      relators.partition {
        _.otherIndex == index
      }

    val covered: Seq[Relator[M]] =
      iRelators
        .filter(r => r.selfScalar != r.otherScalar)
        .map {
          case Relator(m, _, n) =>
            Set(m, n)
        }
        .distinct
        .map {
          _.toSeq
        }
        .map {
          case Seq(m, n) =>
            Relator(m, index, n)
        }

    otherRelators ++ covered
  }
}
