package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsPreArrow}

import scala.language.postfixOps

case class Cycle[T](members: T*) {
  private val numMembers = members.size

  def mappings: Seq[(T, T)] =
    for (i <- 0 until numMembers)
      yield (
        members(i),
        members((i + 1) % numMembers)
      )
}

class PermutationBuilder[T](
  cycles: Seq[Cycle[T]]
) {
  def apply(cycle: T*) =
    new PermutationBuilder(
      cycles :+ Cycle(cycle :_*)
    )

  private def dot(
    values: Set[T],
    mappings: Map[T, T]
  ) =
    Permutations.topos.makeDot(
      FiniteSets.makeArrow(
        FiniteSetsPreArrow(
          values,
          values,
          mappings
        )
      )
    )

  private def allAsSet: Set[T] =
    cycles.map {
      _.members.toSet
    }.fold (
      Set.empty
    )(
      _ union _
    )

  private def allMappings: Map[T, T] =
    Map(
      cycles.map {
        _.mappings
      }.fold (
        Seq.empty
      ) (
        _ ++ _
      ) :_*
    )

  def π =
    dot(
      allAsSet,
      allMappings
    )
}

object Permutations {
  val topos = FiniteSets.ToposOfAutomorphisms.build

  def π[T] = new PermutationBuilder[T](Seq.empty)

  implicit class SmartPermutation[T](
    permutation: Permutations.topos.DOT[
      Permutations.topos.WRAPPER[T]
    ]
  ) {
    lazy val asArrow =
      Permutations.topos unwrap permutation

    lazy val asMap =
      asArrow.source.globals map {
        _(())
      } map { e =>
        e -> asArrow(e)
      } toMap

    lazy val parity =
      Parity of asMap
  }
}

object Parity {
  val EVEN = +1
  val ODD = -1

  def of[T](permutation: Map[T, T]): Int =
    permutation.toSeq match {
      case Nil =>
        EVEN
      case (x, px) +: rest =>
        if (x == px)
          of(permutation - x)
        else
          -of(permutation - px + (x -> permutation(px)))
    }
}

