package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.apps.permutations.Permutations.Permutation
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsPreArrow}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.elementsOf

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

  def π : Permutation[T] =
    Permutations.dot(
      allMappings
    )
}

object Permutations {
  lazy val topos = FiniteSets.ToposOfAutomorphisms.build

  type Permutation[T] = Permutations.topos.DOT[T]

  def dot[T](
    mappings: Map[T, T]
  ): Permutation[T] = {
    val values: Set[T] = mappings.keySet

    Permutations.topos.makeDot(
      FiniteSets.makeArrow(
        FiniteSetsPreArrow(
          values,
          values,
          mappings
        )
      )
    )
  }

  def π[T] = new PermutationBuilder[T](Seq.empty)

  implicit class RichPermutation[T](
    permutation: Permutation[T]
  ) {
    lazy val asArrow =
      Permutations.topos unwrap permutation

    lazy val carrier =
      elementsOf(asArrow.source) toSet

    lazy val asMap =
      carrier map { e =>
        e -> asArrow(e)
      } toMap

    lazy val parity =
      Parity of asMap

    def send(key: T) =
      asMap(key)

    def *(that: RichPermutation[T]): Permutation[T] =
      if (carrier == that.carrier)
        dot(
          asMap mapValues that.asMap
        )
      else
        throw new IllegalArgumentException
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

