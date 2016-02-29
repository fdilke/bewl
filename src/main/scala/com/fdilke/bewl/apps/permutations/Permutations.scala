package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.fsets.{FiniteSetsPreArrow, FiniteSets}

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

  private def dot(values: T*)(mappings: (T, T)*) = {
    val set: Set[T] = Set(values :_*)
    Permutations.topos.makeDot(
      FiniteSets.makeArrow(
        FiniteSetsPreArrow(
          set,
          set,
          Map(mappings: _*)
        )
      )
    )
  }

  private def allAsSeq: Seq[T] =
    cycles.map {
      _.members
    }.fold (
      Seq.empty
    )(
      _ ++ _
    )

  private def allMappings: Seq[(T, T)] =
    cycles.map {
      _.mappings
    }.fold (
      Seq.empty
    ) (
      _ ++ _
    )

  def π =
    dot(
      allAsSeq :_*
    ) (
      allMappings :_*
    )
}

object Permutations {
  val topos = FiniteSets.ToposOfAutomorphisms.build

  def π[T] = new PermutationBuilder[T](Seq.empty)
}
