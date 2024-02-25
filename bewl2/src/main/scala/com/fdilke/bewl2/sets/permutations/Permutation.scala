package com.fdilke.bewl2.sets.permutations

class Permutation[A](
  map: Map[A, A]
):
  def domain: Set[A] =
    map.keySet
  def apply(a: A): A =
    map.get(a).getOrElse(
      throw new IllegalArgumentException(s"key $a not found in map $map")
    )

class Cycle[A](
  cycle: Seq[A]
):
  if (cycle.toSet.size < cycle.length)
    throw new IllegalArgumentException("cycle repeats")

  def toMap: Map[A, A] =
    cycle.indices.map { i =>
      cycle(i) -> cycle( (i + 1) % cycle.length ) 
    }.toMap[A, A]

class OngoingPermutation[A](
  cycles: Seq[Cycle[A]]
):
  def toPermutation: Permutation[A] =
    new Permutation(
      cycles map { 
        _.toMap
      } reduce {
        _ ++ _ 
      }
    )

  def apply(a: A, tail: A*): OngoingPermutation[A] =
    new OngoingPermutation(
      cycles :+ Cycle(a +: tail)
    )

object Permutation:
  def apply[A](a: A, tail: A*): OngoingPermutation[A] =
    new OngoingPermutation(
      Seq[Cycle[A]](
        Cycle(a +: tail)
      )
    )

  implicit def convertOngoing[A](
    ongoing: OngoingPermutation[A]
  ): Permutation[A] =
    ongoing.toPermutation
  

