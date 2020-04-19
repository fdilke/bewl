package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Compact
import com.fdilke.bewl2.topology.PitcherPredicateSolver.solvePitcher

trait PitcherFType[
  PITCHER <: PitcherFType[PITCHER, T],
  T
] {
  def head: T
  def tail: PITCHER
}
trait Pitcher[P, T] {
  def head(
    pitcher: P
  ): T
  def tail(
    pitcher: P
  ): P
  def construct(
    head: => T,
    tail: => P
  ): P
}

object Pitcher {
  def apply[P, T](
    implicit pitcher: Pitcher[P, T]
  ): Pitcher[P, T] = pitcher

  @inline
  def head[P, T](
    p: P
  )(
    implicit pitcher: Pitcher[P, T]
  ): T =
    pitcher.head(p)

  @inline
  def tail[P, T](
    p: P
  )(
    implicit pitcher: Pitcher[P, T]
  ): P =
    pitcher.tail(p)

  @inline
  def construct[P, T](
    head: => T,
    tail: => P
  )(
    implicit pitcher: Pitcher[P, T]
  ): P =
    pitcher.construct(head, tail)

  def constantly[P, T](
    t: T
  )(
    implicit pitcher: Pitcher[P, T]
  ): P = {
    lazy val constantPitcher: P =
      construct(
        t,
        constantPitcher
      )
    constantPitcher
  }
  def compactness[
    P,
    C: Compact
  ](
    implicit pitcherTude: Pitcher[P, C]
  ): Compact[P] =
    (predicate: P => Boolean) => solvePitcher[P, C](predicate).map(p => () => p)
}

trait VanillaPitcher[T] {
  def head: T
  def tail: VanillaPitcher[T]
}

object VanillaPitcher {
  def apply[T](
    t: => T,
    pitcher: => VanillaPitcher[T]
  ): VanillaPitcher[T] =
    new VanillaPitcher[T] {
      override def head: T = t
      override def tail: VanillaPitcher[T] = pitcher
    }

  implicit def pitcherForVanillaPitcher[T]: Pitcher[VanillaPitcher[T], T] =
    new Pitcher[VanillaPitcher[T], T] {
      override def head(
        pitcher: VanillaPitcher[T]
      ): T =
        pitcher.head

      override def tail(
        pitcher: VanillaPitcher[T]
      ): VanillaPitcher[T] =
        pitcher.tail

      override def construct(
        head: => T,
        tail: => VanillaPitcher[T]
      ): VanillaPitcher[T] =
        VanillaPitcher(head, tail)
    }
}
