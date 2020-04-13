package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.PitcherPredicateSolver.solvePitcher
import com.fdilke.bewl2.topology.{Compact, PitcherPredicateSolver}

trait PitcherFType[
  PITCHER <: PitcherFType[PITCHER, T],
  T
] {
  def head: T
  def tail: PITCHER
}

trait Catcher[
  SELF <: Catcher[SELF, T, U],
  T,
  U
] { self: SELF =>

  def either: Either[
    U,
    T => SELF
  ]

  final def apply[
    PITCHER <: PitcherFType[PITCHER, T]
  ](
    pitcher: PITCHER
  ): U =
    either match {
      case Left(u) => u
      case Right(t2self) =>
        t2self(pitcher.head)(pitcher.tail)
    }
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

trait JonssonTarski[J] {
  def join(l: J, r: J): J
  def left(join: J): J
  def right(join: J): J
}

object JonssonTarski {
  @inline
  def apply[J](
    implicit tude: JonssonTarski[J]
  ): JonssonTarski[J] = tude

  @inline
  def join[J: JonssonTarski](l: J, r: J): J =
    JonssonTarski[J].join(l, r)

  @inline
  def left[J: JonssonTarski](join: J): J =
    JonssonTarski[J].left(join)

  @inline
  def right[J: JonssonTarski](join: J): J =
    JonssonTarski[J].right(join)
}
