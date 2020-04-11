package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Compact

trait PitcherOld[
  PITCHER <: PitcherOld[PITCHER, T],
  T
] {
  val head: T
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
    PITCHER <: PitcherOld[PITCHER, T]
  ](
    pitcher: PITCHER
  ): U =
    either match {
      case Left(u) => u
      case Right(t2self) =>
        t2self(pitcher.head)(pitcher.tail)
    }
}

trait VanillaPitcherOld[T] extends PitcherOld[VanillaPitcherOld[T], T]

object VanillaPitcherOld {
  implicit def compactVanillaPitcher[
    C: Compact
  ]: Compact[VanillaPitcherOld[C]] =
    predicate => ???

  def apply[T](
    t: T,
    pitcher: => VanillaPitcherOld[T]
  ): VanillaPitcherOld[T] =
    new VanillaPitcherOld[T] {
      override val head: T = t
      override def tail: VanillaPitcherOld[T] = pitcher
    }

  def constantly[T](t: T): VanillaPitcherOld[T] =
    new VanillaPitcherOld[T] { pitcher =>
      override val head: T = t
      override def tail: VanillaPitcherOld[T] = pitcher
    }
}

trait Pitcher[P[_]] {
  def head[T](
    pitcher: P[T]
  ): T
  def tail[T](
    pitcher: P[T]
  ): P[T]
  def construct[T](
    head: => T,
    tail: => P[T]
  ): P[T]
}

object Pitcher {
  def apply[P[_]: Pitcher](
    implicit pitcher: Pitcher[P]
  ): Pitcher[P] = pitcher

  @inline
  def head[T, P[_]: Pitcher](
    pitcher: P[T]
  ): T =
    Pitcher[P].head(pitcher)

  @inline
  def tail[T, P[_]: Pitcher](
    pitcher: P[T]
  ): P[T] =
    Pitcher[P].tail(pitcher)

  @inline
  def construct[T, P[_]: Pitcher](
    head: => T,
    tail: => P[T]
  ): P[T] =
    Pitcher[P].construct(head, tail)

  def constantly[T, P[_]: Pitcher](
    t: T
  ): P[T] = {
    lazy val constantPitcher: P[T] =
      construct(
        t,
        constantPitcher
      )
    constantPitcher
  }
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

  implicit val pitcherForVanillaPitcher: Pitcher[VanillaPitcher] =
    new Pitcher[VanillaPitcher] {
      override def head[T](
        pitcher: VanillaPitcher[T]
      ): T =
        pitcher.head

      override def tail[T](
        pitcher: VanillaPitcher[T]
      ): VanillaPitcher[T] =
        pitcher.tail

      override def construct[T](
        head: => T,
        tail: => VanillaPitcher[T]
      ): VanillaPitcher[T] =
        VanillaPitcher(head, tail)
    }
}
