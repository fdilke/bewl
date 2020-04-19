package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Pitcher.{head, tail}
import com.fdilke.bewl2.topology.Compact.forAll
import com.fdilke.bewl2.topology.PitcherPredicateSolver.solvePitcher
import com.fdilke.bewl2.topology.{Compact, Hausdorff}

import scala.annotation.tailrec

trait PitcherFType[
  PITCHER <: PitcherFType[PITCHER, T],
  T
] {
  def head: T
  def tail: PITCHER
}

trait CatcherFType[
  SELF <: CatcherFType[SELF, T, U],
  T,
  U
] { self: SELF =>

  def either: Either[
    U,
    T => SELF
  ]
}

object CatcherFType {
  def standardCatcher[
    C <: CatcherFType[C, T, U],
    T,
    U
  ](
    constructFn: Either[U, T => C] => C
  ): Catcher[C, T, U] =
    new Catcher[C, T, U] {
      override def either(c: C): Either[U, T => C] =
        c.either

      override def construct(e: => Either[U, T => C]): C =
        constructFn(e)
    }
}

trait Catcher[C, T, U] {
  def either(c: C): Either[U, T => C]
  def construct(e: => Either[U, T => C]): C
}

object Catcher {

  @inline def apply[C, T, U](
    implicit catcherTude: Catcher[C, T, U]
  ): Catcher[C, T, U] =
    catcherTude

  @inline
  def either[C, T, U](
    c: C
  )(
    implicit tude: Catcher[C, T, U]
  ): Either[U, T => C] =
    tude.either(c)

  @inline
  def construct[C, T, U](
    e: => Either[U, T => C]
  )(
    implicit tude: Catcher[C, T, U]
  ): C =
    tude.construct(e)

  @tailrec
  def applyCatcher[C, P, T, U](
    c: C
  )(
    p: P
  )(
    implicit
    pitcherTude: Pitcher[P, T],
    catcherTude: Catcher[C, T, U]
  ): U =
    either(c) match {
      case Left(u) => u
      case Right(t2c) =>
        applyCatcher(
          t2c(head(p))
        )(
          tail(p)
        )
    }

  def hausdorff[CATCHER, C: Compact, H: Hausdorff](
    implicit catcherTude: Catcher[CATCHER, C, H]
  ): Hausdorff[CATCHER] =
    new Hausdorff[CATCHER] {
      override def equalH(
        catcher1: CATCHER,
        catcher2: CATCHER
      ): Boolean =
        forAll[VanillaPitcher[C]] { pitcher =>
          Hausdorff.equalH(
            applyCatcher(catcher1)(pitcher),
            applyCatcher(catcher2)(pitcher)
          )
        }(Pitcher.compactness[VanillaPitcher[C], C])
      override def intKey(catcher: CATCHER): Int =
        Compact.optional[C] match {
          case None => 0
          case Some(c) =>
            Hausdorff.intKey(
              applyCatcher(catcher)(
                Pitcher.constantly[VanillaPitcher[C], C](c)
              )
            )
        }
    }

  def recast[C1, C2, S, T](
    c1: C1
  )(
    implicit catcherTude1: Catcher[C1, S, T],
    catcherTude2: Catcher[C2, S, T]
  ): C2 = {
    def recastSub(c: C1): C2 =
      catcherTude1.either(c) match {
        case Left(t)    => catcherTude2.construct(Left(t))
        case Right(s2c) => catcherTude2.construct(Right(s => recastSub(s2c(s))))
      }
    recastSub(c1)
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
