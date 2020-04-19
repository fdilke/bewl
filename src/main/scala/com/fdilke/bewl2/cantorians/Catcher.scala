package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Pitcher.{head, tail}
import com.fdilke.bewl2.topology.Compact.forAll
import com.fdilke.bewl2.topology.{Compact, Hausdorff}

import scala.annotation.tailrec

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
      catcherTude2.construct(
        catcherTude1.either(c) match {
          case Left(t)    => Left(t)
          case Right(s2c) => Right(s => recastSub(s2c(s)))
        }
      )
    recastSub(c1)
  }
}
