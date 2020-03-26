package com.fdilke.bewl2.topology

import Compact._
import com.fdilke.bewl2.util.FindEnum

import scala.reflect.runtime.universe._
import scala.language.{implicitConversions, postfixOps}

trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]
}

object Compact {

  implicit def CompactnessForEnum[
    ENUM <: Enumeration : TypeTag
  ]: Compact[ENUM#Value] =
    (predicate: ENUM#Value => Boolean) =>
      FindEnum[ENUM].values find predicate map {
        v => () => v
      }

  @inline def find[T : Compact](
    predicate: T => Boolean
  ): Option[
    () => T
  ] =
    implicitly[Compact[T]] find(
      predicate
    )

  @inline def exists[T : Compact](
    predicate: T => Boolean
  ): Boolean =
    find[T](
      predicate
    ) isDefined

  @inline def forAll[T : Compact](
    predicate: T => Boolean
  ): Boolean =
    ! exists[T] {
      !predicate(_)
    }
}

trait Hausdorff[T] {
  def equalH(
    t1: T,
    t2: T
  ): Boolean
}

object Hausdorff {
  implicit def HausdorffForEnum[ENUM <: Enumeration](
    enum: ENUM
  ): Hausdorff[enum.Value] =
      _ == _

  @inline def equalH[T : Hausdorff](
    t1: T,
    t2: T
  ): Boolean =
    implicitly[Hausdorff[T]] equalH(
      t1, t2
    )

  implicit def hausdorffExponential[
    C : Compact,
    H : Hausdorff
  ]: Hausdorff[
    C => H
  ] =
    (f, g) =>
      forAll[C] { c =>
        equalH(
          f(c),
          g(c)
        )
      }
}
