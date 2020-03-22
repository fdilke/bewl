package com.fdilke.bewl2.topology

trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]
}

object Compact {
  implicit def CompactnessForEnum[ENUM <: Enumeration](
    enum: ENUM
  ): Compact[enum.Value] =
    (predicate: enum.Value => Boolean) =>
      enum.values find (predicate) map {
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
//  implicit def xx[C : Compact, H : Hausdorff]:
}
