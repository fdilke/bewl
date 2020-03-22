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
}

trait Hausdorff[T] {
  def equal(
    t1: T,
    t2: T
  ): Boolean
}

object Hausdorff {
  implicit def HausdorffForEnum[ENUM <: Enumeration](
    enum: ENUM
  ): Hausdorff[enum.Value] =
      _ == _
}
