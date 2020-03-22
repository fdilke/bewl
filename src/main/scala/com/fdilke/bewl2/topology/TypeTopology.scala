package com.fdilke.bewl2.topology

trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]
}

object Compact {
  def compactnessFor(
    enum: Enumeration
  ): Compact[enum.Value] =
    (predicate: enum.Value => Boolean) =>
      enum.values find(predicate) map {
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
  implicit class HausdorffForEnum[ENUM <: Enumeration](
    enum: ENUM
  ) extends Hausdorff[ENUM#Value] {
    override def equal(
      t1: ENUM#Value,
      t2: ENUM#Value
    ): Boolean =
      t1 == t2
  }
}
