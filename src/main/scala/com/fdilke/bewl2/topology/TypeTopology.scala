package com.fdilke.bewl2.topology

import scala.reflect.ClassTag


trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]
}

object Compact {
  def compactnessFor(
    enum: Enumeration // : ClassTag
  ): Compact[enum.Value] =
    (predicate: (enum.Value => Boolean)) =>
      enum.values find(predicate) map {
        v => () => v
      }
}
