package com.fdilke.bewl.helper

// Efficiently tell if a sequence has the required size
// - without taking any more of its elements than necessary

object MiserlyMeasure {
  def apply[A](
    sequence: Traversable[A],
    expectedSize: Int
  ): Boolean =
    sequence.toSeq.lengthCompare(
      expectedSize
    ) == 0
}
