package com.fdilke.bewl.helper

// Efficiently tell if a sequence has the required size
// - without taking any more of its elements than necessary

object MiserlyMeasure {
  def apply[A](
    sequence: Traversable[A],
    expectedSize: Int
  ): Boolean = {
    val expectedEmpty = expectedSize == 0
    if (sequence.isEmpty)
      expectedEmpty
    else if (expectedEmpty)
      false
    else
      MiserlyMeasure(
        sequence.tail,
        expectedSize - 1
      )
  }
}
