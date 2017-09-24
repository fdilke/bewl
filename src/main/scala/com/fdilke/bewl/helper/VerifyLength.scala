package com.fdilke.bewl.helper

// Tell if a sequence has a given length without
// taking any more of it than necessary

object VerifyLength {
  def apply[A](
    sequence: Seq[A],
    expectedSize: Int
  ): Boolean =
    sequence.lengthCompare(
      expectedSize
    ) == 0
}
