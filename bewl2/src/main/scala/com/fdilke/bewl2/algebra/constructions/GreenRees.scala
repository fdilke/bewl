package com.fdilke.bewl2.algebra.constructions

// See green-rees-theory.md

object GreenRees {
  def factorize[H](word: Seq[H]): Factorization[H] =
    val (leftSegment, leftLetter) = extractLeftSegment(word)
    val (rightSegmentR, rightLetter) = extractLeftSegment(word.reverse)
    Factorization(leftSegment, leftLetter, rightLetter, rightSegmentR.reverse)

  def segmentIndex[H](word: Seq[H]): Int =
    if word.isEmpty then
      throw new IllegalArgumentException("can't extract segment from empty word")
    else
      word.indices.find { i =>
        val segment = word.slice(0, i+1)
        word.forall { segment.contains }
      }.get

  def extractLeftSegment[H](word: Seq[H]): (Seq[H], H) =
    val lastIndex: Int = segmentIndex(word)
    val leftSegment = word.slice(0, lastIndex)
    val leftLetter = word(lastIndex)
    (leftSegment, leftLetter)

  case class Factorization[H](
    leftSegment: Seq[H],
    leftLetter: H,
    rightLetter: H,
    rightSegment: Seq[H]
  )
}
