package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec

// See green-rees-theory.md

object GreenRees {
  def factorize[H](word: Seq[H]): Factorization[H] =
    val allLetters = word.toSet
    val (leftSegment, leftLetter) = extractLeftSegment(word, allLetters)
    val (rightSegmentR, rightLetter) = extractLeftSegment(word.reverse, allLetters)
    Factorization(leftSegment, leftLetter, rightLetter, rightSegmentR.reverse)

  @tailrec private def extractLeftSegmentHelper[H](
    segment: Seq[H],
    seen: Seq[H],
    lettersRemaining: Set[H]
  ): (Seq[H], H) =
    val letter = segment.head
    val newLettersRemaining = lettersRemaining - letter
    if (newLettersRemaining.isEmpty) then
      (seen, letter)
    else
      extractLeftSegmentHelper(segment.tail, seen :+ letter, newLettersRemaining)

  def extractLeftSegment[H](
    word: Seq[H],
    allLetters: Set[H]
  ): (Seq[H], H) =
    if word.isEmpty then
      throw new IllegalArgumentException("can't extract segment from empty word")
    extractLeftSegmentHelper(word, Seq.empty, word.toSet)

  def extractLeftSegment[H](
    word: Seq[H]
  ): (Seq[H], H) =
    extractLeftSegment(word, word.toSet)

  case class Factorization[H](
    leftSegment: Seq[H],
    leftLetter: H,
    rightLetter: H,
    rightSegment: Seq[H]
  )
}
