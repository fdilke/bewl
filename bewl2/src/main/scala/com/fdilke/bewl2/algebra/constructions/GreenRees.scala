package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec

// See green-rees-theory.md

object GreenRees {

  extension[H](word: Seq[H])
    def factorize: Factorization[H] =
      val allLetters: Set[H] = word.toSet
      val (leftSeg: Seq[H], leftLetter: H) = word.leftSegment(allLetters)
      val (rightSegR, rightLetter): (Seq[H], H) = word.reverse.leftSegment(allLetters)
      Factorization(leftSeg, leftLetter, rightLetter, rightSegR.reverse)
      
    def leftSegment: (Seq[H], H) =
      leftSegment(word.toSet)

    def leftSegment(
      allLetters: Set[H]
    ): (Seq[H], H) =
      if word.isEmpty then
        throw new IllegalArgumentException("can't extract segment from empty word")
      extractLeftSegmentHelper(word, Seq.empty, word.toSet)

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
    

  case class Factorization[H](
    leftSegment: Seq[H],
    leftLetter: H,
    rightLetter: H,
    rightSegment: Seq[H]
  )
}
