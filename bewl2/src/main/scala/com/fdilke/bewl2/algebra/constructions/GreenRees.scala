package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec
import com.fdilke.utility.Shortcuts.*

object GreenRees:
  extension(word: String)
    def leftSegment: (String, Char) =
      if (word.isEmpty)
        throw new IllegalArgumentException("can't extract segment from empty word")      
      else
        val (prefix, last) = extractLeftSegmentHelper(word, "", word.toSet)
        (prefix.string, last)

    def factorize: Option[Factorization] =
      if word.isEmpty() then
        None
      else
        val (prefix, leftEnd) = word.leftSegment
        val (suffixReversed, rightEnd) = word.reverse.leftSegment
        Some(
          Factorization(
            prefix = prefix,
            leftEnd = leftEnd,
            rightEnd = rightEnd,
            suffix = suffixReversed.reverse
          )
        )

  @tailrec private def extractLeftSegmentHelper(  
    segment: Seq[Char],
    seen: Seq[Char],
    lettersRemaining: Set[Char]
  ): (Seq[Char], Char) =
    val letter = segment.head
    val newLettersRemaining = lettersRemaining - letter
    if (newLettersRemaining.isEmpty) then
      (seen, letter)
    else
      extractLeftSegmentHelper(
        segment.tail,
        seen :+ letter,
        newLettersRemaining
      )

case class Factorization(
  prefix : String,
  leftEnd : Char,
  rightEnd : Char,
  suffix : String
)

