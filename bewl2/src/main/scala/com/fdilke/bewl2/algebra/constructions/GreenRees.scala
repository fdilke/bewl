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
            prefix = prefix.factorize,
            leftEnd = leftEnd,
            rightEnd = rightEnd,
            suffix = suffixReversed.reverse.factorize
          )
        )

    def =!=(word2: String): Boolean =
      word.factorize == word2.factorize

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
  prefix : Option[Factorization],
  leftEnd : Char,
  rightEnd : Char,
  suffix : Option[Factorization]
)

