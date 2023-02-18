package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec
import com.fdilke.utility.Shortcuts.*
import com.fdilke.bewl2.algebra.constructions.GreenRees.factorize

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

  // Find the shortest word with a given prefix and suffix
  def sandwich(prefix: String, suffix: String) =
    prefix.indices.find { i =>
      suffix.startsWith(prefix.slice(i, prefix.size))
    } match {
      case None => prefix ++ suffix
      case Some(i) => prefix.slice(0, i) ++ suffix
    }

  case class Factorization(
    prefix : Option[Factorization],
    leftEnd : Char,
    rightEnd : Char,
    suffix : Option[Factorization]
  )

  object Factorization:
    def apply(
      prefix: String,
      leftEnd: Char,
      rightEnd: Char,
      suffix: String
    ): Factorization =
      Factorization(
        prefix.factorize,
        leftEnd,
        rightEnd,
        suffix.factorize
      )

