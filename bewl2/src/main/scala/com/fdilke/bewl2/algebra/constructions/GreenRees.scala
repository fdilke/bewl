package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec
import com.fdilke.utility.Shortcuts.*

object GreenRees:
  extension(word: String)
    def leftSegment: String =
      if (word.isEmpty)
        throw new IllegalArgumentException("can't extract segment from empty word")      
      else
        val (prefix, last) = extractLeftSegmentHelper(word, "", word.toSet)
        (prefix :+ last).string

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
        

