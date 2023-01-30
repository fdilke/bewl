package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec

// See green-rees-theory.md

object GreenRees:

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

    def =!=(word2: Seq[H]): Boolean =
      if (word.isEmpty) then
        word2.isEmpty
      else if (word2.isEmpty) then
        false
      else
        word.factorize =!= word2.factorize

    def canonical: Seq[H] =
      if (word.isEmpty) then
        word
      else
        word.factorize.canonical

    def *(word2: Seq[H]): Seq[H] =
      (word ++ word2).canonical

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
    

  def canonicalWords[H](
    letters: Seq[H]
  ): Set[Seq[H]] =
    ???

  case class Factorization[H](
    leftSegment: Seq[H],
    leftLetter: H,
    rightLetter: H,
    rightSegment: Seq[H]
  ):
    def =!=(other: Factorization[H]): Boolean =
      (leftSegment =!= other.leftSegment) &&
      (leftLetter == other.leftLetter) &&
      (rightLetter == other.rightLetter) &&
      (rightSegment =!= other.rightSegment)

    def recombine: Seq[H] =
      val left = leftSegment :+ leftLetter
      val right = rightLetter +: rightSegment
      left.indices.find { i =>
        right.startsWith(left.slice(i, left.size))
      } match {
        case None => left ++ right
        case Some(i) => left.slice(0, i) ++ right
      }

    def canonical: Seq[H] =
      Factorization(
        leftSegment.canonical,
        leftLetter,
        rightLetter,
        rightSegment
      ).recombine

