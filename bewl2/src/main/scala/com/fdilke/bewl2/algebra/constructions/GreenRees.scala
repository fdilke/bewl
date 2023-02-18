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

    def canonical: String =
      canonicalFactorize(word.factorize)

    def *(word2: String) =
      (word + word2).canonical

  def canonicalFactorize(
    maybeFac: Option[Factorization]
  ): String =
      maybeFac.map {
        _.canonical
      } getOrElse ""

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

  private def permutations(n: Int): BigInt =  
    (1 to n).map(BigInt(_)).foldLeft(BigInt(1))(_ * _)

  private def combinations(n: Int, k: Int): BigInt =
    permutations(n) / (permutations(k) * permutations(n - k))    

  def orderFree(n: Int): BigInt =
    (for { k <- 0 to n } yield {
      (for { i <- 1 to k } yield {
        BigInt(k - i + 1).pow(BigInt(2).pow(i).toInt)
      }).product * combinations(n, k)
    }).sum

  case class Factorization(
    prefix : Option[Factorization],
    leftEnd : Char,
    rightEnd : Char,
    suffix : Option[Factorization]
  ):
    def canonical: String =
      sandwich(
        canonicalFactorize(prefix) :+ leftEnd,
        rightEnd +: canonicalFactorize(suffix),
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

