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

    def =!=(word2: String): Boolean =
      Factorization(word) == Factorization(word2)

    def canonical: String =
      Factorization(word).toWord

    def *(word2: String) =
      (word + word2).canonical

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

  def setOfCanonicals(
    letters: String
  ): Set[String] =
    AlphabetContext(letters).toSet
    
  trait Wordable {
    def toWord: String
  }

  case class Factorization(
    components: Option[(Prefix, Suffix)]
  ) extends Wordable:
    override def toWord: String =
      components map { case (prefix, suffix) => 
        sandwich(prefix.toWord, suffix.toWord)
      } getOrElse ""
  
  case class Prefix(
    prefix : Factorization,
    leftEnd : Char,
  ) extends Wordable:
    override def toWord: String =
      prefix.toWord :+ leftEnd

  object Prefix:
    def apply(word: String): Prefix =
      val (prefix, letter) = leftSegment(word)
      Prefix(Factorization(prefix), letter)

  case class Suffix(
    rightEnd : Char,
    suffix : Factorization
  ) extends Wordable:
    override def toWord: String =
      rightEnd +: suffix.toWord

  object Suffix:
    def apply(word: String): Suffix =
      val (xiffus, letter) = leftSegment(word.reverse)
      Suffix(letter, Factorization(xiffus.reverse))

  object Factorization:
    def apply(
      word: String,
    ): Factorization =
      Factorization(
        if word.isEmpty() then
          None
        else
          Some(
              Prefix(word),
              Suffix(word)
            )
      )

  class AlphabetContext(
    alphabet: String
  ) extends Traversable[String]:
    class WordNode(
      val canonical: String
    ) extends Iterable[String]:
      lazy val continuation: Map[Char, WordNode] =
        alphabet.map { letter =>
          letter -> {
          val next: String = (canonical :+ letter).canonical
          if (next.length > canonical.length)
            WordNode(next)
          else
            lookup(next)
          }
        }.toMap

      override def iterator: Iterator[String] =
        Iterator(canonical) ++
          alphabet.iterator.flatMap { letter =>
            val next = continuation(letter)
            if (next.canonical.length > canonical.length)
              next.iterator
            else
              Iterator.empty
          }
          
      def lookup(seq: String): WordNode =
        seq.foldLeft[WordNode](root) { (node, letter) =>
          node.continuation(letter)
        }
      
    val root: WordNode =
      WordNode("")

    override def iterator: Iterator[String] =
      root.iterator
