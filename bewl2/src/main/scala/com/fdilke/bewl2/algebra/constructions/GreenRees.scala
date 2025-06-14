package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec
import com.fdilke.utility.Shortcuts.*
import com.fdilke.bewl2.sets.SetsUtilities._

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

    infix def R(word2: String): Boolean =
      Factorization(word).optionalPrefix == Factorization(word2).optionalPrefix

    infix def L(word2: String): Boolean =
      Factorization(word).optionalSuffix == Factorization(word2).optionalSuffix

    def canonical: String =
      Factorization(word).toWord

    infix def isCanonical: Boolean =
      word.canonical == word

    infix def *(word2: String) =
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

  def alphabetOfSize(numLetters: Int) =
    ((0 until numLetters) map { i =>
      (i + 'a'.toInt).toChar
    }).string

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
      combinations(n, k) * eggBoxSize(k).pow(2)
    }).sum

  // The number of R-classes in the D-class of words including all letters from an alphabet of size n
  def eggBoxSize(n: Int): BigInt =
    n match {
      case 0 => BigInt(1)
      case _ => n * eggBoxSize(n - 1).pow(2)
    }

  def longestLength(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case 2 => 3
      case _ => 2 * longestLength(n-1) + 2
    }

  def longestWord(numLetters: Int): String =
    if (numLetters == 0)
      ""
    else
      val prevLongest = longestWord(numLetters - 1)
      val requiredLength = longestLength(numLetters)
      val letters: String = alphabetOfSize(numLetters)
      def explore(prefix: String) : Option[String] =
        if prefix.length() == requiredLength then
          Some(prefix)
        else
          letters.iterator.map { letter =>
            prefix :+ letter
          }.filter { 
            _.isCanonical
          }.map(explore).find {
            case None => false
            case Some(solution) => true
          }.flatten

      explore(prevLongest) match {
          case None => throw IllegalArgumentException("aarg")
          case Some(candidate) => candidate
      }


  private def remapString(
    calculated: String,
    source: String,
    target: String
  ): String =
    calculated.map { char =>
      val index: Int = source.indexOf(char)
      target(index)
    }    

  def prefixesUsingAll(letters: String): Iterable[Prefix] =
    if (letters.isEmpty)
      Iterable.empty
    else for {
      letter <- (letters : Iterable[Char])
      without = letters filterNot { _ == letter }
      factorization <- facsUsingAll(without)
    } yield
      Prefix(factorization, letter)

  def suffixesUsingAll(letters: String): Iterable[Suffix] =
    if (letters.isEmpty)
      Iterable.empty
    else for {
      letter <- (letters : Iterable[Char])
      without = letters filterNot { _ == letter }
      factorization <- facsUsingAll(without)
    } yield
      Suffix(letter, factorization)

  def facsUsingAll(letters: String): Iterable[Factorization] =
    if (letters.isEmpty())
      Iterable(
        Factorization.empty
      )
    else for {
      prefix <- prefixesUsingAll(letters)
      suffix <- suffixesUsingAll(letters)
    } yield
      Factorization(Some((prefix, suffix)))

  def enumerateCanonicals(
    letters: String
  ): Iterable[String] =
    for {
      alphabet <- subsetsOfString(letters)
      fac <- facsUsingAll(alphabet)
    } yield
      fac.toWord

  def enumerateCanonicalsSlow(
    letters: String
  ): Iterable[String] =
    AlphabetContext(letters)
    
  trait HasLazyWord {
    lazy val toWord: String
  }

  case class Factorization(
    components: Option[(Prefix, Suffix)]
  ) extends HasLazyWord:
    override lazy val toWord: String =
      components map { case (prefix, suffix) => 
        sandwich(prefix.toWord, suffix.toWord)
      } getOrElse ""
    def optionalPrefix: Option[Prefix] =
      components map { _._1 }
    def optionalSuffix: Option[Suffix] =
      components map { _._2 }
    override def toString: String =
      components match {
        case None => "0"
        case Some(p -> s) => s"(${p.toWord}:${s.toWord})"
      }

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
    val empty: Factorization =
      Factorization(None)
  
  case class Prefix(
    prefix : Factorization,
    leftEnd : Char,
  ) extends HasLazyWord:
    override lazy val toWord: String =
      prefix.toWord :+ leftEnd

  object Prefix:
    def apply(word: String): Prefix =
      val (prefix, letter) = leftSegment(word)
      Prefix(Factorization(prefix), letter)

  case class Suffix(
    rightEnd : Char,
    suffix : Factorization
  ) extends HasLazyWord:
    override lazy val toWord: String =
      rightEnd +: suffix.toWord

  object Suffix:
    def apply(word: String): Suffix =
      val (xiffus, letter) = leftSegment(word.reverse)
      Suffix(letter, Factorization(xiffus.reverse))

  class AlphabetContext(
    alphabet: String
  ) extends Iterable[String]:
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
