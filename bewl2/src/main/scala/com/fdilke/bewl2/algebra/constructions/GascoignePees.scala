package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec
import com.fdilke.utility.IterateToFixed
import GascoignePees.*
import java.math.BigInteger
import scala.util.Random

// See green-rees-theory.md

object GascoignePees:

  extension[H](word: Seq[H])
    def factorize: Factoring[H] =
      val allLetters: Set[H] = word.toSet
      val (leftSeg: Seq[H], leftLetter: H) = word.leftSegment(allLetters)
      val (rightSegR, rightLetter): (Seq[H], H) = word.reverse.leftSegment(allLetters)
      Factoring(leftSeg, leftLetter, rightLetter, rightSegR.reverse)
      
    def leftSegment: (Seq[H], H) =
      leftSegment(word.toSet)

    def leftSegment(
      allLetters: Set[H]
    ): (Seq[H], H) =
      if word.isEmpty then
        throw new IllegalArgumentException("can't extract segment from empty word")
      extractLeftSegmentHelper(word, Seq.empty, word.toSet)

    def <~(word2: Seq[H]): Boolean =
      (word ++ word2) =!= word2

    def R(word2: Seq[H]): Boolean =
      (word <~ word2) && (word2 <~ word)

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

    def slowcan(using Ordering[H]): Seq[H] =
      val letters: Seq[H] = word.distinct.sorted
      IterateToFixed(
        Seq.empty[H]
      ) { can =>
        if word =!= can then
          can
        else
          letters map { letter =>
            can :+ letter
          } find { newCan =>
            println("trying " + newCan + " for word: " + word)
            println("(newCan <~ word) = " + (newCan <~ word))
            println("!(newCan =!= can) = " + !(newCan =!= can))
            // println("!(newCan <~ can) = " + !(newCan <~ can))
            println(s"that's !($newCan =!= $can)")
            (newCan <~ word) && !(newCan =!= can) // && !(newCan <~ can))
          } match {
            case None => throw new IllegalArgumentException(
              s"""can't extend "$can" in context of "$word""""
            )
            case Some(newCan) =>
              if (newCan.size > word.size + 2)
                throw new IllegalArgumentException(
                  "this is insane: word = " + word
                )
              newCan
          }
        }

    def *(word2: Seq[H]): Seq[H] =
      (word ++ word2).canonical

  extension[H](set: Set[Seq[H]])
    def *(set2: Set[Seq[H]]): Set[Seq[H]] =
      set.flatMap { seq => 
        set.map { _ * seq }
      }

  // extension(word: String)
  //   def <~(word2: String): Boolean =
  //     word.toSeq <~ word2.toSeq

  // extension[H](letter: H)
  //   def *(seq: Seq[H]): Seq[H] =
  //     Seq(letter) * seq

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
    
  def enumerateWordsBySquares[H](
    letters: Seq[H]
  ): Set[Seq[H]] =
    IterateToFixed[Set[Seq[H]]](
      (letters.map { Seq(_) } :+ Seq.empty).toSet
    ) { set => // appallingly inefficient?
      set * set
    }

  def enumerateWordsByLetters[H](
    letters: Seq[H]
  ): Set[Seq[H]] =
    IterateToFixed[Set[Seq[H]]](
      Set(Seq.empty[H])
    ) { set => // appallingly inefficient?
      set union (for {
        letter <- letters.toSet
        seq <- set
      } yield {
        Seq(letter) * seq
      })
    }
    
  def enumerateWordsByTree[H](
    letters: Seq[H]
  ): Set[Seq[H]] =
    AlphabetContext[H](letters).toSet
    
  def orderFree(n: Int): BigInt =
    (for { k <- 0 to n } yield {
      (for { i <- 1 to k } yield {
        BigInt(k - i + 1).pow(BigInt(2).pow(i).toInt)
      }).product * combinations(n, k)
    }).sum

  def permutations(n: Int): BigInt =  
    (1 to n).map(BigInt(_)).foldLeft(BigInt(1))(_ * _)

  def combinations(n: Int, k: Int): BigInt =
    permutations(n) / (permutations(k) * permutations(n - k))    

  def longestLength(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case 2 => 3
      case _ => 2 * longestLength(n-1) + 2
    }

  def longest0[H](letters: Seq[H]): Seq[H] =
    letters match {
      case Seq() => Seq()
      case Seq(a) => Seq(a)
      case Seq(a, b) => Seq(a, b, a)
      case _ => 
        val numLetters = letters.size
        val a = letters.head
        val z = letters.last
        longest0(letters.slice(0, numLetters-1)) ++ Seq(z, a) ++
          longest0(letters.slice(1, numLetters))
    }

  def longestWord[H](letters: Seq[H]): Seq[H] =
    letters match {
      case Seq() => Seq()
      case Seq(a) => Seq(a)
      case Seq(a, b) => Seq(a, b, a)
      case _ => 
        val numLetters = letters.size
        val a = letters.head
        val z = letters.last
        longestWord(letters.slice(0, numLetters-1)) ++ Seq(z, a) ++
          longestWord(interchange(letters.slice(1, numLetters)))
    }

  private def interchange[H](word: Seq[H]): Seq[H] =
    val len = word.size
    if (len <= 2)
      word
    else if (len == 4)
      word
    else
      word.slice(0, len-2) :+ word(len-1) :+ word(len-2)

  private def interchange0[H](word: Seq[H]): Seq[H] =
    if (word.size <= 2)
      word
    else
      word(0) +: word(2) +: word(1) +: word.slice(3, word.size)

  class AlphabetContext[H](
    alphabet: Seq[H]
  ) extends Traversable[Seq[H]]:
    class WordNode(
      val canonical: Seq[H]
    ) extends Iterable[Seq[H]]:
      lazy val continuation: Map[H, WordNode] =
        alphabet.map { letter =>
          letter -> {
          val next: Seq[H] = (canonical :+ letter).canonical
          if (next.length > canonical.length)
            WordNode(next)
          else
            lookup(next)
          }
        }.toMap

      override def iterator: Iterator[Seq[H]] =
        Iterator(canonical) ++
          alphabet.iterator.flatMap { letter =>
            val next = continuation(letter)
            if (next.canonical.length > canonical.length)
              next.iterator
            else
              Iterator.empty
          }
          
      def lookup(seq: Seq[H]): WordNode =
        seq.foldLeft[WordNode](root) { (node, letter) =>
          node.continuation(letter)
        }
      
    val root: WordNode =
      WordNode(Seq.empty)

    override def iterator: Iterator[Seq[H]] =
      root.iterator
  
case class Factoring[H](
  leftSegment: Seq[H],
  leftLetter: H,
  rightLetter: H,
  rightSegment: Seq[H]
):
  def =!=(other: Factoring[H]): Boolean =
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
    Factoring(
      leftSegment.canonical,
      leftLetter,
      rightLetter,
      rightSegment.canonical
    ).recombine


