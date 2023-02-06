package com.fdilke.bewl2.algebra.constructions

import scala.annotation.tailrec
import com.fdilke.utility.IterateToFixed
import GreenRees.*
import java.math.BigInteger
import scala.util.Random

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

  extension[H](set: Set[Seq[H]])
    def *(set2: Set[Seq[H]]): Set[Seq[H]] =
      set.flatMap { seq => 
        set.map { _ * seq }
      }

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

  def stochasticLongest(n: Int): Int =
    if (n == 0)
      0
    else
      val random = new Random
      var longest: Int = -1
      for { i <- 0 to 1000 } {
        var word: Seq[Int] = Seq.empty // 0 until n
        for { j <- 0 to (n*100) } {
          word = Seq(random.nextInt(n)) * word
          longest = Math.max(longest, word.size)
        }
      }
      longest

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
      rightSegment.canonical
    ).recombine


