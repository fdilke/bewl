package com.fdilke.bewl2.apps

import com.fdilke.bewl2.algebra.constructions.GascoignePees.*
import com.fdilke.utility.Shortcuts.*
import java.util.concurrent.atomic.AtomicBoolean

object LittleProg extends App:
  val words: Set[String] =
    enumerateWordsByTree("abcd").map { _.string }
  for { word <- words }   {
    print(word + "\t")
  }
  println("\n" + words.size + "total")

  // for { n <- 1 to 30 } {
  //   val size = orderFree(n)
  //   val numDigitsRoot = size.toString().length() / n
  //   println("" + n + "\t" + numDigitsRoot)
  // }

  // for (n <- 0 to 4) {
  //   val letters: String = "abcde".slice(0, n)
  //   val words: Set[String] =
  //     enumerateWordsByTree(letters).map { _.string }
  //     val maxLength: Int =
  //       words.map { _.length }.max
  //       val longest = 
  //         words.filter { _.length == maxLength }
  //         println(s"$n\t$maxLength\t${longest.size}\t${longest.head}")
  //       }

  for (n <- 0 to 10) {
    println("" + n + "\t" + longestLength(n))
  }

object IsRACongruence extends App:
  val degree = 3
  val letters: String = "abcde".slice(0, degree)
  val words: Set[String] =
      enumerateWordsByTree(letters).map { _.string }
  val broken= new AtomicBoolean
  for {
    word <- words
    word2 <- words
  } {
    if (word R word2) {
      for { letter <- letters } {
        if ((word + letter).canonical != (word2 + letter).canonical) {
          println(s"broken on $word, $word2, $letter")
          broken.set(true)
        }
      }
    }
  }
  if (broken.get)
    println("Broken")
  else
    println("All ok")

object EarliestLongestWordOfDeg4 extends App:
  val degree = 4
  val letters: String = "abcde".slice(0, degree)
  val words: Set[String] =
      enumerateWordsByTree(letters).map { _.string }
  val maxLength: Int =
      words.map { _.length }.max
  val earliest = words.toSeq.filter { _.length == maxLength }.sorted.head
  println("earliest = " + earliest)

object AnalyzeLongWords extends App:
  def show(letters: String): Unit =
    val word = longest0(letters).string
    val len = word.length()
    val wordC = word.canonical.string
    val lenC = wordC.length
    println(s"""longest0($letters) = $word($len) => $wordC($lenC)""")

  def show0(letters: String): Unit =
    val word = longestWord(letters).string
    val len = word.length()
    val wordC = word.canonical.string
    val lenC = wordC.length
    println(s"""longestWord($letters) = $word($len) => $wordC($lenC)""")

  show("abcd")
  show("bcd")
  show("bdc")
  println("")
  show0("abcd")
  show0("bcd")
  show0("bdc")

object FindPermutation extends App:
  val degree = 5
  val letters: String = "abcde".slice(0, degree)
  val a_to_y: String = letters.slice(0, degree-1)
  val b_to_z: String = letters.slice(1, degree)
  val prefix = longestWord(a_to_y) :+ letters.last :+ letters.head
  val candidates = b_to_z.permutations.toSeq.map { p =>
    p -> (prefix ++ longestWord(p)).string
  }.filter { case (p, candidate) =>
    val collapses: Boolean = 
      ( candidate.canonical.size < candidate.size)
    !collapses
  }
  val earliest = candidates.sortBy { (p, word) => word }.head
  println("candidates: " + candidates)
  println("earliest: " + earliest)

  

