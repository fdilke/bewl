package com.fdilke.bewl2.apps
import com.fdilke.bewl2.algebra.constructions.GreenRees._
import com.fdilke.utility.TimeIt

object LittleProg extends App:
  val words: Set[String] =
    enumerateCanonicalsSlow("abcd").toSet
  for { word <- words }   {
    print(word + "\t")
  }
  println("\n" + words.size + "total")

  // for { n <- 1 to 30 } {
  //   val size = orderFree(n)
  //   val numDigitsRoot = size.toString().length() / n
  //   println("" + n + "\t" + numDigitsRoot)
  // }


object EarliestLongestWordOfDeg4 extends App:
  val degree = 4
  val letters: String = alphabetOfSize(degree)
  val words: Set[String] =
      enumerateCanonicalsSlow(letters).toSet
  val maxLength: Int =
      words.map { _.length }.max
  val earliest = words.toSeq.filter { _.length == maxLength }.sorted.head
  println("earliest = " + earliest)

object CheckCanonicalTimings extends App:
  for (degree <- 0 to 4) {
    val letters: String = alphabetOfSize(degree)
    val (canonSlow, canonSlowTime) = TimeIt { enumerateCanonicalsSlow(letters).size }
    val (canon, canonTime) = TimeIt { enumerateCanonicals(letters).size }
    assert { canon == canonSlow }
    println(s"$degree\t${canon}\t${canonTime}\t${canonSlowTime}")
  }

object ShowEggboxDegree3 extends App:
  val degree = 3
  val letters = alphabetOfSize(degree)
  val prefixes = prefixesUsingAll(letters)
  val suffixes = suffixesUsingAll(letters)
  val padLength = longestLength(degree) + 1
  def pad(text: String) =
    text.padTo(padLength, ' ')

  print(pad("R \\ L"))
  for { suff <- suffixes} {
    print(pad(suff.toWord))
  }
  println()
  for { pre <- prefixes} {
    print(pad(pre.toWord))
    for { suff <- suffixes} {
      val product = pre.toWord * suff.toWord
      print(pad(product))
    }
    println()
  }
