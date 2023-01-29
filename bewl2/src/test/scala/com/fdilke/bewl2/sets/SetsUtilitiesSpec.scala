package com.fdilke.bewl2.sets;

import junit.framework.TestCase
import munit.FunSuite
import SetsUtilities.*
import com.fdilke.bewl2.sets.Sets
import Sets.*

import scala.language.postfixOps
import com.fdilke.bewl2.utility.RichFunSuite

import java.util.concurrent.atomic.AtomicBoolean

class SetsUtilitiesSpec extends RichFunSuite:
  test("enumerates all maps between two sets") {
      allMaps(Set(1, 2), Set("a", "b", "c")).map { f =>
          Seq(f(1), f(2))
      }.toSeq is
      Seq(
         Seq("a", "a"),
         Seq("b", "a"),
         Seq("c", "a"),
         Seq("a", "b"),
         Seq("b", "b"),
         Seq("c", "b"),
         Seq("a", "c"),
         Seq("b", "c"),
         Seq("c", "c")
      )
  }

  test("gives sensible results even when the source is empty") {
    allMaps(Set[String](), Set(0)).size is 1
  }

  test("gives sensible results even when the target is empty") {
    allMaps(Set(0), Set()).isEmpty is true
  }

  test("gives sensible results even when both source and target are empty") {
    allMaps(Set(), Set()).size is 1
  }

  test("enumerates n-ary operations: degenerate case of binaries on 1") {
    (allNaryOps(arity = 2, order = 1) map { f =>
      f(0, 0)
    }).toSeq is Seq(0)
  }

  test("enumerates n-ary operations: degenerate case of unaries on 2") {
    (allNaryOps(arity = 1, order = 2) map { f =>
      f(0) -> f(1)
    }).toSeq is
      Seq(
        0 -> 0,
        1 -> 0,
        0 -> 1,
        1 -> 1
      )
  }

  test("enumerates n-ary operations: even more degenerate case of nullaries on 0") {
    allNaryOps(arity = 0, order = 0).toSeq is Seq.empty
  }

  test("enumerates n-ary operations: case of binaries on 2") {
    (allNaryOps(arity = 2, order = 2) map { f =>
      s"${f(0,0)}${f(0,1)}${f(1,0)}${f(1,1)}"
    }).toSet is
      Set(
        "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
        "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"
      )
  }

  test("compact notation for importing enumerations into FinSet") {
    enum Suit:
      case Hearts, Clubs, Spades, Diamonds

    import Suit.*
    val called: AtomicBoolean = AtomicBoolean(false)
    withEnum[Suit] {
      dot[Suit] is Set(Hearts, Clubs, Spades, Diamonds)
      called.set(true)
    }
    called.get() is true
  }

  test("enumerate sequences of a given length on given symbols") {
    sequencesOfLength("", 0).toSeq is Seq[Seq[Char]]("")
    sequencesOfLength("a", 0).toSeq is Seq[Seq[Char]]("")
    sequencesOfLength("ab", 1).toSeq is Seq[Seq[Char]]("a", "b")
    sequencesOfLength("ab", 2).toSeq is Seq[Seq[Char]]("aa", "ba", "ab", "bb")
    sequencesOfLength("abc", 2).toSeq is Seq[Seq[Char]](
      "aa", "ba", "ca", 
      "ab", "bb", "cb", 
      "ac", "bc", "cc"
    )
    sequencesOfLength("ab", 3).toSeq is Seq[Seq[Char]](
      "aaa", "baa", 
      "aba", "bba",
      "aab", "bab",
      "abb", "bbb"
    )
  }