package com.fdilke.bewl2.topos

import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*

import scala.Function.tupled
import scala.language.postfixOps
import com.fdilke.bewl2.utility.RichFunSuite

import scala.collection.immutable.Set
import com.fdilke.bewl2.sets.Sets
import Sets._

class QuantifiersSpec extends RichFunSuite:

  test("Universal quantifiers work as expected for sets") {
    withDot(Set[Int](1, 2, 3)) {
      ∀[Int] isArrow Map[Map[Int, Boolean], Boolean](
        Map( 1 -> false, 2 -> false, 3 -> false ) -> false,
        Map( 1 -> true,  2 -> false, 3 -> false ) -> false,
        Map( 1 -> false, 2 -> true,  3 -> false ) -> false,
        Map( 1 -> false, 2 -> false, 3 -> true  ) -> false,
        Map( 1 -> true,  2 -> true,  3 -> false ) -> false,
        Map( 1 -> true,  2 -> false, 3 -> true  ) -> false,
        Map( 1 -> false, 2 -> true,  3 -> true  ) -> false,
        Map( 1 -> true,  2 -> true,  3 -> true  ) -> true
      )
      withDot(Set[String]("123", "12")) {
        val hee: String ~> Boolean = ∀[String, Int] { case (text, i) =>
          text contains s"$i"
        }
        hee isArrow Map[String, Boolean](
          "123" -> true,
          "12"  -> false
        )
      }
    }
  }

  test("Existential quantifiers work as expected for sets") {
    withDot(Set[Int](1, 2, 3)) {
      ∃[Int] isArrow Map[Map[Int, Boolean], Boolean](
        Map( 1 -> false, 2 -> false, 3 -> false ) -> false,
        Map( 1 -> true,  2 -> false, 3 -> false ) -> true,
        Map( 1 -> false, 2 -> true,  3 -> false ) -> true,
        Map( 1 -> false, 2 -> false, 3 -> true  ) -> true,
        Map( 1 -> true,  2 -> true,  3 -> false ) -> true,
        Map( 1 -> true,  2 -> false, 3 -> true  ) -> true,
        Map( 1 -> false, 2 -> true,  3 -> true  ) -> true,
        Map( 1 -> true,  2 -> true,  3 -> true  ) -> true
      )
      withDot(Set[String]("123", "12", "1a", "bubb")) {
        val hee: String ~> Boolean = ∃[String, Int] { case (text, i) =>
          text contains s"$i"
        }
        hee isArrow Map[String, Boolean](
          "123" -> true,
          "12"   -> true,
          "1a"   -> true,
          "bubb" -> false
        )
      }
    }
  }
