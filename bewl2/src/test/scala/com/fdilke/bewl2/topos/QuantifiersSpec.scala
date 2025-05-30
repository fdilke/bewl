package com.fdilke.bewl2.topos

import com.fdilke.bewl2.sets.SetsWithSlowActions
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*

import scala.Function.tupled
import scala.language.postfixOps
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._

import scala.collection.immutable.Set
import com.fdilke.bewl2.sets.SetsWithSlowActions
import SetsWithSlowActions._

class QuantifiersSpec extends FunSuite:

  test("Universal quantifiers work as expected for sets"):
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
        ∀[String, Int] { case (text, i) =>
          text.contains(s"$i")
        } isArrow Map[String, Boolean](
          "123" -> true,
          "12" -> false
        )
      }
    }

  test("Universal quantifiers (budget boolean version) work as expected for sets"):
    withDot(Set[Int](1, 2, 3)) {
      ∀[Int]{ _ % 2 == 0 } is false
      ∀[Int]{ _ < 5 } is true
    }

  test("Existential quantifiers work as expected for sets"):
    withDot(Set[Int](1, 2, 3)):
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
      withDot(Set[String]("123", "12", "1a", "bubb")):
        ∃[String, Int] { case (text, i) =>
          text.contains(s"$i")
        } isArrow Map[String, Boolean](
          "123" -> true,
          "12"   -> true,
          "1a"   -> true,
          "bubb" -> false
        )

  test("Existential quantifiers (budget boolean version) work as expected for sets"):
    withDot(Set[Int](1, 2, 3)):
      ∃[Int]{ _ % 7 == 5 } is false
      ∃[Int]{ _ < 2 } is true
