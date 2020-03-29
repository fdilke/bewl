package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Compact._
import Hausdorff._
import StrontiumDogEnumeration._
import WeekdayEnumeration._
import EmptyEnumeration._
import com.fdilke.bewl2.topology.HausdorffToCompactPredicateSolver.solveMap

class HausdorffToCompactPredicateSolverTest extends AnyFunSpec {
  describe("The predicate solver can act on maps") {
    it("detects immediate success for the rubberstamp predicate on an empty map") {
      def rubberstampPredicate(calendar: Weekday => StrontiumDog): Boolean =
        true

      solveMap(
        rubberstampPredicate
      ) shouldBe
        Some(Map.empty)
    }

    it("detects immediate failure for the Dr No predicate on an empty map") {
      def drNoPredicate(calendar: Weekday => StrontiumDog): Boolean =
        false

      solveMap(
        drNoPredicate
      ) shouldBe
        None
    }

    it("solves in one go for the Johnny-on-Wednesday predicate on an empty map") {
      def johnnyOnWed(calendar: Weekday => StrontiumDog): Boolean =
        calendar(Wednesday) == Johnny

      solveMap(
        johnnyOnWed
      ) shouldBe Some(
        Map(
          Wednesday -> Johnny
        )
      )
    }

    it("solves for the Tuesday-same-as-Wednesday predicate on an empty map") {
      def tueSameWed(calendar: Weekday => StrontiumDog): Boolean =
        calendar(Tuesday) == calendar(Wednesday)

      solveMap(
        tueSameWed
      ) match {
        case Some(map) =>
          map.keySet shouldBe Set(Tuesday, Wednesday)
          map.values.toSet.size shouldBe 1
        case other => fail("Solver failed")
      }
    }
    it("finds effective solutions for various predicates") {
      val samplePredicates: Seq[(Weekday => StrontiumDog) => Boolean] =
        Seq(
          f => f(Friday) == f(Monday) && (f(Wednesday).toString startsWith "The"),
          f => f(Friday) != f(Monday),
          f => Set(f(Monday), f(Tuesday), f(Wednesday)).size == 2
        )
      samplePredicates.foreach { pred =>
        solveMap(pred) match {
          case Some(map) =>
            pred(map) shouldBe true
          case other =>
            fail("Solver failed")
        }
      }
    }
    it("can diagnose when there is no solution for a predicate") {
      val samplePredicates: Seq[(Weekday => StrontiumDog) => Boolean] =
        Seq(
          f => f(Wednesday).toString startsWith "Stix",
          f => WeekdayEnumeration.values.map(f).size > StrontiumDogEnumeration.values.size,
          f => forAll[Weekday] { wd1 =>
            forAll[Weekday] { wd2 =>
              (f(wd1) != f(wd2)) || (wd1 == wd2)
            }
          }
        )
      samplePredicates.foreach { pred =>
        solveMap(pred) shouldBe None
      }
    }
    // making this part of the 'solve for function' logic as it doesn't really apply to maps
    ignore("can diagnose when there is no solution for a predicate because target is empty") {
      def rubberstamp(youWish: StrontiumDog => Impossibility): Boolean =
        true

      solveMap(rubberstamp) shouldBe None
    }
    // Can't do this without compactness or some other condition on the source
    ignore("can diagnose when there is trivially a solution for a predicate because source and target are both empty") {
      def rubberstamp(youWish: Impossibility => Impossibility): Boolean =
        true

      solveMap(rubberstamp) shouldBe
        Some(Map.empty)
    }
  }
}
