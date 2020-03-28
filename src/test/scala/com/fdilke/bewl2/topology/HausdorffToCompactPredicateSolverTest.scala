package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Compact._
import Hausdorff._

import StrontiumDogEnumeration._
import WeekdayEnumeration._

class HausdorffToCompactPredicateSolverTest extends AnyFunSpec {
  describe("The predicate solver can act on maps") {
    it("detects immediate success for the rubberstamp predicate on an empty map") {
      def rubberstampPredicate(calendar: Weekday => StrontiumDog): Boolean =
        true

      val solver =
        new HausdorffToCompactPredicateSolver(
          rubberstampPredicate
        )

      solver.tryMap(
        Map.empty
      ) shouldBe
        solver.ThatWorks(Map.empty)
    }

    it("detects immediate failure for the Dr No predicate on an empty map") {
      def drNoPredicate(calendar: Weekday => StrontiumDog): Boolean =
        false

      new HausdorffToCompactPredicateSolver(
        drNoPredicate
      ).tryMap(
        Map.empty
      ) shouldBe GivenUp
    }

    it("solves in one go for the Johnny-on-Wednesday predicate on an empty map") {
      def johnnyOnWed(calendar: Weekday => StrontiumDog): Boolean =
        calendar(Wednesday) == Johnny

      val solver =
        new HausdorffToCompactPredicateSolver(
          johnnyOnWed
        )

      solver.tryMap(
        Map.empty
      ) shouldBe
        solver.ThatWorks(Map(
          Wednesday -> Johnny
        ))
    }

    it("solves for the Tuesday-same-as-Wednesday predicate on an empty map") {
      def tueSameWed(calendar: Weekday => StrontiumDog): Boolean =
        calendar(Tuesday) == calendar(Wednesday)

      val solver =
        new HausdorffToCompactPredicateSolver(
          tueSameWed
        )

      solver.tryMap(
        Map.empty
      ) match {
        case solver.ThatWorks(map) =>
          map.keySet shouldBe Set(Tuesday, Wednesday)
          map.values.toSet.size shouldBe 1
        case other => fail("Solver gave wrong result: " + other)
      }
    }
  }
}
