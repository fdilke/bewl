package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Compact._
import Hausdorff._
import StrontiumDogEnumeration._
import WeekdayEnumeration._
import com.fdilke.bewl2.topology.HausdorffToCompactPredicateSolver.solve

class HausdorffToCompactPredicateSolverTest extends AnyFunSpec {
  describe("The predicate solver can act on maps") {
    it("detects immediate success for the rubberstamp predicate on an empty map") {
      def rubberstampPredicate(calendar: Weekday => StrontiumDog): Boolean =
        true

      solve(
        rubberstampPredicate
      ) shouldBe
        Some(Map.empty)
    }

    it("detects immediate failure for the Dr No predicate on an empty map") {
      def drNoPredicate(calendar: Weekday => StrontiumDog): Boolean =
        false

      solve(
        drNoPredicate
      ) shouldBe
        None
    }

    it("solves in one go for the Johnny-on-Wednesday predicate on an empty map") {
      def johnnyOnWed(calendar: Weekday => StrontiumDog): Boolean =
        calendar(Wednesday) == Johnny

      solve(
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

      solve(
          tueSameWed
      ) match {
        case Some(map) =>
          map.keySet shouldBe Set(Tuesday, Wednesday)
          map.values.toSet.size shouldBe 1
        case other => fail("Solver failed")
      }
    }
    it("finds effective solutions for various predicates") {
      val samplePredicates: Seq[ (Weekday => StrontiumDog) => Boolean ] =
        Seq(
          f => f(Friday) == f(Monday) && ( f(Wednesday).toString startsWith "The" ),
          f => f(Friday) != f(Monday),
          f => Set(f(Monday), f(Tuesday), f(Wednesday)).size == 2
        )
      samplePredicates.foreach { pred =>
        solve(pred) match {
          case Some(map) =>
            pred(map) shouldBe true
          case other =>
            fail("Solver failed")
        }
      }
    }
  }
}
