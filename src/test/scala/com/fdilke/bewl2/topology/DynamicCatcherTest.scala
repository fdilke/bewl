package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Compact._
import Hausdorff._

import StrontiumDogEnumeration._
import WeekdayEnumeration._

class DynamicCatcherTest extends AnyFunSpec {
  describe("The predicate solver can act on maps") {
    it("detects immediate success for the rubberstamp predicate on an empty map") {
      def rubberstampPredicate(calendar: Weekday => StrontiumDog): Boolean =
        true

      new HausdorffToCompactPredicateSolver(
        rubberstampPredicate
      ).tryMap(
        Map.empty
      ) shouldBe ThatWorks
    }
  }
}
