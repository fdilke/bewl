package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Compact._
import Hausdorff._
import StrontiumDogEnumeration._
import WeekdayEnumeration._
import EmptyEnumeration._
import com.fdilke.bewl2.topology.HausdorffToCompactPredicateSolver.{solveFunction, solveMap}

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
          new Key(Wednesday) -> Johnny
        )
      )
    }

    it("solves for the Tuesday-same-as-Wednesday predicate on an empty map") {
      def tueSameWed(calendar: Weekday => StrontiumDog): Boolean =
        calendar(Tuesday) == calendar(Wednesday)

      solveFunction(
        tueSameWed
      ) match {
        case Some(function) =>
          function(Tuesday) shouldBe function(Wednesday)
        case _ =>
          fail("Solver failed")
      }
    }
    it("finds effective solutions for various predicates") {
      val samplePredicates: Seq[(Weekday => StrontiumDog) => Boolean] =
        Seq(
          f => f(Friday) == f(Monday) && (f(Wednesday).toString.startsWith("The")),
          f => f(Friday) != f(Monday),
          f => Set(f(Monday), f(Tuesday), f(Wednesday)).size == 2
        )
      samplePredicates.foreach { pred =>
        solveFunction(pred) match {
          case Some(fn) =>
            pred(fn) shouldBe true
          case other =>
            fail("Solver failed")
        }
      }
    }
    it("can diagnose when there is no solution for a predicate") {
      val samplePredicates: Seq[(Weekday => StrontiumDog) => Boolean] =
        Seq(
          f => f(Wednesday).toString.startsWith("Stix"),
          f => WeekdayEnumeration.values.map(f).size > StrontiumDogEnumeration.values.size,
          f => forAll[Weekday](wd1 => forAll[Weekday](wd2 => (f(wd1) != f(wd2)) || (wd1 == wd2)))
        )
      samplePredicates.foreach(pred => solveFunction(pred) shouldBe None)
    }
    it("returns a function that can be evaluated on all arguments") {
      solveFunction[Int, StrontiumDog](dogOfTheDay => dogOfTheDay(2) == Johnny) match {
        case None => fail("no solution found")
        case Some(dogOfTheDay) =>
          StrontiumDogEnumeration.values should contain(dogOfTheDay(22))
          StrontiumDogEnumeration.values should contain(dogOfTheDay(-842))
      }
    }
    // need to decide what to do about the case when target is empty
    // logically we can return a function which, if ever called, will blow up
    // but if source was compact, we could tell if it was empty and if so return a
    // function we know will never be called (because no one can call it with a valid arg)
    // for now, just do what's easiest and don't even enforce this in tests

    // making this part of the 'solve for function' logic as it doesn't really apply to maps
    ignore("can diagnose when there is no solution for a predicate because target is empty") {
      def rubberstamp(youWish: StrontiumDog => Impossibility): Boolean =
        true

      solveFunction(rubberstamp) shouldBe None
    }
    // Can't do this without compactness or some other condition on the source
    ignore(
      "can diagnose when there is trivially a solution for a predicate because source and target are both empty"
    ) {
      def rubberstamp(youWish: Impossibility => Impossibility): Boolean =
        true

      solveMap(rubberstamp) shouldBe
        Some(Map.empty)
    }
  }
}
