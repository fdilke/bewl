package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.VanillaPitcher
import com.fdilke.bewl2.topology
import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.topology.EmptyEnumeration._
import com.fdilke.bewl2.topology.Hausdorff._
import com.fdilke.bewl2.topology.VanillaPitcherPredicateSolver.{solveFunction, solveMap}
import com.fdilke.bewl2.topology.StrontiumDogEnumeration._
import com.fdilke.bewl2.topology.WeekdayEnumeration._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.Iterator.iterate
import scala.language.postfixOps

class VanillaPitcherPredicateSolverTest extends AnyFunSpec {
  ignore("The predicate solver can act on maps") {
    it("detects immediate success for the rubberstamp predicate on an empty map") {
      def rubberstampPredicate(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        true

      solveMap(
        rubberstampPredicate
      ) shouldBe
        Some(Map.empty)
    }

    it("detects immediate failure for the Dr No predicate on an empty map") {
      def drNoPredicate(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        false

      solveMap(
        drNoPredicate
      ) shouldBe
        None
    }

    it("solves in one go for the Johnny-on-Wednesday predicate on an empty map") {
      def johnnyOnWed(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        calendar.tail.tail.head == Johnny

      solveMap(
        johnnyOnWed
      ) shouldBe Some(
        Map(
          new Key(Wednesday) -> Johnny
        )
      )
    }

    it("solves for the Tuesday-same-as-Wednesday predicate on an empty map") {
      def tueSameWed(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        calendar.head == calendar.tail.head

      solveFunction(
        tueSameWed
      ) match {
        case Some(function) =>
          function.head shouldBe function.tail.head
        case _ =>
          fail("Solver failed")
      }
    }
    it("finds effective solutions for various predicates") {
      val samplePredicates: Seq[VanillaPitcher[StrontiumDog] => Boolean] =
        Seq(
          f => f.head == f.tail.head && (f.tail.tail.head.toString startsWith "The"),
          f => f.head != f.tail.tail.head,
          f => Set(f.head, f.tail.head, f.tail.tail.head).size == 2
        )
      samplePredicates.foreach { pred =>
        solveFunction(pred) match {
          case Some(fn) =>
            pred(fn) shouldBe true
          case None =>
            fail("Solver failed")
        }
      }
    }
    it("can diagnose when there is no solution for a predicate") {
      val samplePredicates: Seq[
        VanillaPitcher[StrontiumDog] => Boolean
      ] = Seq(
        f => f.tail.head.toString startsWith "Stix",
        f => {
          val h = (iterate(f) {
            _ tail
          })
          val siz = (h map {
            _ head
          } take 10).size
          siz > NUM_STRONTIES
        },
        f => f.tail.head.id > NUM_STRONTIES
      )
      samplePredicates.foreach { pred => solveFunction(pred) shouldBe None }
    }
    it("returns a function that can be evaluated on all arguments") {
      solveFunction[StrontiumDog] { dogOfTheDay => dogOfTheDay.tail.tail.head == Johnny } match {
        case None => fail("no solution found")
        case Some(dogOfTheDay) =>
          StrontiumDogEnumeration.values should contain(dogOfTheDay.head)
          StrontiumDogEnumeration.values should contain(dogOfTheDay.tail.tail.tail.head)
      }
    }
  }
}
