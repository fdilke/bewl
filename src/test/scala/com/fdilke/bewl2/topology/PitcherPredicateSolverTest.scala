package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.VanillaPitcher
import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.topology.EmptyEnumeration._
import com.fdilke.bewl2.topology.StrontiumDogEnumeration._
import com.fdilke.bewl2.topology.PitcherPredicateSolver.{solvePitcher, solveSeq}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.Iterator.iterate
import scala.language.postfixOps

class PitcherPredicateSolverTest extends AnyFunSpec {
  describe("The predicate solver can act on maps") {
    it("detects immediate success for the rubberstamp predicate on an empty map") {
      def rubberstampPredicate(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        true

      solveSeq(
        rubberstampPredicate
      ) shouldBe
        Some(Seq.empty)
    }

    it("detects immediate failure for the Dr No predicate") {
      def drNoPredicate(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        false

      solveSeq(
        drNoPredicate
      ) shouldBe
        None
    }

    it("solves in one go for the Johnny-as-head predicate") {
      def johnnyOnWed(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        calendar.tail.tail.head == Wulf

      solveSeq(
        johnnyOnWed
      ) shouldBe Some(
        Seq(Johnny, Johnny, Wulf)
      )
    }

    it("solves for the Tuesday-same-as-Wednesday predicate on an empty map") {
      def tueSameWed(calendar: VanillaPitcher[StrontiumDog]): Boolean =
        calendar.head == calendar.tail.head

      solvePitcher(
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
        solvePitcher(pred) match {
          case Some(pitcher) =>
            pred(pitcher) shouldBe true
          case None =>
            fail("Solver failed")
        }
      }
    }
    it("can diagnose when there is no solution for a predicate") {
      def first10stronts(pitcher: VanillaPitcher[StrontiumDog]): Set[StrontiumDog] =
        (iterate(pitcher) {
          _ tail
        } map {
          _ head
        } take 10).toSet

      val samplePredicates: Seq[
        VanillaPitcher[StrontiumDog] => Boolean
      ] = Seq(
        f => f.tail.head.toString startsWith "Stix",
        f => first10stronts(f).size > NUM_STRONTIES,
        f => f.tail.head.id > NUM_STRONTIES
      )
      samplePredicates.foreach { pred => solvePitcher(pred) shouldBe None }
    }
    it("returns a function that can be evaluated on all arguments") {
      solvePitcher[StrontiumDog, VanillaPitcher] { dogOfTheDay =>
        dogOfTheDay.tail.tail.head == Johnny
      } match {
        case None => fail("no solution found")
        case Some(dogOfTheDay) =>
          StrontiumDogEnumeration.values should contain(dogOfTheDay.head)
          StrontiumDogEnumeration.values should contain(dogOfTheDay.tail.tail.tail.head)
      }
    }
    it("can generate a seq, but not a pitcher for uninhabited types") {
      def rubberstampPredicate(calendar: VanillaPitcher[Impossibility]): Boolean =
        true
      solveSeq[Impossibility, VanillaPitcher](rubberstampPredicate) shouldBe Some(Seq.empty)
      solvePitcher[Impossibility, VanillaPitcher](rubberstampPredicate) shouldBe None
    }
  }
}
