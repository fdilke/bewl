package com.fdilke.bewl2.cantorians

import java.util.function.Predicate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CompactExponentialAlgoTest extends AnyFunSpec {

  describe("The compact power algorithm") {
    ignore("can solve simple equations") {
      findsSolution { f => f(3) }
    }
    ignore("can solve a harder case") {
      findsSolution { f => f(2) == f(3) }
    }
    ignore("can tell when there's no solution") {
      verifiesNoSolution { f => false }
    }
    ignore("can tell when there's still no solution") {
      verifiesNoSolution { f =>
        f(3)
        false
      }
    }
  }

  private def verifiesNoSolution(
    criterion: (Int => Boolean) => Boolean
  ): Unit =
    CompactExponentialAlgo.special(criterion) shouldBe None

  private def findsSolution(
    criterion: (Int => Boolean) => Boolean
  ): Unit =
    CompactExponentialAlgo.special(criterion) match {
      case None => fail("Did not find solution")
      case Some(solutionFactory) =>
        val solution: (Int => Boolean) =
          solutionFactory()

        criterion(solution) shouldBe true
    }
}
