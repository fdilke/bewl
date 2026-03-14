package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.ToposOfMonoidActions
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl.helper.StandardSymbols.{i, x, y}
import org.scalatest.freespec.AnyFreeSpec

import scala.language.reflectiveCalls

class FindGeneratorsTest extends AnyFreeSpec {

  import monoidOf3.regularAction

  private val finder =
    FiniteSets.FindGenerators.forMonoid(
      monoidOf3
    )

  import finder.apply

  private val regularAnalysis =
    apply(
      regularAction
    )

  private val actionTopos =
    ToposOfMonoidActions.of(monoidOf3, FiniteSets.DefaultMonoidAssistant)

  "The generator finder" - {
    "can build up a set of maximal cyclic subalgebras for a monoid action" - {
      import regularAnalysis.initialCyclics

      "which are initially empty" in {

        initialCyclics.cyclics shouldBe empty

        initialCyclics.contains(i) shouldBe false

        initialCyclics.transversal shouldBe empty
      }

      "which can be added to, filtering out any eclipsed cyclics" in {
        val cyclics_I =
          initialCyclics + i

        cyclics_I.cyclics should have size 1

        val cyclics_X_Y =
          initialCyclics + x + y

        cyclics_X_Y.cyclics should have size 1

        val theCyclics =
          (cyclics_X_Y + i).cyclics

        theCyclics should have size 1
        theCyclics.head.generator shouldEqual i
      }

      "which can be used to build up the complete set" in {
        val allMaxCyclics =
          Seq(i, x, y).foldLeft(
            initialCyclics
          ) {
            _ << _
          }

        val theCyclics =
          allMaxCyclics.cyclics

        theCyclics should have size 1
        theCyclics.head.generator shouldEqual i
      }

      "as expected for the empty action" in {
        apply(
          monoidOf3.voidAction
        ).generators shouldBe empty
      }

      "as expected for a non-cyclic action" in {
        val regularSquared =
          actionTopos.unwrap(
            actionTopos
              .makeDot(
                regularAction
              )
              .squared
          )
        apply(
          regularSquared
        ).generators should have size 7
      }

      "as expected for another non-cyclic action" in {
        val theOmega =
          actionTopos.unwrap(
            actionTopos.omega
          )
        apply(
          theOmega
        ).generators should have size 2
      }
    }

    "can extract a set of generators for a monoid action" in {
      apply(
        monoidOf3.regularAction
      ).generators shouldBe Seq(i)
    }
  }
}
