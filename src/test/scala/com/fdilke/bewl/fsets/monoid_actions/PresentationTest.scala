package com.fdilke.bewl.fsets.monoid_actions;

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{>, ToposOfMonoidActions, VOID}
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.freespec.AnyFreeSpec
import com.fdilke.bewl.helper.StandardSymbols.{a, b, c, i, iso, r, s, source, target, x, y}
import org.scalatest.matchers.should.Matchers._

import scala.language.{existentials, reflectiveCalls}

class PresentationTest extends AnyFreeSpec {

  private val actionTopos =
    ToposOfMonoidActions.of(monoidOf3)

  "Building actions from presentations" - {
    "works for an empty generating set" in {
      val presentation =
        FiniteSetsPresentedAction(
          monoidOf3
        )(
          List[GeneratorWithRelators[Symbol, VOID]]()
        )
      val emptyAction =
        monoidOf3.voidAction
      val emptyProjection: Int > VOID =
        presentation.project(
          emptyAction,
          Seq[VOID]()
        )
      emptyProjection should have(
        source(presentation.action.actionCarrier),
        target(emptyAction.actionCarrier)
      )
      monoidOf3.actions.isMorphism(
        presentation.action,
        emptyAction,
        emptyProjection
      ) shouldBe true
      emptyProjection shouldBe iso
    }
    "works for a single generator with no relators" in {
      val presentation =
        FiniteSetsPresentedAction(
          monoidOf3
        )(
          List[GeneratorWithRelators[Symbol, FiniteSets.UNIT]](
            GeneratorWithRelators(
              (),
              Seq.empty
            )
          )
        )

      val regularAction =
        monoidOf3.regularAction
      val regularProjection: Int > Symbol =
        presentation.project(
          regularAction,
          List(
            i
          )
        )
      regularProjection should have(
        source(presentation.action.actionCarrier),
        target(regularAction.actionCarrier)
      )
      monoidOf3.actions.isMorphism(
        presentation.action,
        regularAction,
        regularProjection
      ) shouldBe true
      regularProjection shouldBe iso
    }
    "works for presenting a cyclic right ideal { x, y }" in {
      val presentation =
        FiniteSetsPresentedAction(
          monoidOf3
        )(
          List[GeneratorWithRelators[Symbol, FiniteSets.UNIT]](
            GeneratorWithRelators(
              (),
              Seq(
                Relator(x, 0, i)
              )
            )
          )
        )

      val idealCarrier = dot(x, y)
      val idealAction =
        monoidOf3.action(idealCarrier) {
          monoidOf3.multiply(_, _)
        }

      val idealProjection: Int > Symbol =
        presentation.project(
          idealAction,
          List(
            x
          )
        )
      idealProjection should have(
        source(presentation.action.actionCarrier),
        target(idealCarrier)
      )
      monoidOf3.actions.isMorphism(
        presentation.action,
        idealAction,
        idealProjection
      ) shouldBe true
      idealProjection shouldBe iso
    }
    "works for presenting 2 over its endomorphism monoid" in {
      val ¬ = Symbol("¬")
      val O = Symbol("O")
      val I = Symbol("I")
      val end2 =
        monoidFromTable(
          i,
          ¬,
          O,
          I,
          ¬,
          i,
          O,
          I,
          O,
          I,
          O,
          I,
          I,
          O,
          O,
          I
        )

      val presentation =
        FiniteSetsPresentedAction(
          end2
        )(
          List[GeneratorWithRelators[Symbol, Int]](
            GeneratorWithRelators(
              0,
              Seq(
                Relator(O, 0, i),
                Relator(¬, 0, I)
              )
            )
          )
        )

      val twoCarrier = dot(0, 1)
      val twoActionMap: Map[Symbol, Int => Int] =
        Map(
          i -> identity,
          ¬ -> { 1 - _ },
          O -> { _ => 0 },
          I -> { _ => 1 }
        )
      val twoAction =
        end2.action(twoCarrier)((s, m) => twoActionMap(m)(s))

      val twoProjection: Int > Int =
        presentation.project(
          twoAction,
          List(
            0
          )
        )
      twoProjection should have(
        source(presentation.action.actionCarrier),
        target(twoCarrier)
      )
      end2.actions.isMorphism(
        presentation.action,
        twoAction,
        twoProjection
      ) shouldBe true
      twoProjection shouldBe iso
    }
    "works for a cyclic action over a group" in {
      val group = monoidFromTable(
        i,
        a,
        b,
        c,
        r,
        s,
        a,
        i,
        s,
        r,
        c,
        b,
        b,
        r,
        i,
        s,
        a,
        c,
        c,
        s,
        r,
        i,
        b,
        a,
        r,
        b,
        c,
        a,
        s,
        i,
        s,
        c,
        a,
        b,
        i,
        r
      )

      val presentation =
        FiniteSetsPresentedAction(
          group
        )(
          List[GeneratorWithRelators[Symbol, String]](
            GeneratorWithRelators(
              "x",
              Seq(
                Relator(r, 0, a)
              )
            )
          )
        )

      presentation.action.actionCarrier should have size 3
    }
    // note: should probably add a more sophisticated
    // example, with multiple interacting generators
  }
}
