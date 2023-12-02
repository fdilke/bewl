package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.sets.Sets
import Sets.{
  Monoid,
  Dot,
  MonoidActionGeneratorFinder,
  PresentationFinder,
  withDot,
  withMonoidFromTable,
  RichArrow
}
import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.utility.StockStructures._

import scala.language.{existentials, reflectiveCalls}

class PresentationSpec extends FunSuite:

  val Seq(i, a, b, c, r, s, x, y) =
    Seq[String]("i", "a", "b", "c", "r", "s", "x", "y").map { Symbol(_) }

  withMonoidOf3(Sets):
    (_: Dot[Symbol]) ?=> (monoidOf3: Sets.Monoid[Symbol]) ?=>

    val actionTopos: Topos[
      monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap
    ] =
      monoidOf3.actionTopos

    test("Building actions from presentations works for an empty generating set"):
      val presentation =
        FiniteSetsPresentedMonoidAction(
          monoidOf3
        )(
          List[GeneratorWithRelators[Symbol, Void]]()
        )
      monoidOf3.withVoidAction:
        (voidAction: monoidOf3.Action[Void]) ?=>

        val voidProjection: Int => Void =
          presentation.project(
            voidAction,
            Seq.empty[Void]
          )
        given Dot[Int] = presentation.action.dot
        given monoidOf3.Action[Int] = presentation.action
        monoidOf3.actions.isMorphism(
          voidProjection
        ) is true
        voidProjection.isIso is true

    test("Building actions works for a single generator with no relators"):
      val presentation =
        FiniteSetsPresentedMonoidAction(
          monoidOf3
        )(
          List[GeneratorWithRelators[Symbol, Unit]](
            GeneratorWithRelators(
              (),
              Seq.empty
            )
          )
        )

      monoidOf3.withRegularAction:
        (regularAction: monoidOf3.Action[Symbol]) ?=>

        val regularProjection: Int => Symbol =
          presentation.project(
            regularAction,
            List(i)
          )
        
        given Dot[Int] = presentation.action.dot
        given monoidOf3.Action[Int] = presentation.action
        monoidOf3.actions.isMorphism(
          regularProjection
        ) is true
        regularProjection.isIso is true

    test("Building actions works for presenting a cyclic right ideal { x, y }"):
      val presentation =
        FiniteSetsPresentedMonoidAction(
          monoidOf3
        )(
          List[GeneratorWithRelators[Symbol, Unit]](
            GeneratorWithRelators(
              (),
              Seq(
                Relator(x, 0, i)
              )
            )
          )
        )

      withDot(Set[Symbol](x, y)):
        (idealDot: Dot[Symbol]) ?=>

        val idealMultiply: ((Symbol, Symbol)) => Symbol =
            case (s, m) => monoidOf3.multiply(s, m)

        monoidOf3.withAction(idealMultiply):
          (idealAction: monoidOf3.Action[Symbol]) ?=>
          val idealProjection: Int => Symbol =
            presentation.project(
              idealAction,
              List(x)
            )
          // idealProjection should have(
          //   source(presentation.action.actionCarrier),
          //   target(idealCarrier)
          // )
          given Dot[Int] = presentation.action.dot
          given monoidOf3.Action[Int] = presentation.action
          monoidOf3.actions.isMorphism(
            // presentation.action,
            // idealAction,
            idealProjection
          ) is true
          idealProjection.isIso is true

    test("Building an action works for presenting 2 over its endomorphism monoid"):
      val ¬ = Symbol("¬")
      val O = Symbol("O")
      val I = Symbol("I")
      withMonoidFromTable(
        i, ¬, O, I,
        ¬, i, O, I,
        O, I, O, I,
        I, O, O, I
      ):
        (_: Dot[Symbol]) ?=> (end2: Monoid[Symbol]) ?=>

        val presentation: PresentedMonoidAction[Int, end2.Action] =
          FiniteSetsPresentedMonoidAction(end2)(
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
        withDot(Set[String]("0", "1")):
          (twoDot: Dot[String]) ?=>

          val twoActionMap: Map[Symbol, String => String] =
            Map(
              i -> identity,
              ¬ -> Map("0" -> "1", "1" -> "0"),
              O -> { _ => "0" },
              I -> { _ => "1" }
            )
          val twoMultiply: ((String, Symbol)) => String =
            case (s, m) => twoActionMap(m)(s)

          end2.withAction(twoMultiply):
            (twoAction: end2.Action[String]) ?=>
            val twoProjection: Int => String =
              presentation.project(
                twoAction,
                List("0")
              )
            given Dot[Int] = presentation.action.dot
            given end2.Action[Int] = presentation.action
            end2.actions.isMorphism(
                twoProjection
            ) is true
            twoProjection.isIso is true

    test("Building an action works for a cyclic action over a group"):
      withMonoidFromTable(
        i, a, b, c, r, s,
        a, i, s, r, c, b,
        b, r, i, s, a, c,
        c, s, r, i, b, a,
        r, b, c, a, s, i,
        s, c, a, b, i, r
      ):
        (_: Dot[Symbol]) ?=> (group: Monoid[Symbol]) ?=>

        val presentation =
          FiniteSetsPresentedMonoidAction(group)(
            List[GeneratorWithRelators[Symbol, String]](
              GeneratorWithRelators(
                "x",
                Seq(
                  Relator(r, 0, a)
                )
              )
            )
          )

        presentation.action.dot.dot.size is 3

// note: should probably add a more sophisticated
// example, with multiple interacting generators

// TODO: Refactor for convenience with withPresentation():