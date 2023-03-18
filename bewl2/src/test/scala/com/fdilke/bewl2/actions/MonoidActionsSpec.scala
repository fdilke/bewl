package com.fdilke.bewl2.actions

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposSpec
import munit.FunSuite
import com.fdilke.bewl2.utility.StockEnums
import com.fdilke.bewl2.utility.StockStructures._
import StockEnums.Direction
import Direction._
import MonoidActionsSpec._
import com.fdilke.bewl2.utility.Opacity

object MonoidActionsSpec:
  val Seq(i, x, y): Seq[Symbol] = 
    Seq[String]("i", "x", "y").map { Symbol(_) }
  val Seq(xR, yR): Seq[ROPE] =
    Seq[String]("x", "y").map { Rope.blur[[A] =>> A](_) }

  val monoidOf3: Sets.Monoid[Symbol] =
    withMonoidOf3:
      summon

  val actionTopos: Topos[monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap] =
    monoidOf3.actionTopos
  val fooAction: monoidOf3.Action[Symbol] =
    monoidOf3.withRegularAction:
      summon

  def actionOnStrings(strings: String*): monoidOf3.Action[String] =
    Sets.withDot(
      Set[String](strings :_*)
    ):
      monoidOf3.Action {
        (s, m) => monoidOf3.multiply(Symbol(s), m).name
      }

  val barAction: monoidOf3.Action[String] =
    actionOnStrings("x", "y")

  object Rope extends Opacity[String]
  type ROPE = Rope.theType
  val bazAction: monoidOf3.Action[ROPE] =
    Rope.blur(actionOnStrings("i", "x", "y"))

  val intAction: monoidOf3.Action[Int] =
    Sets.withDot(
      Set[Int](0, 1, 2, 3)
    ):
      monoidOf3.Action[Int](
        (n: Int, r: Symbol) =>
          if (n == 0)
            0
          else
            r match {
              case `i` => n
              case `x` => 1
              case `y` => 2
            }
      )

  val boolAction: monoidOf3.Action[Boolean] =
    monoidOf3.Action[Boolean](
      (b: Boolean, _: Symbol) =>
        b
    )

abstract class MonoidActionsSpec extends GenericToposSpec()(MonoidActionsSpec.actionTopos):
  import topos.*

  override type FOO = Symbol
  override type BAR = String
  override type BAZ = ROPE

  override def withTestDots(
    block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit =
    withDots(
      fooAction,
      barAction,
      bazAction,
      intAction,
      boolAction
    ):
      block(
        new ToposFixtures {
          override val foo2bar: Symbol ~> String = { _.name }
          override val foo2baz: Symbol ~> ROPE =
            s => Rope.blur[[A] =>> A](s.name)

          override val monicBar2baz: String ~> ROPE =
            Rope.blur[[A] =>> A](_)

          override val foobar2baz: (Symbol, String) ~> ROPE = 
            Map(
              (i, "x") -> xR,
              (x, "x") -> xR,
              (y, "x") -> yR,
              (i, "y") -> yR,
              (x, "y") -> xR,
              (y, "y") -> yR
            )

          override val foo2ImageOfBar: Symbol ~> ROPE =
            Map(
              i -> yR, 
              x -> xR,
              y -> yR
            )

          override val equalizerSituation: EqualizerSituation[_, _, _] =
            EqualizerSituation[FOO, Int, Boolean](
              Map(
                i -> 1,
                x -> 1,
                y -> 2
              ),
              Map(
                0 -> true,
                1 -> true,
                2 -> true,
                3 -> true
              ),
              Map(
                0 -> false,
                1 -> true,
                2 -> true,
                3 -> true
              )
            )
        }
      )




