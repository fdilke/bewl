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

object MonoidActionsSpec:
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

  val bazAction: monoidOf3.Action[String] =
    actionOnStrings("i", "x", "y")
  
class MonoidActionsSpec
/*
class MonoidActionsSpec extends GenericToposSpec()(MonoidActionsSpec.actionTopos):
  import topos.*

  override type FOO = Symbol
  override type BAR = String
  override type BAZ = String

  override def withTestDots(
    block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit =
    withDots(
      fooAction,
      barAction,
      bazAction
    ):
      block(
        new ToposFixtures {
          override val foo2bar: Direction ~> String =
            Map[Direction, String](elems =
              Up -> "one",
              Down -> "two"
            )

          override val foo2baz: Direction ~> Int =
            Map[Direction, Int](elems =
              Up -> 3,
              Down -> 2
            )

          override val monicBar2baz: String ~> Int =
            Map[String, Int](elems =
              "one" -> 2,
              "two" -> 3,
              "three" -> 1
            )

          override val foobar2baz: (Direction, String) ~> Int =
            Map[(Direction, String), Int](elems =
              (Up, "one") -> 2,
              (Down, "one") -> 3,
              (Up, "two") -> 1,
              (Down, "two") -> 2,
              (Up, "three") -> 2,
              (Down, "three") -> 3
            )

          override val foo2ImageOfBar: Direction ~> Int =
            Map[Direction, Int](elems =
              Up -> 3,
              Down -> 2
            )

          override val equalizerSituation: EqualizerSituation[_, _, _] =
            new EqualizerSituation[FOO, BAR, BAZ](
              foo2bar,
              Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3),
              Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 1)
            )
        }
      )
*/




