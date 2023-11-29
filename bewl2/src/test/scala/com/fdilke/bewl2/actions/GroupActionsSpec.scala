package com.fdilke.bewl2.actions

import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.sets.{SetsWithSlowActions, BaseSets}
import com.fdilke.bewl2.sets.{Sets, BaseSets}
import com.fdilke.bewl2.topos.GenericToposSpec
import munit.FunSuite
import com.fdilke.bewl2.utility.StockEnums
import com.fdilke.bewl2.utility.StockStructures._
import StockEnums.Direction
import Direction._
import com.fdilke.bewl2.utility.Opacity
import com.fdilke.utility.TimeIt
import cats.syntax.monoid
import VulgarSymbolDefs._

object SlowGroupActionsSetup extends GroupActionsSetup(SetsWithSlowActions)
object FastGroupActionsSetup extends GroupActionsSetup(Sets)

abstract class SlowGroupActionsSpec extends BaseGroupActionsSpec(SlowGroupActionsSetup.Fixtures)
abstract class FastGroupActionsSpec extends BaseGroupActionsSpec(FastGroupActionsSetup.Fixtures)

trait GroupActionsSetupFixtures[ACTION[_]]:
  val actionTopos: Topos[ACTION, [A] =>> A, Void, Unit, Boolean, Map]
  val fooAction: ACTION[Symbol]
  val barAction: ACTION[String]
  val bazAction: ACTION[Int]
  // val bazAction: ACTION[ROPE]
  // val intAction: ACTION[Int]
  // val boolAction: ACTION[Boolean]

abstract class BaseGroupActionsSpec[ACTION[_]](
  val fixtures: GroupActionsSetupFixtures[ACTION]
) extends GenericToposSpec[ACTION, [A] =>> A, Void, Unit, Boolean, Map](
  fixtures.actionTopos
):
  import topos.*
  import fixtures.*

  override type FOO = Symbol
  override type BAR = String
  override type BAZ = Int

  override def withTestDots(
    block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit =
    withDots(
      fooAction,
      barAction,
      bazAction
      // intAction,
      // boolAction
    ):
      block(
        new ToposFixtures {
          override val foo2bar: Symbol ~> String =
            Map(e -> "x", a -> "x'")

          override val foo2baz: Symbol ~> Int =
            Map(e -> 4, a -> 3)

          override val monicBar2baz: String ~> Int =
            Map("x" -> 3, "x'" -> 4, "y" -> 5)

          val altMonicBar2baz: String ~> Int =
            Map("x" -> 2, "x'" -> 1, "y" -> 5)

          override val foobar2baz: (Symbol, String) ~> Int = 
            Map(
              (e, "x") -> 1,
              (e, "x'") -> 3,
              (e, "y") -> 2,
              (a, "x") -> 4,
              (a, "x'") -> 2,
              (a, "y") -> 1
            )

          override val foo2ImageOfBar: Symbol ~> Int =
            Map(e -> 4, a -> 3)

          override val equalizerSituation: EqualizerSituation[_, _, _] =
            EqualizerSituation[Unit, String, Int](
              _ => "y",
              monicBar2baz,
              altMonicBar2baz
            )
        }
      )

abstract class GroupActionsSetup(val baseSets: BaseSets):
  import baseSets._

  val groupOf2: Group[Symbol] =
    withGroupOf2(baseSets):
      summon

  object Fixtures extends GroupActionsSetupFixtures[groupOf2.Action]:    
    override val actionTopos: Topos[
      groupOf2.Action, [A] =>> A, Void, Unit, Boolean, Map
    ] =
      groupOf2.actionTopos
    override val fooAction: groupOf2.Action[Symbol] =
      groupOf2.withRegularAction:
        summon

    def actionOnStrings(strings: String*): groupOf2.Action[String] =
      withDot(
        Set[String](strings :_*)
      ):
        groupOf2.Action {
          (s, g) =>
            if (g == e) || (s == "y") then
              s
            else if (s == "x")
              "x'"
            else
              "x"
        }

    override val barAction: groupOf2.Action[String] =
      actionOnStrings("x", "x'", "y")

    def actionOnInts(intMap: Map[Int, Int]): groupOf2.Action[Int] =
      withDot(
        intMap.keySet
      ):
        groupOf2.Action:
          (i, g) =>
            if (g == e) then
              i
            else
              intMap(i)

    override val bazAction: groupOf2.Action[Int] =
      actionOnInts(Map(1 -> 2, 2 -> 1, 3 -> 4, 4 -> 3, 5 -> 5))

    // override val intAction: groupOf2.Action[Int] =
    //   withDot(
    //     Set[Int](0, 1, 2, 3)
    //   ):
    //     groupOf2.Action[Int](
    //       (n: Int, r: Symbol) =>
    //         if (n == 0)
    //           0
    //         else
    //           r match {
    //             case `i` => n
    //             case `x` => 1
    //             case `y` => 2
    //           }
    //     )

    // override val boolAction: groupOf2.Action[Boolean] =
    //   groupOf2.Action[Boolean](
    //     (f: Boolean, _: Symbol) =>
    //       f
    //   )

object VulgarSymbolDefs:
  object Rope extends Opacity[String]
  // type ROPE = Rope.theType
  val Seq(e, a, b): Seq[Symbol] = 
    Seq[String]("e", "a", "b").map { Symbol(_) }
  // val Seq(xR, yR): Seq[ROPE] =
  //   Seq[String]("x", "y").map { Rope.blur[[A] =>> A](_) }

