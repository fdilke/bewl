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
  val bazAction: ACTION[ROPE]
  val intAction: ACTION[Int]
  val boolAction: ACTION[Boolean]

abstract class BaseGroupActionsSpec[ACTION[_]](
  val fixtures: GroupActionsSetupFixtures[ACTION]
) extends GenericToposSpec[ACTION, [A] =>> A, Void, Unit, Boolean, Map](
  fixtures.actionTopos
):
  import topos.*
  import fixtures.*

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
          override val foo2bar: Symbol ~> String =
            Map(
              i -> "x", x -> "x", y -> "y"
            )

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
          (s, m) => groupOf2.multiply(Symbol(s), m).name
        }

    override val barAction: groupOf2.Action[String] =
      actionOnStrings("x", "y")

    override val bazAction: groupOf2.Action[ROPE] =
      Rope.blur(actionOnStrings("i", "x", "y"))

    override val intAction: groupOf2.Action[Int] =
      withDot(
        Set[Int](0, 1, 2, 3)
      ):
        groupOf2.Action[Int](
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

    override val boolAction: groupOf2.Action[Boolean] =
      groupOf2.Action[Boolean](
        (f: Boolean, _: Symbol) =>
          f
      )

object VulgarSymbolDefs:
  object Rope extends Opacity[String]
  type ROPE = Rope.theType
  val Seq(i, x, y): Seq[Symbol] = 
    Seq[String]("i", "x", "y").map { Symbol(_) }
  val Seq(xR, yR): Seq[ROPE] =
    Seq[String]("x", "y").map { Rope.blur[[A] =>> A](_) }

