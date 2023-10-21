package com.fdilke.bewl2.actions

import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.sets.{ Sets, FastSets, BaseSets }
import com.fdilke.bewl2.topos.GenericToposSpec
import munit.FunSuite
import com.fdilke.bewl2.utility.StockEnums
import com.fdilke.bewl2.utility.StockStructures._
import StockEnums.Direction
import Direction._
import com.fdilke.bewl2.utility.Opacity
import com.fdilke.utility.TimeIt
import com.fdilke.bewl2.utility.RichFunSuite
import cats.syntax.monoid
import CommonSymbolDefs._

object SlowMonoidActionsSetup extends MonoidActionsSetup(Sets)
object FastMonoidActionsSetup extends MonoidActionsSetup(FastSets)

class SlowMonoidActionsSpec extends BaseMonoidActionsSpec(SlowMonoidActionsSetup.Fixtures)
// class FastMonoidActionsSpec extends BaseMonoidActionsSpec(FastMonoidActionsSetup.Fixtures)

trait MonoidActionsSetupFixtures[ACTION[_], RIGHT_IDEAL, INTERNAL_MAP[_, _]]:
  val actionTopos: Topos[ACTION, [A] =>> A, Void, Unit, RIGHT_IDEAL, INTERNAL_MAP]
  val fooAction: ACTION[Symbol]
  val barAction: ACTION[String]
  val bazAction: ACTION[ROPE]
  val intAction: ACTION[Int]
  val boolAction: ACTION[Boolean]

abstract class BaseMonoidActionsSpec[ACTION[_], RIGHT_IDEAL, INTERNAL_MAP[_, _]](
  val fixtures: MonoidActionsSetupFixtures[ACTION, RIGHT_IDEAL, INTERNAL_MAP]
) extends GenericToposSpec[ACTION, [A] =>> A, Void, Unit, RIGHT_IDEAL, INTERNAL_MAP](
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
              e -> "a", a -> "a", b -> "b"
            )

          override val foo2baz: Symbol ~> ROPE =
            s => Rope.blur[[A] =>> A](s.name)

          override val monicBar2baz: String ~> ROPE =
            Rope.blur[[A] =>> A](_)

          override val foobar2baz: (Symbol, String) ~> ROPE = 
            Map(
              (e, "a") -> aR,
              (a, "a") -> aR,
              (b, "a") -> bR,
              (e, "b") -> bR,
              (a, "b") -> aR,
              (b, "b") -> bR
            )

          override val foo2ImageOfBar: Symbol ~> ROPE =
            Map(
              e -> bR, 
              a -> aR,
              b -> bR
            )

          override val equalizerSituation: EqualizerSituation[_, _, _] =
            EqualizerSituation[FOO, Int, Boolean](
              Map(
                e -> 1,
                a -> 1,
                b -> 2
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

  // val fixtures: MonoidActionsFixtures,
  // val actionTopos: Topos[ACTION, [A] =>> A, Void, Unit, RIGHT_IDEAL, INTERNAL_MAP]

abstract class MonoidActionsSetup(val baseSets: BaseSets):
  import baseSets._

  val monoidOf3: Monoid[Symbol] =
    withMonoidOf3(baseSets):
      summon

  object Fixtures extends MonoidActionsSetupFixtures[monoidOf3.Action, monoidOf3.RightIdeal, monoidOf3.InternalMap]:    
    override val actionTopos: Topos[
      monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap
    ] =
      monoidOf3.actionTopos
    override val fooAction: monoidOf3.Action[Symbol] =
      monoidOf3.withRegularAction:
        summon

    def actionOnStrings(strings: String*): monoidOf3.Action[String] =
      withDot(
        Set[String](strings :_*)
      ):
        monoidOf3.Action {
          (s, m) => monoidOf3.multiply(Symbol(s), m).name
        }

    override val barAction: monoidOf3.Action[String] =
      actionOnStrings("a", "b")

    override val bazAction: monoidOf3.Action[ROPE] =
      Rope.blur(actionOnStrings("e", "a", "b"))

    override val intAction: monoidOf3.Action[Int] =
      withDot(
        Set[Int](0, 1, 2, 3)
      ):
        monoidOf3.Action[Int](
          (n: Int, r: Symbol) =>
            if (n == 0)
              0
            else
              r match {
                case `e` => n
                case `a` => 1
                case `b` => 2
              }
        )

    override val boolAction: monoidOf3.Action[Boolean] =
      monoidOf3.Action[Boolean](
        (f: Boolean, _: Symbol) =>
          f
      )

// abstract class BaseMonoidActionsSpec(
//   val fixtures: MonoidActionsFixtures
// ) extends GenericToposSpec[
//   fixtures.monoidOf3.Action, [A] =>> A, Void, Unit, fixtures.monoidOf3.RightIdeal, fixtures.monoidOf3.InternalMap
//   ](
//   fixtures.actionTopos
// ):

// fixtures.monoidOf3.InternalMap
// fixtures.monoidOf3.RightIdeal
// fixtures.monoidOf3.Action

// abstract class BaseMonoidActionsSpec[ACTION[_], RIGHT_IDEAL, INTERNAL_MAP[_, _]](
//   val fixtures: MonoidActionsFixtures
// ) extends GenericToposSpec[ACTION, [A] =>> A, Void, Unit, RIGHT_IDEAL, INTERNAL_MAP](
//   fixtures.actionTopos
// )

object CommonSymbolDefs: // TODO fix this!
  object Rope extends Opacity[String]
  type ROPE = Rope.theType
  val Seq(e, a, b): Seq[Symbol] = 
    Seq[String]("e", "a", "b").map { Symbol(_) }
  val Seq(aR, bR): Seq[ROPE] =
    Seq[String]("a", "b").map { Rope.blur[[A] =>> A](_) }
