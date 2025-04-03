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
import CommonSymbolDefs._

object SlowMonoidActionsSetup extends MonoidActionsSetup(SetsWithSlowActions)
object FastMonoidActionsSetup extends MonoidActionsSetup(Sets)

class SlowMonoidActionsSpec extends BaseMonoidActionsSpec(SlowMonoidActionsSetup.Fixtures)
class FastMonoidActionsSpec extends BaseMonoidActionsSpec(FastMonoidActionsSetup.Fixtures)

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
      block:
        new ToposFixtures:
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
            
          override val isomorphismSituation: IsomorphismSituation[_, _] =
            IsomorphismSituation[Symbol, ROPE]:
              foo2baz
//              Seq(i, x, y).map:
//                q => q -> q
//              .toMap.view.mapValues: symbol =>
//                monicBar2baz(symbol.toString)

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
        monoidOf3.Action:
          (s, m) => monoidOf3.multiply(Symbol(s), m).name

    override val barAction: monoidOf3.Action[String] =
      actionOnStrings("x", "y")

    override val bazAction: monoidOf3.Action[ROPE] =
      Rope.blur(actionOnStrings("i", "x", "y"))

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
                case `i` => n
                case `x` => 1
                case `y` => 2
              }
        )

    override val boolAction: monoidOf3.Action[Boolean] =
      monoidOf3.Action[Boolean]:
        (f: Boolean, _: Symbol) =>
          f

object CommonSymbolDefs:
  object Rope extends Opacity[String]
  type ROPE = Rope.theType
  val Seq(i, x, y): Seq[Symbol] = 
    Seq[String]("i", "x", "y").map { Symbol(_) }
  val Seq(xR, yR): Seq[ROPE] =
    Rope.blur[Seq]:
      Seq[String]("x", "y")
