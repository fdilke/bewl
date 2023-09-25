package com.fdilke.bewl2.actions

import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposSpec
import munit.FunSuite
import com.fdilke.bewl2.utility.StockEnums
import com.fdilke.bewl2.utility.StockStructures._
import StockEnums.Direction
import Direction._
import MonoidActionsSpec._
import com.fdilke.bewl2.utility.Opacity
import com.fdilke.utility.TimeIt
import com.fdilke.bewl2.utility.RichFunSuite
import cats.syntax.monoid

object MonoidActionsSpec:
  val Seq(e, a, b): Seq[Symbol] = 
    Seq[String]("e", "a", "b").map { Symbol(_) }
  val Seq(aR, bR): Seq[ROPE] =
    Seq[String]("a", "b").map { Rope.blur[[A] =>> A](_) }

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
    actionOnStrings("a", "b")

  object Rope extends Opacity[String]
  type ROPE = Rope.theType
  val bazAction: monoidOf3.Action[ROPE] =
    Rope.blur(actionOnStrings("e", "a", "b"))

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
              case `e` => n
              case `a` => 1
              case `b` => 2
            }
      )

  val boolAction: monoidOf3.Action[Boolean] =
    monoidOf3.Action[Boolean](
      (f: Boolean, _: Symbol) =>
        f
    )


object TackyProbe extends App:
  type FOO = Symbol
  type BAR = String
  import MonoidActionsSpec.*
  import actionTopos.* 

  println("I'm going to perform an action")
  withDots(fooAction, barAction):
    sanityTest[FOO]
    sanityTest[BAR]
    val foo2bar: Symbol ~> String = Map(e -> "a", a -> "a", b -> "b")
    println("Computing logical operations")
    val (logOps, msg) = TimeIt { logicalOperations }
    println(s"Computing logical operations: done ($msg)")
    println("Sanity testing over the monoid")
    sanityTest(foo2bar)
    println(s"Enumerating morphisms...")
    morphisms[FOO, BAR].foreach { m =>
      println(s"Morphism: $m")
    }
    println(s"Enumerating morphisms...done")
  println("I'm going to perform an action... how was that?")


class MonoidActionsSpec extends GenericToposSpec()(MonoidActionsSpec.actionTopos):
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




