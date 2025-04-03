package com.fdilke.bewl2.actions

import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposSpec

import Sets.{ Automorphism => Permutation }

abstract class ToposOfAutomorphismsSpec
  extends GenericToposSpec[
    Permutation,
    [A] =>> A,
    Void,
    Unit,
    Boolean,
    Map
  ](
    Sets.toposOfAutomorphisms
  ):
  import topos._

  override type FOO = Symbol
  override type BAR = Int
  override type BAZ = String

  val Seq(a, b, c, d) =
    Seq("a", "b", "c", "d").map:
      Symbol(_)

  private val fooPermutation: Permutation[FOO] =
    Sets.withDot(Set(a, b, c, d)):
      Permutation[Symbol]:
        Map(a -> c, b -> d, c -> a, d -> b) : Sets.~>[Symbol, Symbol]

  private val barPermutation: Permutation[BAR] =
    ???

  private val bazPermutation: Permutation[BAZ] =
    ???

  override def withTestDots(
    block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit =
    withDots(
      fooPermutation,
      barPermutation,
      bazPermutation
    ):
      block:
        new ToposFixtures:
          override val foo2bar: Symbol ~> Int =
            ??? // Map(e -> "x", a -> "x'")

          override val foo2baz: Symbol ~> String =
            ??? // Map(e -> 4, a -> 3)

          override val monicBar2baz: Int ~> String =
            ??? // Map("x" -> 3, "x'" -> 4, "y" -> 5)

          val altMonicBar2baz: Int ~> String =
            ??? // Map("x" -> 2, "x'" -> 1, "y" -> 5)

          override val foobar2baz: (Symbol, Int) ~> String =
            ???
//            Map(
//              (e, "x") -> 1,
//              (e, "x'") -> 3,
//              (e, "y") -> 2,
//              (a, "x") -> 4,
//              (a, "x'") -> 2,
//              (a, "y") -> 1
//            )

          override val foo2ImageOfBar: Symbol ~> String =
            ??? // Map(e -> 4, a -> 3)

          override val equalizerSituation: EqualizerSituation[_, _, _] =
            EqualizerSituation[Unit, Symbol, String](
              _ => Symbol("sensibleChoiceNotThis"),
              foo2ImageOfBar,
              monicBar2baz o foo2bar
//              _ => "y",
//              monicBar2baz,
//              altMonicBar2baz
            )
            
          override val isomorphismSituation: IsomorphismSituation[_, _] =
            IsomorphismSituation[Symbol, String]:
              _.toString

