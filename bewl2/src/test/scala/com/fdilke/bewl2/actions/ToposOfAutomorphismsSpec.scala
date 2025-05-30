package com.fdilke.bewl2.actions

import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposSpec

import Sets.{ Automorphism => Permutation }

class ToposOfAutomorphismsSpec
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

  override def withTestDots(
    block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit =
    val Seq(a, b, c, d) =
      Seq("a", "b", "c", "d").map:
        Symbol(_)

    val fooPermutation: Permutation[FOO] =
      Sets.withDot(Set(a, b, c, d)):
        Permutation[Symbol]:
          Map(a -> c, b -> d, c -> a, d -> b)

    val barPermutation: Permutation[BAR] =
      Sets.withDot(Set(1, 2)):
        Permutation[Int]:
          Map(1 -> 2, 2 -> 1)

    val bazPermutation: Permutation[BAZ] =
      Sets.withDot(Set("x", "y", "z", "w")):
        Permutation[String]:
          Map("x" -> "y", "y" -> "x", "z" -> "z", "w" -> "w")
    withDots(
      fooPermutation,
      barPermutation,
      bazPermutation
    ):
      block:
        new ToposFixtures:
          override val foo2bar: Symbol ~> Int =
            Map(
              a -> 1,
              b -> 2,
              c -> 2,
              d -> 1
            )

          override val foo2baz: Symbol ~> String =
            Map(a -> "x", b -> "z", c -> "y",  d -> "z")

          override val monicBar2baz: Int ~> String =
            Map(
              1 -> "x",
              2 -> "y"
            )

          val altMonicBar2baz: Int ~> String =
            Map(1 -> "x", 2 -> "y")

          override val foobar2baz: (Symbol, Int) ~> String =
            Map(
              (a, 1) -> "x",
              (b, 1) -> "y",
              (c, 1) -> "y",
              (d, 1) -> "y",
              (a, 2) -> "x",
              (b, 2) -> "x",
              (c, 2) -> "y",
              (d, 2) -> "x"
            )

          override val foo2ImageOfBar: Symbol ~> String =
            Map(
              a -> "x",
              b -> "x",
              c -> "y",
              d -> "y"
            )

          override val equalizerSituation: EqualizerSituation[?, ?, ?] =
            EqualizerSituation[Unit, Symbol, String](
              _ => Symbol("sensibleChoiceNotThis"),
              foo2ImageOfBar,
              monicBar2baz o foo2bar
            )
            
          override val isomorphismSituation: IsomorphismSituation[?, ?] =
            IsomorphismSituation[Symbol, (Int, Int)]:
              Map(a -> (1, 2), b -> (1, 1), c -> (2, 1),  d -> (2, 2))

