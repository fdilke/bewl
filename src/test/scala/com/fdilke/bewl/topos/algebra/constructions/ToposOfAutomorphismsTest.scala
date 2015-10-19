package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsPreArrow}
import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}

import scala.Function._

abstract class ToposOfAutomorphismsTest extends GenericToposTests(new ToposWithFixtures {

  override val topos = FiniteSets.ToposOfAutomorphisms.build

  import topos._

  override type FOO = WRAPPER[Symbol]
  override type BAR = WRAPPER[Int]
  override type BAZ = WRAPPER[String]

  private val (a, b, c, d) = ('a, 'b, 'c, 'd)
  override val foo = dot(a, b, c, d)(a -> c, b -> d, c -> a, d -> b)

  override val bar = dot(1, 2)(1 -> 2, 2 -> 1)

  override val baz = dot("x", "y", "z", "w")(
    "x" -> "y", "y" -> "x", "z" -> "z", "w" -> "w"
  )

  override val foo2bar = foo(bar)(Map(
    a -> 1,
    b -> 2,
    c -> 2,
    d -> 1
  ).asInstanceOf[Map[FOO, BAR]])

  override val monicBar2baz = bar(baz)(Map(
    1 -> "x",
    2 -> "y"
  ).asInstanceOf[Map[BAR, BAZ]])

  override val foo2ImageOfBar = foo(baz)(Map(
    a -> "x",
    b -> "x",
    c -> "y",
    d -> "y"
  ).asInstanceOf[Map[FOO, BAZ]])

  override val foobar2baz = bifunctionAsBiArrow(foo, bar, baz)(untupled (Map(
    (a, 1) -> "x", (b, 1) -> "y", (c, 1) -> "y", (d, 1) -> "y",
    (a, 2) -> "x", (b, 2) -> "x", (c, 2) -> "y", (d, 2) -> "x"
  )))

  private def dot[A](values: A*)(mappings: (A, A)*) = /* : DOT[A] = */ {
    val set: Set[A] = Set(values :_*)
    makeDot(FiniteSets.makeArrow(FiniteSetsPreArrow(set, set, Map(mappings: _*))))
  }

  override def makeSampleDot() =
    dot(1, 2, 3)(1 -> 2, 2 -> 3, 3 -> 1)

  override def makeSampleArrow() =
    makeSampleDot()(makeSampleDot())(Map(
      1 -> 3, 3 -> 2, 2 -> 1
    ).asInstanceOf[Map[WRAPPER[Int], WRAPPER[Int]]])

  override val equalizerSituation = new EqualizerSituation[WRAPPER[UNIT], FOO, BAZ](
    dot[UNIT]()()(foo)(Map()),
    foo2ImageOfBar,
    monicBar2baz o foo2bar
  )
}) {
  // TODO: add tests for booleanness, globals
}
