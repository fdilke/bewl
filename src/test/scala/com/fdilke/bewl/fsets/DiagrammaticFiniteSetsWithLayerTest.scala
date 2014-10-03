package com.fdilke.bewl.fsets

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}

class DiagrammaticFiniteSetsWithLayerTest extends GenericToposTests(new ToposWithFixtures {
  type TOPOS = DiagrammaticFiniteSets.type
  val topos = DiagrammaticFiniteSets

  import topos._

  type FOO = WrappedArrow[Boolean]
  type BAR = WrappedArrow[String]
  type BAZ = WrappedArrow[Int]

  def star[T](elements: T*) = wrapDot(FiniteSetsDot(elements))

  def quiver[S, T](source: STAR[WrappedArrow[S]], target: STAR[WrappedArrow[T]], map: (S, T)*):
    QUIVER[WrappedArrow[S], WrappedArrow[T]] =
    wrapArrow[S, T](FiniteSetsArrow(
      source.getDot.asInstanceOf[DOT[S]],
      target.getDot.asInstanceOf[DOT[T]],
      Map(map:_*)
    ))

  override val foo = star(true, false)
  override val bar = star("X", "Y")
  override val foo2bar = quiver(foo, bar, true -> "X", false -> "Y")
  override val baz = star(1, 2, 3)
  override val foo2ImageOfBar = quiver(foo, baz, true -> 3, false -> 2)

//  override val foobar2baz = FiniteSetsBiArrow[FOO, BAR, BAZ](
//    foo, bar, baz, (true, "X") -> 2, (false, "X") -> 3, (true, "Y") -> 1, (false, "Y") -> 2
//  )

  override val monicBar2baz = quiver(
    bar, baz, "X" -> 2, "Y" -> 3
  )

//  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
//    foo2bar,
//    arrow[BAR, BAZ](bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
//    arrow[BAR, BAZ](bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
//  )
})

