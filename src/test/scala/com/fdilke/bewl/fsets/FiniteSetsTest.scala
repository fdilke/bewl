package com.fdilke.bewl.fsets

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}

class FiniteSetsTest extends GenericToposTests(new ToposWithFixtures {
  type TOPOS = FiniteSets.type
  val topos = FiniteSets

  import topos._

  type FOO = WrappedArrow[Boolean]
  type BAR = WrappedArrow[String]
  type BAZ = WrappedArrow[Int]

  def makeStar[T](elements: T*) = star(DiagrammaticFiniteSets.FiniteSetsDot(elements))

  def makeQuiver[S, T](source: STAR[WrappedArrow[S]], target: STAR[WrappedArrow[T]], map: (S, T)*) =
    functionAsQuiver(source, target, Map(map: _*))

  def makeBiQuiver[L, R, T](
    left: STAR[WrappedArrow[L]],
    right: STAR[WrappedArrow[R]],
    target: STAR[WrappedArrow[T]],
    mappings: ((L, R), T)*) =
    bifunctionAsBiQuiver(left, right, target, (l: L, r: R) => Map(mappings:_*)((l, r)))

  override val foo = makeStar(true, false)
  override val bar = makeStar("X", "Y")
  override val foo2bar = makeQuiver(foo, bar, true -> "X", false -> "Y")
  override val baz = makeStar(1, 2, 3)
  override val foo2ImageOfBar = makeQuiver(foo, baz, true -> 3, false -> 2)

  override val foobar2baz = makeBiQuiver(
    foo, bar, baz, (true, "X") -> 2, (false, "X") -> 3, (true, "Y") -> 1, (false, "Y") -> 2
  )

  override val monicBar2baz = makeQuiver(
    bar, baz, "X" -> 2, "Y" -> 3
  )

  private val sampleDotSource = DiagrammaticFiniteSets.FiniteSetsDot(Seq(1, 2))
  private val sampleDotTarget = DiagrammaticFiniteSets.FiniteSetsDot(Seq(true, false))

  override def makeSampleStar() = star(sampleDotSource)

  override def makeSampleQuiver() = makeQuiver(star(sampleDotSource), star(sampleDotTarget), 1 -> true, 2 -> false)

    override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
      foo2bar,
      makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
      makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
    )
})



