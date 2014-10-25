package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.{ARROW, DOT}
import com.fdilke.bewl.topos.{BetterWrappings, Topos, GenericToposTests, ToposWithFixtures}

class LayeredFiniteSetsTest extends GenericToposTests(new ToposWithFixtures {
  val topos = LayeredFiniteSets
  import topos._

  type FOO = WRAPPER[Boolean]
  type BAR = WRAPPER[String]
  type BAZ = WRAPPER[Int]

  def buildDot[T](elements: Seq[T]) =
    DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot(elements)

  def makeStar[T](elements: T*): STAR[WRAPPER[T]] = star(buildDot(elements))

  def makeQuiver[S, T](source: STAR[WRAPPER[S]], target: STAR[WRAPPER[T]], map: (S, T)*) =
    functionAsQuiver(source, target, Map(map: _*))

  def makeBiQuiver[L, R, T](
    left: STAR[WRAPPER[L]],
    right: STAR[WRAPPER[R]],
    target: STAR[WRAPPER[T]],
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

  private val sampleDotSource = buildDot(Seq(1, 2))
  private val sampleDotTarget = buildDot(Seq(true, false))

  override def makeSampleStar() = star(sampleDotSource)

  override def makeSampleQuiver() = makeQuiver(star(sampleDotSource),
    star(sampleDotTarget), 1 -> true, 2 -> false)

    override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
      foo2bar,
      makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
      makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
    )
})



