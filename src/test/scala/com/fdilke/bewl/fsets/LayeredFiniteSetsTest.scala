package com.fdilke.bewl.fsets

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}

// TODO: worst code ever. fix it!!

class LayeredFiniteSetsTest extends GenericToposTests(new ToposWithFixtures {
  type TOPOS = LayeredFiniteSets.type
  val topos = LayeredFiniteSets

  import topos._

  type FOO = WrappedArrow[Boolean]
  type BAR = WrappedArrow[String]
  type BAZ = WrappedArrow[Int]

  def buildDot[T](elements: Seq[T]) =
    DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot(elements)

  def makeStar[T](elements: T*) = constructStar(buildDot(elements))

  def constructStar[T](dot: DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot[T]) =
    star(dot.asInstanceOf[topos.topos.DOT[T]])

  def makeQuiver[S, T](source: STAR[WrappedArrow[S]], target: STAR[WrappedArrow[T]], f: S => T) =
    functionAsQuiver(source, target, f)

  def buildQuiver[S, T](source: AdapterStar[WrappedArrow[S]], target: AdapterStar[WrappedArrow[T]], map: (S, T)*) =
    makeQuiver(source.asInstanceOf[topos.STAR[WrappedArrow[S]]],
      target.asInstanceOf[topos.STAR[WrappedArrow[T]]],
      Map(map: _*))

  def makeBiQuiver[L, R, T](
    left: STAR[WrappedArrow[L]],
    right: STAR[WrappedArrow[R]],
    target: STAR[WrappedArrow[T]],
    mappings: ((L, R), T)*) =
    bifunctionAsBiQuiver(left, right, target, (l: L, r: R) => Map(mappings:_*)((l, r)))

  override val foo = makeStar(true, false)
  override val bar = makeStar("X", "Y")
  override val foo2bar = buildQuiver(foo, bar, true -> "X", false -> "Y")
  override val baz = makeStar(1, 2, 3)
  override val foo2ImageOfBar = buildQuiver(foo, baz, true -> 3, false -> 2)

  override val foobar2baz = makeBiQuiver(
    foo, bar, baz, (true, "X") -> 2, (false, "X") -> 3, (true, "Y") -> 1, (false, "Y") -> 2
  )

  override val monicBar2baz = buildQuiver(
    bar, baz, "X" -> 2, "Y" -> 3
  )

  private val sampleDotSource = buildDot(Seq(1, 2))
  private val sampleDotTarget = buildDot(Seq(true, false))

  override def makeSampleStar() = star(sampleDotSource.
    asInstanceOf[topos.topos.DOT[Int]]
  )

  override def makeSampleQuiver() = buildQuiver(constructStar(sampleDotSource),
    constructStar(sampleDotTarget), 1 -> true, 2 -> false)

    override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
      foo2bar,
      buildQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
      buildQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
    )
})



