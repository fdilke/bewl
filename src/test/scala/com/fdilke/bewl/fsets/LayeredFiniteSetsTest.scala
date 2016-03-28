package com.fdilke.bewl.fsets

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import org.scalatest.Matchers._

class LayeredFiniteSetsTest extends GenericToposTests[DiagrammaticFiniteSets.Element] (
  new ToposWithFixtures[DiagrammaticFiniteSets.Element] {
  val topos = LayeredFiniteSets
  import topos._

  type FOO = DiagrammaticFiniteSets.WrappedArrow[Boolean]
  type BAR = DiagrammaticFiniteSets.WrappedArrow[String]
  type BAZ = DiagrammaticFiniteSets.WrappedArrow[Int]

  def buildDot[T](elements: Seq[T]) =
    DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot(elements)

  def dot[T](elements: T*): DOT[DiagrammaticFiniteSets.WrappedArrow[T]] =
    makeDot(buildDot(elements))

  def arrow[S, T](
    source: topos.DOT[DiagrammaticFiniteSets.WrappedArrow[S]],
    target: topos.DOT[DiagrammaticFiniteSets.WrappedArrow[T]],
    map: (S, T)*
  ) =
    functionAsArrow(source, target, Map(map: _*))

  def makeBiArrow[L, R, T](
    left: topos.DOT[DiagrammaticFiniteSets.WrappedArrow[L]],
    right: topos.DOT[DiagrammaticFiniteSets.WrappedArrow[R]],
    target: topos.DOT[DiagrammaticFiniteSets.WrappedArrow[T]],
    mappings: ((L, R), T)*
  ) =
    bifunctionAsBiArrow(left, right, target) { (l, r) => Map(mappings:_*)((l, r)) }

  override val foo = dot(true, false)
  override val bar = dot("X", "Y", "Z")
  override val foo2bar = arrow(foo, bar, true -> "X", false -> "Y")
  override val baz = dot(1, 2, 3, 4)
  override val foo2ImageOfBar = arrow(foo, baz, true -> 3, false -> 2)

  override val foobar2baz = makeBiArrow(
    foo, bar, baz,
    (true, "X") -> 2, (false, "X") -> 3,
    (true, "Y") -> 1, (false, "Y") -> 2,
    (true, "Z") -> 2, (false, "Z") -> 3
  )

  override val monicBar2baz = arrow(
    bar, baz, "X" -> 2, "Y" -> 3, "Z" -> 1
  )

  private val sampleDotSource: DiagrammaticFiniteSets.DOT[Int] = buildDot(Seq(1, 2))
  private val sampleDotTarget: DiagrammaticFiniteSets.DOT[Boolean] = buildDot(Seq(true, false))

  override def makeSampleDot(): DOT[DiagrammaticFiniteSets.WrappedArrow[Int]] =
    makeDot[Int](sampleDotSource)

  override def makeSampleArrow():
    >[
      DiagrammaticFiniteSets.WrappedArrow[Int],
      DiagrammaticFiniteSets.WrappedArrow[Boolean]
    ] = arrow(
    makeDot(sampleDotSource),
    makeDot(sampleDotTarget),
    1 -> true,
    2 -> false
  )

  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
    foo2bar,
    arrow(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
    arrow(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
  )
}) {
  import fixtures._
  import fixtures.topos._

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have('size(2))
    }

    it("works on the fixtures") {
      foo.globals should have('size(2))
      bar.globals should have('size(3))
      baz.globals should have('size(4))
    }
  }
}