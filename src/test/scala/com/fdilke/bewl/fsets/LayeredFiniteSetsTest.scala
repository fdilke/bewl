package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.{ARROW, DOT}
import com.fdilke.bewl.topos.{Wrappings, Topos, GenericToposTests, ToposWithFixtures}
import org.scalatest.Matchers._

class LayeredFiniteSetsTest extends GenericToposTests(new ToposWithFixtures {
  val topos = LayeredFiniteSets
  import topos._

  type FOO = WRAPPER[Boolean]
  type BAR = WRAPPER[String]
  type BAZ = WRAPPER[Int]

  def buildDot[T](elements: Seq[T]) =
    DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot(elements)

  def makeStar[T](elements: T*): topos.DOT[WRAPPER[T]] = star(buildDot(elements))

  def makeQuiver[S, T](source: topos.DOT[WRAPPER[S]], target: topos.DOT[WRAPPER[T]], map: (S, T)*) =
    functionAsQuiver(source, target, Map(map: _*))

  def makeBiQuiver[L, R, T](
    left: topos.DOT[WRAPPER[L]],
    right: topos.DOT[WRAPPER[R]],
    target: topos.DOT[WRAPPER[T]],
    mappings: ((L, R), T)*) =
    bifunctionAsBiQuiver(left, right, target) { (l, r) => Map(mappings:_*)((l, r)) }

  override val foo = makeStar(true, false)
  override val bar = makeStar("X", "Y", "Z")
  override val foo2bar = makeQuiver(foo, bar, true -> "X", false -> "Y")
  override val baz = makeStar(1, 2, 3, 4)
  override val foo2ImageOfBar = makeQuiver(foo, baz, true -> 3, false -> 2)

  override val foobar2baz = makeBiQuiver(
    foo, bar, baz,
    (true, "X") -> 2, (false, "X") -> 3,
    (true, "Y") -> 1, (false, "Y") -> 2,
    (true, "Z") -> 2, (false, "Z") -> 3
  )

  override val monicBar2baz = makeQuiver(
    bar, baz, "X" -> 2, "Y" -> 3, "Z" -> 1
  )

  private val sampleDotSource = buildDot(Seq(1, 2))
  private val sampleDotTarget = buildDot(Seq(true, false))

  override def makeSampleDot() = star(sampleDotSource)

  override def makeSampleArrow() = makeQuiver(star(sampleDotSource),
    star(sampleDotTarget), 1 -> true, 2 -> false)

  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
    foo2bar,
    makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
    makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
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