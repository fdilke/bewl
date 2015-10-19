
package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import org.scalatest.Matchers._

import scala.Function.untupled

abstract class TooSlowToposOfActionsTest extends GenericToposTests(new ToposWithFixtures {

  private val (i, x, y) = ('i, 'x, 'y)

  val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

  override val topos = FiniteSets.ToposOfActions.forMonoid(monoidOf3)

  import topos._

  override type FOO = WRAPPER[Symbol]
  override type BAR = WRAPPER[String]
  override type BAZ = WRAPPER[Int]

  override val foo = makeDot(monoidOf3.regularAction)

  private val barDot: FiniteSets.DOT[String] = FiniteSetsUtilities.dot("i", "x", "y")

  private val scalarMultiply: (String, Symbol) => String =
    (s, m) => monoidOf3.multiply(Symbol(s), m).name
  override val bar = makeDot(monoidOf3.action(barDot)(scalarMultiply))

  private val bazDot: FiniteSets.DOT[Int] = FiniteSetsUtilities.dot(0, 1, 2, 3)

  private def bazMultiply(n: Int, r: Symbol) : Int =
    if (n == 0)
      0
    else r match {
      case `i` => n
      case `x` => 1
      case `y` => 2
    }

  val bazAction = monoidOf3.action(bazDot)(bazMultiply)

  override val baz = makeDot(bazAction)
  override val foo2ImageOfBar = functionAsArrow(foo, baz, Map('i -> 1, 'x -> 1, 'y -> 2))
  override val foo2bar = functionAsArrow(foo, bar, Map('i -> "x", 'x -> "x", 'y -> "y"))
  override val foobar2baz = bifunctionAsBiArrow(foo, bar, baz)(untupled (Map[(Symbol, String), Int](
    (i, "i") -> 1, (x, "i") -> 2, (y, "i") -> 1,
    (i, "x") -> 2, (x, "x") -> 1, (y, "x") -> 1,
    (i, "y") -> 1, (x, "y") -> 2, (y, "y") -> 2
  )))
  override val monicBar2baz = functionAsArrow(bar, baz, Map("i" -> 3, "x" -> 1, "y" -> 2))

  override def makeSampleDot() =
    makeDot(bazAction)

  override def makeSampleArrow() =
    functionAsArrow(foo, bar, Map(
      i -> "x", 
      x -> "x", 
      y -> "x"
    ))

  override val equalizerSituation = {
    type BINARY = WRAPPER[Boolean]
    val binaryDot : FiniteSets.DOT[Boolean] = FiniteSetsUtilities.dot(true, false)
    def binaryMultiply(b: Boolean, r: Symbol) : Boolean = b
    val binary = makeDot(monoidOf3.action(binaryDot)(binaryMultiply))
    new EqualizerSituation[FOO, BAZ, BINARY](
      foo2baz,
      functionAsArrow(baz, binary, Map(0 -> true, 1 -> true, 2 -> true, 3 -> true)),
      functionAsArrow(baz, binary, Map(0 -> false, 1 -> true, 2 -> true, 3 -> true))
    )
  }
}) {
  import fixtures._
  import fixtures.topos._

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have size 2
    }

    it("works on the fixtures") {
      foo.globals should have size 0
      bar.globals should have size 0
      baz.globals should have size 1
    }
  }

  describe("Arrow enumeration") {
    // too slow! Belongs in a worksheet or app (pending optimization)
    it("also works on the fixtures") {
      (omega >> omega).size shouldBe 6
    }

    ignore("...optional extras") {
      foo >> foo should have size 3
      foo >> bar should have size 3
      foo >> baz should have size 4

      // probably not - that would be if we were only counting isomorphisms
      (foo >> (omega > omega)) should have size 2
    }
  }
}