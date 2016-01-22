
package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import org.scalatest.Matchers._

import scala.Function.untupled

class ToposOfGroupActionsTest

/*
class ToposOfGroupActionsTest extends GenericToposTests(
  new ToposWithFixtures {

    private val (i, x, y, a, b, c, d, e, f, f2, g, g2, r, s) =
      ('i,'x,'y,'a,'b,'c,'d,'e,'f,'f2,'g,'g2, 'r, 's)

    val group = groupOfUnits(monoidFromTable(
      i, a,
      a, i
    ))._1

    override val topos = FiniteSets.ToposOfGroupActions of group

    import topos._

    override type FOO = WRAPPER[Symbol]
    override type BAR = WRAPPER[String]
    override type BAZ = WRAPPER[String]

    override val foo = makeDot(group.regularAction)

    private val barDot: FiniteSets.DOT[String] = FiniteSetsUtilities.dot("x", "x'", "y")

    private val barFlip: String => String = Map("x" -> "x'", "x'" -> x)

    private val barMultiply: (String, Symbol) => String =
      (s, m) => if (m == a) barFlip(s) else s

    override val bar = makeDot(group.action(barDot)(barMultiply))

    private val bazDot: FiniteSets.DOT[Int] = FiniteSetsUtilities.dot(1, 2, 3, 4)

    // TODO: pack away local private stuff into a scope

    private val bazFlip: Int => Int = Map(1 -> 2, 2 -> 1, 3 -> 4, 4 -> 3)

    private val bazMultiply: (Int, Symbol) => Int =
      (s, m) => if (m == a) bazFlip(s) else s

    override val baz = makeDot(group.action(bazDot)(bazMultiply))

    override val foo2bar = functionAsArrow(foo, bar, Map(i -> "x", x -> "x", y -> "y"))
    override val foo2ImageOfBar = functionAsArrow(foo, baz, Map(i -> "y", x -> "x", y -> "y"))
    override val foobar2baz = bifunctionAsBiArrow(foo, bar, baz)(untupled (Map(
      (i, "x") -> "x", (x, "x") -> "x", (y, "x") -> "y",
      (i, "y") -> "y", (x, "y") -> "x", (y, "y") -> "y"
    )))
    override val monicBar2baz = functionAsArrow(bar, baz, Map("x" -> "x", "y" -> "y"))

    override def makeSampleDot() =
      makeDot(monoidOf3.action(barDot)(scalarMultiply))

    override def makeSampleArrow() =
      functionAsArrow(foo, bar, Map(
        i -> "x",
        x -> "x",
        y -> "x"
      ))

    override val equalizerSituation = {
      val wizDot: FiniteSets.DOT[Int] = FiniteSetsUtilities.dot(0, 1, 2, 3)

      def wizMultiply(n: Int, r: Symbol) : Int =
        if (n == 0)
          0
        else r match {
          case `i` => n
          case `x` => 1
          case `y` => 2
        }

      val wizAction = monoidOf3.action(wizDot)(wizMultiply)

      val wiz = makeDot(wizAction)

      val foo2wiz = functionAsArrow(foo, wiz, Map('i -> 1, 'x -> 1, 'y -> 2))

      type WIZ = WRAPPER[Int]
      type BINARY = WRAPPER[Boolean]
      val binaryDot : FiniteSets.DOT[Boolean] = FiniteSetsUtilities.dot(true, false)
      def binaryMultiply(b: Boolean, r: Symbol) : Boolean = b
      val binary = makeDot(monoidOf3.action(binaryDot)(binaryMultiply))
      new EqualizerSituation[FOO, WIZ, BINARY](
        foo2wiz,
        functionAsArrow(wiz, binary, Map(0 -> true, 1 -> true, 2 -> true, 3 -> true)),
        functionAsArrow(wiz, binary, Map(0 -> false, 1 -> true, 2 -> true, 3 -> true))
      )
    }
  }
) {
  import fixtures._
  import topos._

  describe("The Boolean property") {
    it("holds") {
      topos shouldBe 'boolean
    }
  }

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have size 2
    }

    it("works on the fixtures") {
      foo.globals shouldBe 'empty
      bar.globals shouldBe 'empty
      baz.globals shouldBe 'empty
    }
  }

  describe("Arrow enumeration") {
    // too slow! Belongs in a worksheet or app (pending optimization)
    it("also works on the fixtures") {
      (omega >> omega) should have size 6
    }

    ignore("...optional extras") {
      foo >> foo should have size 3
      foo >> bar should have size 2
      foo >> baz should have size 3

      // probably not - that would be if we were only counting isomorphisms
      (foo >> (omega > omega)) should have size 2
    }
  }
}
*/