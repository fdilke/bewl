
package com.fdilke.bewl.actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.topos.{ToposWithFixtures, GenericToposTests}

abstract class RightActionsToposTest extends GenericToposTests(new ToposWithFixtures {

  private val (i, x, y) = ('i, 'x, 'y)

  val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    )

  override val topos = monoidOf3.rightActions

  import topos._

  override type FOO = WRAPPER[Symbol]
  override type BAR = WRAPPER[String]
  override type BAZ = WRAPPER[Int]

  override val foo = star(monoidOf3.rightRegularAction)

  private val barStar: FiniteSets.STAR[String] = FiniteSetsUtilities.makeStar("i", "x", "y")

  private val scalarMultiply: (String, Symbol) => String = (s, m) =>
    monoidOf3.multiply(Symbol(s), m).name
  override val bar = star(monoidOf3.rightAction(barStar)(scalarMultiply))

  private val bazStar: FiniteSets.STAR[Int] = FiniteSetsUtilities.makeStar(0, 1, 2, 3)

  private def bazMultiply(n: Int, r: Symbol) : Int =
    if (n == 0)
      0
    else r match {
      case `i` => n
      case `x` => 1
      case `y` => 2
    }

  val bazAction = monoidOf3.rightAction(bazStar)(bazMultiply)

  override val baz = star(bazAction)
  override val foo2ImageOfBar = functionAsQuiver(foo, baz, Map('i -> 1, 'x -> 1, 'y -> 2))
  override val foo2bar = functionAsQuiver(foo, bar, Map('i -> "x", 'x -> "x", 'y -> "y"))
  private def foobar2BazFunc(a: Symbol, b : String) = Map[(Symbol, String), Int](
    (i, "i") -> 1, (x, "i") -> 2, (y, "i") -> 1,
    (i, "x") -> 2, (x, "x") -> 1, (y, "x") -> 1,
    (i, "y") -> 1, (x, "y") -> 2, (y, "y") -> 2
  )((a, b))
  override val foobar2baz = bifunctionAsBiQuiver(foo, bar, baz)(foobar2BazFunc)
  override val monicBar2baz = functionAsQuiver(bar, baz, Map("i" -> 3, "x" -> 1, "y" -> 2))

  override def makeSampleStar() =
    star(bazAction)

  override def makeSampleQuiver() =
    functionAsQuiver(foo, bar, Map(i -> "x", x -> "x", y -> "x"))

  override val equalizerSituation = {
    type BINARY = WRAPPER[Boolean]
    val binaryStar : FiniteSets.STAR[Boolean] = FiniteSetsUtilities.makeStar(true, false)
    def binaryMultiply(b: Boolean, r: Symbol) : Boolean = b
    val binary = star(monoidOf3.rightAction(binaryStar)(binaryMultiply))
    new EqualizerSituation[FOO, BAZ, BINARY](
      foo2baz,
      functionAsQuiver(baz, binary, Map(0 -> true, 1 -> true, 2 -> true, 3 -> true)),
      functionAsQuiver(baz, binary, Map(0 -> false, 1 -> true, 2 -> true, 3 -> true))
    )
  }
})
