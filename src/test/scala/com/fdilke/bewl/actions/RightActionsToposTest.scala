
package com.fdilke.bewl.actions

import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.fsets.FiniteSets.NaiveMonoid
import com.fdilke.bewl.topos.{ToposWithFixtures, GenericToposTests}

abstract class RightActionsToposTest extends GenericToposTests(new ToposWithFixtures {

  val monoidOf3: FiniteSets.NaiveMonoid[Symbol] = {
    val (i, x, y) = ('i, 'x, 'y)
    val carrier = FiniteSetsUtilities.makeStar(i, x, y)
    val unit = FiniteSetsUtilities.makeNullaryOperator(carrier, i)
    val product = FiniteSetsUtilities.makeBinaryOperator(carrier,
      (i, i) -> i, (i, x) -> x, (i, y) -> y,
      (x, i) -> x, (x, x) -> x, (x, y) -> y,
      (y, i) -> y, (y, x) -> x, (y, y) -> y
    )
    NaiveMonoid[Symbol](carrier, unit, product)
  }

  override val topos = monoidOf3.rightActions

  import topos._

  override type FOO = WRAPPER[Symbol]
  override type BAR = WRAPPER[String]
  override type BAZ = WRAPPER[Int]

  override val foo = star(monoidOf3.rightRegularAction)

  private val barStar: FiniteSets.STAR[String] = FiniteSetsUtilities.makeStar("x", "y")

  private val scalarMultiply: (String, Symbol) => String = (s, m) =>
    monoidOf3.multiply(Symbol(s), m).name
  override val bar = star(monoidOf3.rightAction(barStar)(scalarMultiply))

  private val bazStar: FiniteSets.STAR[Int] = FiniteSetsUtilities.makeStar(0, 1, 2)

  private def bazMultiply(n: Int, r: Symbol) : Int =
    if (n == 0)
      0
    else r match {
      case 'i => n
      case 'x => 1
      case 'y => 2
    }
  val bazAction = monoidOf3.rightAction(bazStar)(bazMultiply)

  override val baz = star(bazAction)
  override val equalizerSituation = null
  override val foo2ImageOfBar = functionAsQuiver(foo, baz, Map[FOO, BAZ]('i -> 1, 'x -> 1, 'y -> 2))
  override val foo2bar = functionAsQuiver(foo, bar, Map[FOO, BAR]('i -> "x", 'x -> "x", 'y -> "x"))
  private def foobar2BazFunc(x: Symbol, y : String) = Map[(Symbol, String), Int](
    ('i, "x") -> 2, ('x, "x") -> 1, ('y, "x") -> 1,
    ('i, "y") -> 1, ('x, "y") -> 2, ('y, "y") -> 2
  )((x, y))
  override val foobar2baz = bifunctionAsBiQuiver(foo, bar, baz)(foobar2BazFunc)
  override val monicBar2baz = functionAsQuiver(bar, baz, Map[BAR, BAZ]("x" -> 1, "y" -> 2))

  override def makeSampleStar() =
    star(bazAction)

  override def makeSampleQuiver() =
    functionAsQuiver(foo, bar, Map[FOO, BAR]('i -> "x", 'x -> "x", 'y -> "x"))

})
