package com.fdilke.bewl.actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.NaiveMonoid
import com.fdilke.bewl.fsets.FiniteSetsUtilities
import com.fdilke.bewl.topos.{ToposWithFixtures, GenericToposTests}

abstract class RightActionsToposTest extends GenericToposTests(new ToposWithFixtures {
  val triadicMonoid: FiniteSets.NaiveMonoid[Symbol] = {
    val (i, a, b, c, f, f2, g, g2) = ('i, 'a, 'b, 'c, 'f, 'f2, 'g, 'g2)
    val carrier = FiniteSetsUtilities.makeStar(i, a, b, c, f, f2, g, g2)
    val unit = FiniteSetsUtilities.makeNullaryOperator(carrier, i)
    val product = FiniteSetsUtilities.makeBinaryOperator(carrier,
      (i, i) -> i, (i, a) -> a, (i, b) -> b, (i, c) -> c, (i, f) -> f, (i, f2) -> f2, (i, g) -> g, (i, g2) -> g2,
      (a, i) -> a, (a, a) -> a, (a, b) -> a, (a, c) -> a, (a, f) -> a, (a, f2) -> a,  (a, g) -> a, (a, g2) -> a,
      (b, i) -> b, (b, a) -> b, (b, b) -> b, (b, c) -> b, (b, f) -> b, (b, f2) -> b,  (b, g) -> b, (b, g2) -> b,
      (c, i) -> c, (c, a) -> c, (c, b) -> c, (c, c) -> c, (c, f) -> c, (c, f2) -> c,  (c, g) -> c, (c, g2) -> c,
      (f, i) -> f, (f, a) -> b, (f, b) -> c, (f, c) -> b, (f, f) -> f2,(f, f2) -> f,  (f, g) -> b, (f, g2) -> b,
      (f2,i) -> f2,(f2,a) -> c, (f2,b) -> b, (f2,c) -> c, (f2,f) -> f, (f2,f2) -> f2, (f2,g) -> c, (f2,g2) -> c,
      (g, i) -> g, (g, a) -> c, (g, b) -> a, (g, c) -> a, (g, f) -> a, (g, f2) -> a,  (g, g) -> g2,(g, g2) -> g,
      (g2,i) -> g2,(g2,a) -> a, (g2,b) -> c, (g2,c) -> c, (g2,f) -> c, (g2,f2) -> c,  (g2,g) -> g, (g2,g2) -> g2
    )
    NaiveMonoid[Symbol](carrier, unit, product)
  }

  override val topos = triadicMonoid.rightActions

  import topos._

  override type FOO = WRAPPER[Symbol]
  override type BAR = WRAPPER[String] // essentially the image of left multiplication by f
  override type BAZ = WRAPPER[Int]

  override val foo = star(triadicMonoid.rightRegularAction)

  private val barStar: FiniteSets.STAR[String] = FiniteSetsUtilities.makeStar("f", "f2", "b", "c")

  private val scalarMultiply: (String, Symbol) => String = (s, m) =>
    triadicMonoid.multiply(Symbol(s), m).name
  override val bar = star(triadicMonoid.rightAction(barStar)(scalarMultiply))


  override val baz = null.asInstanceOf[STAR[BAZ]]
  override val equalizerSituation = null
  override val foo2ImageOfBar = null.asInstanceOf[QUIVER[FOO, BAZ]]
  override val foo2bar = null.asInstanceOf[QUIVER[FOO, BAR]]
  override val foobar2baz = null
  override val monicBar2baz = null.asInstanceOf[QUIVER[BAR, BAZ]]

  override def makeSampleStar(): STAR[_ <: ELEMENT] =
    null.asInstanceOf[STAR[_ <: ELEMENT]]

  override def makeSampleQuiver(): QUIVER[_ <: ELEMENT, _ <: ELEMENT] =
    null.asInstanceOf[QUIVER[_ <: ELEMENT, _ <: ELEMENT]]

})
