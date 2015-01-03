package com.fdilke.bewl.actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.NaiveMonoid
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.topos.{Wrappings, Topos, ToposWithFixtures, GenericToposTests}
import org.scalatest.FunSpec

class RightActionsToposTest extends FunSpec {
  it("<placeholder for a proper test>") {
    // ...
  }
}

/*
class RightActionsToposTest extends GenericToposTests(new ToposWithFixtures {
  val triadicMonoid = {
    val (i, a, b, c, f, f2, g, g2) = ('i, 'a, 'b, 'c, 'f, 'f2, 'g, 'g2)
    val carrier = makeStar(i, a, b, c, f, f2, g, g2)
    val unit = makeNullaryOperator(carrier, i)
    val product = makeBinaryOperator(carrier,
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

  type TOPOS = Topos with Wrappings[FiniteSets.ELEMENT, triadicMonoid.RightAction, triadicMonoid.RightActionPrequiver]
  val topos : TOPOS = triadicMonoid.rightActions

//  import topos._

  type FOO = Symbol
  type BAR = String  // essentially the image of left multiplication by f
  type BAZ = Int

  override val foo = topos.star(triadicMonoid.rightRegularAction)

  private val barStar = makeStar("f", "f2", "b", "c")

  private val barTable = Map[(String, Symbol), String](

  )
  private val xx: (String, Symbol) => String = (s, m) => barTable((s, m))
  override val bar = triadicMonoid.rightAction(barStar)(xx(_, _)) // barTable((_, _)))
})
*/