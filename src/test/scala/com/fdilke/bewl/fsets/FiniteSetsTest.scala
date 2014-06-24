package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities
import com.fdilke.bewl.{GenericToposTests, ToposWithFixtures}
import FiniteSetsUtilities.{dot,arrow}

class FiniteSetsTest extends GenericToposTests(new ToposWithFixtures {
  type TOPOS = FiniteSets.type
  val topos = FiniteSets

  import topos._

  type FOO = Boolean
  type BAR = String
  type BAZ = Int

  override val foo: DOT[FOO] = dot[FOO](true, false)
  override val bar = dot[BAR]("X", "Y")
  override val foo2bar = arrow[FOO, BAR](foo, bar, true -> "X", false -> "Y")
  override val baz = dot[BAZ](1, 2, 3)
  override val foo2ImageOfBar = arrow[FOO, BAZ](foo, baz, true -> 3, false -> 2)

  override val foobar2baz = FiniteSetsBiArrow[FOO, BAR, BAZ](
    foo, bar, baz, (true, "X") -> 2, (false, "X") -> 3, (true, "Y") -> 1, (false, "Y") -> 2
  )

  override val monicBar2baz = arrow[BAR, BAZ](
    bar, baz, "X" -> 2, "Y" -> 3
  )

  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
    foo2bar,
    arrow[BAR, BAZ](bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
    arrow[BAR, BAZ](bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
  )
})

