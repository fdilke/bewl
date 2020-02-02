package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.fsets.FiniteSets._
import com.fdilke.bewl2.topos.GenericToposTests

private object Fixtures {
  implicit val foo: Set[Boolean] = Set(true, false)
  implicit val bar: Set[String] = Set("X", "Y", "Z")
  implicit val baz: Set[Int] = Set(1, 2, 3, 4)
}

import Fixtures._

class FiniteSetsTest extends GenericToposTests[Set, Boolean, String, Int] {
  override val foo2bar = Map(
    true -> "X",
    false -> "Y"
  )
  override val foo2ImageOfBar = Map(
    true -> 3,
    false -> 2
  )
  override val monicBar2baz = Map(
    "X" -> 2,
    "Y" -> 3,
    "Z" -> 1
  )
}
