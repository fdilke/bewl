package com.fdilke.bewl.helper

object StandardSymbols {
  val Seq(
    i, x, y, z, a, b, c, r, s, q,
    iso, injective, epic, monic,
    minimal, simple, section, retraction
  ) = Seq(
    "i", "x", "y", "z", "a", "b", "c", "r", "s", "q",
    "iso", "injective", "epic", "monic",
    "minimal", "simple", "section", "retraction"
  ) map {
    Symbol(_)
  }

  val Seq(
  commutative,
  boolean, source, target,
  left, right, sanityTest
  ) = Seq(
    "commutative",
    "boolean", "source", "target",
    "left", "right", "sanityTest"
  ) map {
    Symbol(_)
  }
}
