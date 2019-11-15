package com.fdilke.bewl.helper

object StandardSymbols {
  val Seq(
    i, x, y, z, a, b, c, r, s,
    iso, injective, epic, monic,
    boolean, source, target
  ) = Seq(
    "i", "x", "y", "z", "a", "b", "c", "r", "s",
    "iso", "injective", "epic", "monic",
    "boolean", "source", "target"
  ) map {
    Symbol(_)
  }
}
