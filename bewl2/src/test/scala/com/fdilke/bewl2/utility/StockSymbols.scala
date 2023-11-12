package com.fdilke.bewl2.utility

object StockSymbols {
  val Seq(a, b, c, d, e, r, s, i, x, y) =
    Seq[String]("a", "b", "c", "d", "e", "r", "s", "i", "x", "y").map { Symbol(_) }
}
