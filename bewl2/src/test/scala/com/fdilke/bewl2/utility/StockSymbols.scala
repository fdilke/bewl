package com.fdilke.bewl2.utility

object StockSymbols {
  val Seq(a, b, c, d, e, r, s) =
    Seq[String]("a", "b", "c", "d", "e", "r", "s").map { Symbol(_) }
}
