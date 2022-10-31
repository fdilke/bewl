package com.fdilke.bewl2.utility

object StockSymbols {
  val Seq(a, b, c, d, e) =
    Seq[String]("a", "b", "c", "d", "e").map { Symbol(_) }
}
