package com.fdilke.bewl.helper

object StandardSymbols {
  val Seq(
    i,
    x,
    y,
    z,
    a,
    b,
    c,
    d,
    e,
    f,
    g,
    f2,
    g2,
    r,
    s,
    q
  ) = Seq(
    "i",
    "x",
    "y",
    "z",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "f2",
    "g2",
    "r",
    "s",
    "q"
  ).map {
    Symbol(_)
  }

  val Seq(
    iso,
    injective,
    epic,
    monic,
    minimal,
    simple,
    section,
    retraction,
    commutative,
    boolean,
    source,
    target,
    left,
    right,
    sanityTest
  ) = Seq(
    "iso",
    "injective",
    "epic",
    "monic",
    "minimal",
    "simple",
    "section",
    "retraction",
    "commutative",
    "boolean",
    "source",
    "target",
    "left",
    "right",
    "sanityTest"
  ).map {
    Symbol(_)
  }
}
