package com.fdilke.bewl2.topos

import org.scalatest.funspec.AnyFunSpec

abstract class GenericToposTests[
  DOT[_]: Topos,
  FOO : DOT,
  BAR : DOT,
  BAZ : DOT
] extends AnyFunSpec {

  val foo2bar : FOO => BAR
  val foo2ImageOfBar : FOO => BAZ
  //  TODO: sort this out
  //  val foobar2baz : BiArrow[
  //    FOO,
  //    BAR,
  //    BAZ
  //  ]
  val monicBar2baz: BAR => BAZ
  final lazy val foo2baz = foo2ImageOfBar // a convenient alias

  // TODO: equalizer situation, with sanity tests
}
