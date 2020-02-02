package com.fdilke.bewl2.topos

import org.scalatest.funspec.AnyFunSpec

abstract class ToposFixtures[DOT[_]: Topos] {

}

class GenericToposTests[DOT[_]: Topos](fixtures: ToposFixtures[DOT]) extends AnyFunSpec {

}
