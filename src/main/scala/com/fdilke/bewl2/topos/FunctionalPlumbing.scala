package com.fdilke.bewl2.topos

object FunctionalPlumbing {
  trait Equalizer[DOT[_], S, R] {
    def include(r: R):S
    def restrict[Q: DOT](
      arrow: Q => S
    ): Q => R
  }

  abstract class EqualizerReceiver[DOT[_], S: DOT, X] {
    def apply[R <: S : DOT](
      equalizer: Equalizer[DOT, S, R]
    ): X
  }

  abstract class CharacteristicArrow[DOT[_], S: DOT, T: DOT, Ω] {
    val chi: T => Ω
    def restrict[R: DOT](
      arrow: R => T
    ): R => S
  }
}
