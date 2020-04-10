package com.fdilke.bewl2.topos

object FunctionalPlumbing {
  trait Equalizer[DOT[_], S, R] {
    val include: R => S
    def restrict[Q: DOT](
      arrow: Q => S
    ): Q => R
  }

  abstract class EqualizerReceiver[DOT[_], S: DOT, X] {
    def apply[R <: S: DOT](
      equalizer: Equalizer[DOT, S, R]
    ): X
  }

  abstract class CharacteristicArrow[DOT[_], S: DOT, T: DOT, Ω] {
    val chi: T => Ω
    def restrict[R: DOT](
      arrow: R => T
    ): R => S
  }

  // I'm sure a logician could tell you why this particularly twisty bit of
  // functional plumbing is necessary.
  @inline def withUnit[
    DOT[_]: Topos,
    S: DOT,
    T: DOT
  ](
    s: S
  )(
    f: Unit => T
  ): T =
    f(
      (Topos[DOT].to1[S]: S => Unit)(
        s
      )
    )

  @inline def collapse[
    DOT[_]: Topos,
    A: DOT,
    B: DOT
  ](
    f: A => (Unit => B)
  ): A => B =
    a => withUnit(a)(f(a))

  @inline def collapse[
    DOT[_]: Topos,
    A: DOT,
    B: DOT,
    C: DOT
  ](
    f: (A, B) => (Unit => C)
  ): (A, B) => C =
    (a, b) => withUnit(a)(f(a, b))
}
