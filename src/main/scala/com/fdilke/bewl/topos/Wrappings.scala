package com.fdilke.bewl.topos

trait Wrappings[
  ~~,
  BASE,
  PREDOT[_ <: BASE],
  PREARROW[_ <: BASE, _ <: BASE],
  WRAPPER[T <: BASE] <: ~~
] {
  topos: Topos[~~] =>

  def makeDot[
    T <: BASE
  ](
    predot: PREDOT[T]
  ): DOT[WRAPPER[T]]

  def makeArrow[
    S <: BASE,
    T <: BASE
  ](
    prearrow: PREARROW[S, T]
  ): WRAPPER[S] > WRAPPER[T]

  def functionAsArrow[
    S <: BASE,
    T <: BASE
  ](
    source: DOT[WRAPPER[S]],
    target: DOT[WRAPPER[T]],
    f: S => T
  ): WRAPPER[S] > WRAPPER[T]

  def bifunctionAsBiArrow[
    L <: BASE,
    R <: BASE,
    T <: BASE
  ](
    left: DOT[WRAPPER[L]],
    right: DOT[WRAPPER[R]],
    target: DOT[WRAPPER[T]]
  )(
    bifunc: (L, R) => T
  ): BiArrow[WRAPPER[L], WRAPPER[R], WRAPPER[T]]

  final def bifunctionAsBiArrow[
    X <: BASE
  ](
    dot: DOT[WRAPPER[X]]
  )(
    bifunc: (X, X) => X
  ): BiArrow[
    WRAPPER[X],
    WRAPPER[X],
    WRAPPER[X]
  ] =
    bifunctionAsBiArrow[X, X, X](dot, dot, dot)(bifunc)

  // An implementation may have the ability to "unwrap" dots
  def unwrap[T <: BASE](
    dot: DOT[WRAPPER[T]]
  ): PREDOT[T] =
    ???
}

object Wrappings {
  type NO_WRAPPER[T] = T
}
