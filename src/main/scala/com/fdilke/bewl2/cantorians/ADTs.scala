package com.fdilke.bewl2.cantorians

trait Pitcher[
  PITCHER <: Pitcher[PITCHER, T],
  T
] {
  val head: T

  def tail: PITCHER
}

trait Catcher[
  SELF <: Catcher[SELF, T, U],
  T,
  U
] {
  self: SELF =>

  def either: Either[
    U,
    T => SELF
  ]

  final def apply[
    PITCHER <: Pitcher[PITCHER, T]
  ](
     pitcher: PITCHER
   ): U =
    either match {
      case Left(u) => u
      case Right(t2self) =>
        t2self(pitcher.head)(pitcher.tail)
    }
}