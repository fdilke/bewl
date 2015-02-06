package com.fdilke.bewl.helper

case class ↔[A, B] (
  / : A => B,
  \ : B => A
) {
  def o[C](Δ: B ↔ C) = 
  	 new ↔[A, C](/ andThen Δ./, Δ.\ andThen \)
  def unary_~() =
    new ↔[B, A](\, /)
}
