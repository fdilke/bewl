package com.fdilke.bewl.helper

case class ↔[A, B] (
  / : A => B,
  \ : B => A
) {
  def unary_~() =
    new ↔[B, A](\, /)
}
