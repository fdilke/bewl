package com.fdilke.bewl.helper

case class ⊕[A, B](
  _1: A,
  _2: B
) {
  override def toString =
    _1.toString + " ⊕ " + _2.toString
}

object ⊕ {
  def tupled[A, B, C](
    bifunc: (A, B) => C
  ): A ⊕ B => C = {
    case a ⊕ b =>
      bifunc(a, b)
  }

  implicit class RichObject[A](
    a: A
  ) {
    def ⊕[B](
      b: B
    ) =
      new ⊕(a, b)
  }
}
