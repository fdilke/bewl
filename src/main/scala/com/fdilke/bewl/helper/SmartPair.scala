package com.fdilke.bewl.helper

case class ⊕[A, B](_1: A, _2: B)

object ⊕ {
  implicit class RichObject[A](a: A) {
    def ⊕[B](b: B) = new ⊕(a, b)
  }
}
