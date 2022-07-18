package com.fdilke.bewl2

trait Mappable[CTXT[_]] {
  def map[A, B](
    a: CTXT[A],
    f: A => B
  ): CTXT[B]
}

object Mappable {
  implicit object IdentityMappable extends Mappable[
    [A] =>> A
  ] {
    override def map[A, B](
      a: A,
      f: A => B
    ):B =
      f(a)
  }
}

