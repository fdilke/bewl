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
  
  inline def apply[C[_]: Mappable]: Mappable[C] =
    implicitly

  implicit class RichMappable[C[_]: Mappable, X](
    mappable: C[X]
  ) {
    def map[Y](
      f: X => Y
    ): C[Y] =
      summon[Mappable[C]].map(mappable, f)
  }
}

