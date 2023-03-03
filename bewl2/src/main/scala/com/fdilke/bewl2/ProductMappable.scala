package com.fdilke.bewl2

trait ProductMappable[CTXT[_]]:
  def map[A, B](
    a: CTXT[A],
    f: A => B
  ): CTXT[B]

  def productMagic[X, Y](
    ca: CTXT[X],
    cb: CTXT[Y]
  ): CTXT[(X, Y)]

object ProductMappable {
  implicit object IdentityMappable extends ProductMappable[
    [A] =>> A
  ]:
    override def map[A, B](
      a: A,
      f: A => B
    ):B =
      f(a)

    override def productMagic[X, Y](
      x: X,
      y: Y
    ): (X, Y) =
      (x, y)
  
  inline def apply[C[_]: ProductMappable]: ProductMappable[C] =
    implicitly

  extension[C[_]: ProductMappable, X](
    cx: C[X]
  )
    def map[Y](
      f: X => Y
    ): C[Y] =
      summon[ProductMappable[C]].map(cx, f)

    def ⊕[Y](cy: C[Y]): C[(X, Y)] =
      summon[ProductMappable[C]].productMagic(cx, cy)      
}

