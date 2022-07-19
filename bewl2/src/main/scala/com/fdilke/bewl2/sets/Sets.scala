package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.Mappable

implicit object Sets extends Topos[Set, [A] =>> A]:

  override def equalArrows[X: Set, Y: Set](
   f1: X => Y,
   f2: X => Y
 ): Boolean =
    implicitly[Set[X]].forall { x =>
      f1(x) == f2(x)
    }

  override def uncachedProductObject[
    X: Set,
    Y: Set
  ]: Set[(X, Y)] =
    for {
      x <- dot[X]
      y <- dot[Y]
    } yield
      (x, y)

  override def productMagic[A: Set, B: Set](
    a: A,
    b: B
  ): (A, B) =
    (a, b)

  override def sanityTest[X: Set]: Unit = ()
  
  override def sanityTest[X: Set, Y: Set](
    f: X ~> Y
  ): Unit =
    dot[X].foreach { x =>
      if (!dot[Y].contains(f(x)))
        throw new IllegalArgumentException("target outside range")
    }


  

  