package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.Monad

implicit object Sets extends Topos[Set, Monad.IDENTITY]:

  override def equalArrows[X: Set, Y: Set](
   f1: X => Y,
   f2: X => Y
 ): Boolean =
    implicitly[Set[X]].forall { x =>
      f1(x) == f2(x)
    }

