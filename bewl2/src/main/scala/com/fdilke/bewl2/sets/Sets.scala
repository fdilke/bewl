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
    
  override def sanityTest[X: Set]: Unit = ()
  
  override def sanityTest[X: Set, Y: Set](f: X ~> Y): Unit = ()
  

  