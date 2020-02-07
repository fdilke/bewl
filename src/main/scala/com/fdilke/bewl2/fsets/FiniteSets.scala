package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.fsets.FiniteSets.FiniteSetsTopos
import com.fdilke.bewl2.topos.Topos

object FiniteSets {
  implicit object FiniteSetsTopos extends Topos[Set] {
    override def sanityTest[S: Set]: Unit =
      println(s"Sanity testing ${ implicitly[Set[S]] }")

    override def sanityTest[S: Set, T: Set](arrow: S => T): Unit =
      println(s"Sanity testing a function of sets... how?")

    override def compareFunctions[S: Set, T: Set](
      func: S=> T,
      func2: S => T
    ): Boolean =
      dot[S] forall { s =>
        func(s) == func2(s)
      }

    override def equalize[
      S: Set,
      T: Set,
      X
    ](
      func1: S => T,
      func2: S => T
    ): EqualizerReceiver[S, X] => X = {
      @inline type R = S
      val subset: Set[R] = dot[S] filter { s =>
        func1(s) == func2(s)
      }
      _(new Equalizer[S, R] {
        override def include(r: R): S = r
        override def restrict[Q: Set](
          arrow: Q => S
        ): Q => R =
          arrow
      })(subset)
    }
  }
}

