package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.fsets.FiniteSets.FiniteSetsTopos
import com.fdilke.bewl2.topos.FunctionalPlumbing.{Equalizer, EqualizerReceiver}
import com.fdilke.bewl2.topos.Topos

import scala.language.postfixOps

object FiniteSets {
  implicit object FiniteSetsTopos extends Topos[Set] {
    override def sanityTest[S: Set]: Unit =
      println(s"Sanity testing ${ implicitly[Set[S]] }")

    override def sanityTest[S: Set, T: Set](arrow: S => T): Unit =
      for { s <- dot[S] }
        if (!dot[T].contains(arrow(s)))
          throw new IllegalArgumentException(
            "Broken arrow: " + functionAsString(arrow) +
              s" maps value $s outside domain ${dot[S]}"
          )

    override def functionAsString[S: Set, T: Set](
      arrow: S => T
    ): String =
      (dot[S].map {
        s => s -> arrow(s)
      } toMap) toString

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
    ): EqualizerReceiver[Set, S, X] => X = {
      @inline type R = S
      val subset: Set[R] = dot[S] filter { s =>
        func1(s) == func2(s)
      }
      _(new Equalizer[Set, S, R] {
        override def include(r: R): S = r
        override def restrict[Q: Set](
          arrow: Q => S
        ): Q => R =
          arrow
      })(subset)
    }

    override def productUncached[A : Set, B : Set]: Set[(A, B)] =
      for {
        a <- dot[A]
        b <- dot[B]
      } yield {
        a -> b
      }

    override implicit val terminator: Set[Unit] =
      Set({})

    override def to1[S: Set]: S => Unit =
      _ => {}

    override implicit val initial: Set[Void] =
      Set[Void]()

    override def from0[S: Set]: Void => S =
      _ => throw new IllegalArgumentException("You passed a Void")
  }
}

