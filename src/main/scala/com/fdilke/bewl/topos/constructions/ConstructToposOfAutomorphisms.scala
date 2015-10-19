package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.{Memoize, ↔}
import com.fdilke.bewl.topos._
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}

trait ConstructToposOfAutomorphisms extends BaseTopos with LogicalOperations {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfAutomorphisms {
    lazy val topos: Topos = new ToposOfAutomorphisms


    class ToposOfAutomorphisms extends Topos /* with Wrappings[
        Ɛ.~,
        ({type λ[X <: Ɛ.~] = monoid.Action[X]})#λ,
        ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = monoid.ActionPreArrow[X, Y]})#λ
    ] */ {

      override type ~ = Ɛ.~

      // TODO: nothing's being overridden: simplify these. OR sort them out properly!
      override type x[S <: ~, T <: ~] = (S, T) with ~
      override type >[S <: ~, T <: ~] = (S => T) with ~

      override type UNIT = Ɛ.UNIT
      override type DOT[X <: ~] = Automorphism[X]
      override type ARROW[S <: ~, T <: ~] = AutomorphismArrow[S, T]
      override type TRUTH = Ɛ.TRUTH
      override val I: DOT[UNIT] = ???
      override val omega: DOT[TRUTH] = ???
      override val truth: ARROW[UNIT, TRUTH] = ???

      case class Automorphism[X <: ~](
        arrow: Ɛ.ARROW[X, X],
        inverse: Ɛ.ARROW[X, X]
      ) extends Dot[X] {
        val carrier = arrow.source
        override val toI: ARROW[X, UNIT] = ???

        override def sanityTest {
          if (carrier != arrow.target)
            throw new IllegalArgumentException("Source and target must match")
          if (carrier != inverse.source ||
            carrier != inverse.target ||
            (inverse o arrow) != carrier.identity ||
            (arrow o inverse) != carrier.identity)
            throw new IllegalArgumentException("Incorrect inverse")
          arrow.sanityTest
          inverse.sanityTest
        }

        override def `>Uncached`[Y <: ~](that: DOT[Y]): EXPONENTIAL[X, Y] = ???
        override def apply[Y <: ~](target: DOT[Y])(f: X => Y): ARROW[X, Y] = ???
        override def xUncached[Y <: ~](that: DOT[Y]): BIPRODUCT[X, Y] = ???
        override val globals: Traversable[ARROW[UNIT, X]] = ???
      }

      class AutomorphismArrow[S <: ~, T <: ~](
        override val source: DOT[S],
        override val target: DOT[T]
     ) extends Arrow[S, T] {
        override def \[U <: ~](monic: ARROW[U, T]): ARROW[S, U] = ???
        override def sanityTest: Unit = ???
        override def ?=(that: ARROW[S, T]): EQUALIZER[S] = ???
        override def apply(s: S) : T = ???
        override def o[R <: ~](that: ARROW[R, S]): ARROW[R, T] = ???
        override val chi: ARROW[T, TRUTH] = ???
      }
    }
  }
}
