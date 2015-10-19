package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.{Memoize, ↔}
import com.fdilke.bewl.topos._
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}

trait ConstructToposOfAutomorphisms extends BaseTopos with LogicalOperations {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfAutomorphisms {
    lazy val build: Topos with Wrappings[
      Ɛ.~,
      ({type λ[X <: Ɛ.~] = Ɛ.ARROW[X, X]})#λ,
      ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = AutomorphismPreArrow[X, Y]})#λ
    ] =
      new ToposOfAutomorphisms

    case class AutomorphismPreArrow[
      S <: Ɛ.~,
      T <: Ɛ.~
    ](
      source: ARROW[S, S],
      target: ARROW[T, T],
      arrow:  ARROW[S, T]
    )

    class ToposOfAutomorphisms extends Topos with Wrappings[
        Ɛ.~,
        ({type λ[X <: Ɛ.~] = Ɛ.ARROW[X, X]})#λ,
        ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = AutomorphismPreArrow[X, Y]})#λ
    ] {

      override type ~ = Ɛ.~

      override type UNIT = Ɛ.UNIT
      override type DOT[X <: ~] = Automorphism[X]
      override type ARROW[S <: ~, T <: ~] = AutomorphismArrow[S, T]
      override type TRUTH = Ɛ.TRUTH
      override val I: DOT[UNIT] = trivialAutomorphism(Ɛ.I)
      override val omega: DOT[TRUTH] = trivialAutomorphism(Ɛ.omega)
      override val truth: ARROW[UNIT, TRUTH] =
        AutomorphismArrow(I, omega, Ɛ.truth)

      case class Automorphism[X <: ~](
        arrow: Ɛ.ARROW[X, X],
        inverse: Ɛ.ARROW[X, X]
      ) extends Dot[X] {
        automorphism =>

        val carrier = arrow.source
        override val toI: ARROW[X, UNIT] =
          AutomorphismArrow(automorphism, I, carrier.toI)

        override def sanityTest {
          arrow.sanityTest
          inverse.sanityTest
          if (carrier != arrow.target)
            throw new IllegalArgumentException("Source and target must match")
          if (carrier != inverse.source ||
            carrier != inverse.target ||
            (inverse o arrow) != carrier.identity ||
            (arrow o inverse) != carrier.identity)
            throw new IllegalArgumentException("Incorrect inverse")
        }

        override def `>Uncached`[Y <: ~](that: DOT[Y]): EXPONENTIAL[X, Y] = ???
        override def apply[Y <: ~](target: DOT[Y])(f: X => Y): ARROW[X, Y] =
          AutomorphismArrow(automorphism, target, carrier(target.carrier)(f))
        override def xUncached[Y <: ~](that: DOT[Y]): BIPRODUCT[X, Y] = {
          val productCarrier = carrier x that.carrier
          new Automorphism(
            (arrow o productCarrier.π0) x (that.arrow o productCarrier.π1),
            (inverse o productCarrier.π0) x (that.inverse o productCarrier.π1)
          ) with BiproductDot[X, Y, X x Y] {
            override val left: DOT[X] = automorphism
            override val right: DOT[Y] = that
            override def pair(l: X, r: Y): x[X, Y] = productCarrier.pair(l, r)
          }
        }
        override val globals: Traversable[ARROW[UNIT, X]] =
          (Ɛ.I >> carrier) filter { global =>
            (arrow o global) == global
          } map { global =>
            AutomorphismArrow(I, automorphism, global)
          }
      }

      case class AutomorphismArrow[S <: ~, T <: ~](
        override val source: DOT[S],
        override val target: DOT[T],
        private val arrow: Ɛ.ARROW[S, T]
     ) extends Arrow[S, T] {
        override def \[U <: ~](monic: ARROW[U, T]): ARROW[S, U] = ???
        override def sanityTest: Unit = {
          source.sanityTest
          target.sanityTest
          arrow.sanityTest
          if ((arrow o source.arrow) != (target.arrow o arrow))
            throw new IllegalArgumentException("Arrow does not respect automorphisms")
        }
        override def ?=(that: ARROW[S, T]): EQUALIZER[S] = ???
        override def apply(s: S) : T = ???
        override def o[R <: ~](that: ARROW[R, S]): ARROW[R, T] =
          AutomorphismArrow(that.source, target, arrow o that.arrow)
        override lazy val chi: ARROW[T, TRUTH] =
          AutomorphismArrow(target, omega, arrow.chi)
      }

      def trivialAutomorphism[X <: Ɛ.~](dot: Ɛ.DOT[X]) =
        Automorphism(dot.identity, dot.identity)

      override type WRAPPER[T <: Ɛ.~] = T

      override def functionAsArrow[
        S <: ~,
        T <: ~
      ] (
        source: Automorphism[S],
        target: Automorphism[T],
        f: S => T
      ): AutomorphismArrow[S, T] =
        ???

      override def makeArrow[
        S <: ~,
        T <: ~
      ] (
        prearrow: AutomorphismPreArrow[S, T]
      ): AutomorphismArrow[S, T] = ???

      override def makeDot[
        T <: ~
      ] (
        predot: Ɛ.ARROW[T, T]
      ): Automorphism[T] = // TODO: memoize AND check iso
        Automorphism(predot, predot.inverse)

      override def bifunctionAsBiArrow[
        L <: ~,
        R <: ~,
        T <: ~
      ] (
        left: Automorphism[L],
        right: Automorphism[R],
        target: Automorphism[T])(bifunc: (L, R) => T
      ): BiArrow[L, R, T] =
        (left x right).biArrow(target) { bifunc }
    }
  }
}
