package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.Memoize
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

      override val I = trivialAutomorphism(Ɛ.I)
      override val omega = trivialAutomorphism(Ɛ.omega)
      override val truth = AutomorphismArrow(I, omega, Ɛ.truth)

      case class Automorphism[X <: ~](
        arrow: Ɛ.ARROW[X, X],
        inverse: Ɛ.ARROW[X, X]
      ) extends Dot[X] { automorphism =>
        val carrier = arrow.source

        override lazy val toI =
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

        override def apply[Y <: ~](target: DOT[Y])(f: X => Y) =
          AutomorphismArrow(automorphism, target, carrier(target.carrier)(f))

        override def xUncached[Y <: ~](that: DOT[Y]) = {
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
        override def `>Uncached`[Y <: ~](that: DOT[Y]) = {
          val exponentialCarrier = carrier > that.carrier
          new Automorphism(
            exponentialCarrier.transpose(exponentialCarrier) {
              (e, x) => that.arrow(e(inverse(x)))
            },
            exponentialCarrier.transpose(exponentialCarrier) {
              (e, x) => that.inverse(e(arrow(x)))
            }
          ) with ExponentialDot[X, Y, X > Y] { exponentialAutomorphism =>
            override val source = automorphism
            override val target = that
            override def transpose[R <: ~](biArrow: BiArrow[R, X, Y]): ARROW[R, X > Y] = 
              AutomorphismArrow(
                biArrow.product.left,
                exponentialAutomorphism,
                exponentialCarrier.transpose(
                  (biArrow.product.left.carrier x biArrow.product.right.carrier).biArrow(
                    biArrow.arrow.arrow.target
                  ){ 
                    biArrow(_, _) 
                  }
                )
              )
          }
        }
        override lazy val globals = 
          carrier.globals.filter { global =>
            (arrow o global) == global
          } map { global =>
            AutomorphismArrow(I, automorphism, global)
          }
      }

      case class AutomorphismArrow[S <: ~, T <: ~](
        override val source: DOT[S],
        override val target: DOT[T],
        private[ToposOfAutomorphisms] val arrow: Ɛ.ARROW[S, T]
     ) extends Arrow[S, T] {
        override def \[U <: ~](monic: ARROW[U, T]): ARROW[S, U] = 
          AutomorphismArrow(source, monic.source, arrow \ monic.arrow)

        override def sanityTest {
          source.sanityTest
          target.sanityTest
          arrow.sanityTest
          if ((arrow o source.arrow) != (target.arrow o arrow))
            throw new IllegalArgumentException("Arrow does not respect automorphisms")
        }

        override def ?=(that: ARROW[S, T]) = {
          val equalizerCarrier = this.arrow ?= that.arrow
          new Automorphism[S](
            (source.arrow o equalizerCarrier.inclusion) \ equalizerCarrier.inclusion, 
            (source.inverse o equalizerCarrier.inclusion) \ equalizerCarrier.inclusion
          ) with EqualizingDot[S] { equalizer =>
            override val equalizerTarget = source
            override val inclusion = 
              equalizer(source){ s => s } // TODO: Build in as default?
            override def restrict[R <: ~](anArrow: ARROW[R, S]) = 
              AutomorphismArrow(anArrow.source, equalizer, equalizerCarrier.restrict(anArrow.arrow))
          }
        }
        override def apply(s: S) = arrow(s)
        override def o[R <: ~](that: ARROW[R, S]) =
          AutomorphismArrow(
            that.source, 
            target, 
            arrow o that.arrow
          )
        override lazy val chi =
          AutomorphismArrow(
            target, 
            omega, 
            arrow.chi
          )
      }

      private def trivialAutomorphism[X <: Ɛ.~](dot: Ɛ.DOT[X]) =
        Automorphism(dot.identity, dot.identity)

      override type WRAPPER[T <: Ɛ.~] = T

      override def functionAsArrow[ // TODO: looks redundant, take out of API?
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
      ) = ???

      private val memoizedDotWrapper =
        Memoize.generic withLowerBound[
        ({
          type λ[T <: Ɛ.~] = Ɛ.ARROW[T, T]
        })#λ,
        ({
          type λ[T <: Ɛ.~] = Automorphism[T]
        })#λ,
          Ɛ.~
        ] { predot =>
          if (predot.isIso)
            Automorphism(predot, predot.inverse)
          else 
            throw new IllegalArgumentException("Arrow is not iso")
        }

      override def makeDot[
        T <: ~
      ] (
        predot: Ɛ.ARROW[T, T]
      ) =
        memoizedDotWrapper(predot)

      override def bifunctionAsBiArrow[
        L <: ~,
        R <: ~,
        T <: ~
      ] (
        left: Automorphism[L],
        right: Automorphism[R],
        target: Automorphism[T])(bifunc: (L, R) => T
      ) =
        (left x right).biArrow(target) { bifunc }
    }
  }
}
