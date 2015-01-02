package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.helper.Memoize

import scala.Function._

object StarsAndQuiversLayer {
  def apply(topos: BaseDiagrammaticTopos): Topos with Wrappings[topos.DOT, topos.ARROW] = {
    class FancyTopos extends Topos with Wrappings[topos.DOT, topos.ARROW] {
      import topos.{ARROW, DOT, BiArrow, buildArrow}

      override type ELEMENT = Element

      trait Element {
        protected[StarsAndQuiversLayer] val arrow: ARROW[Any, Any]
      }

      override type STAR[S <: ELEMENT] = AdapterStar[S]
      override type QUIVER[S <: ELEMENT, T <: ELEMENT] = AdapterQuiver[S, T]

      override type UNIT = WrappedArrow[Unit]
      lazy val I: STAR[UNIT] = star(topos.I).asInstanceOf[STAR[UNIT]]

      override type TRUTH = AdapterTruth
      override lazy val omega = star(topos.omega).asInstanceOf[STAR[TRUTH]]
      override lazy val truth = quiver(topos.truth).asInstanceOf[QUIVER[UNIT, TRUTH]]

      trait AdapterTruth extends Element

      trait AdapterStar[T <: Element] extends Star[T] { self =>
        override lazy val toI: QUIVER[T, UNIT] =
          quiver(dot.toI).asInstanceOf[QUIVER[T, UNIT]]

        override def xUncached[U <: ELEMENT](that: STAR[U]) =
          new AdapterStar[T x U] with BiproductStar[T, U] {
            override val left = self
            override val right = that

            override def pair(t: T, u: U) = asElement(t.arrow x u.arrow)

            override private[StarsAndQuiversLayer] val dot = (self.dot x that.dot).asInstanceOf[DOT[Any]]

            override private[StarsAndQuiversLayer] def asElement(anArrow: ARROW[_, _]) =
              new (T, U)(
                self.asElement(fletch(topos.leftProjection(self.dot, that.dot))(fletch(anArrow))),
                that.asElement(fletch(topos.rightProjection(self.dot, that.dot))(fletch(anArrow)))
              ) with Element {
                override val arrow: ARROW[Any, Any] = fletch(anArrow)
              }
          }

        override def `>Uncached`[U <: ELEMENT](that: STAR[U]) =
          new AdapterStar[T > U] with ExponentialStar[T, U] {
            override val source = AdapterStar.this
            override val target = that

            private val exponential = target.dot A source.dot
            override private[StarsAndQuiversLayer] val dot = exponential.exponentDot.asInstanceOf[DOT[Any]]

            override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, T, U]) =
              AdapterQuiver.fromArrow(biQuiver.product.left, this, exponential.transpose(biArrow(biQuiver)))

            override private[StarsAndQuiversLayer] def asElement(anArrow: ARROW[_, _]) =
              new (T => U) with Element {
                override val arrow: ARROW[Any, Any] = fletch(anArrow)

                override def apply(s: T): U =
                  target.asElement(
                    topos.evaluation(source.dot, target.dot)(
                      anArrow.asInstanceOf[ARROW[Any, Any => Any]],
                      s.arrow
                    ))
              }
          }

        override def sanityTest = dot.sanityTest

        override def apply[U <: ELEMENT](target: STAR[U])(f: T => U) =
          AdapterQuiver[T, U](this, target, f)

        private[StarsAndQuiversLayer] val dot: DOT[Any]

        private[StarsAndQuiversLayer] def asElement(arrow: ARROW[_, _]): T
      }

      object AdapterQuiver {
        def apply[S <: ELEMENT, T <: ELEMENT](source: STAR[S], target: STAR[T], function:  S => T) =
          new AdapterQuiver[S, T](source, target,
            () => function,
            () => function(source.asElement(source.dot.identity)).arrow
          )

        def fromArrow[S <: ELEMENT, T <: ELEMENT](source: STAR[S], target: STAR[T], arrow: ARROW[_, _]) =
          new AdapterQuiver[S, T](source, target,
            () => t => target.asElement(fletch(arrow)(t.arrow)),
            () => fletch(arrow)
          )
      }

      class AdapterQuiver[S <: ELEMENT, T <: ELEMENT](
        val source: STAR[S],
        val target: STAR[T],
        _function: () => S => T,
        _arrow: () => ARROW[Any, Any]
      ) extends Quiver[S, T] {

        private[StarsAndQuiversLayer] lazy val arrow = _arrow()
        private[StarsAndQuiversLayer] lazy val function = _function()

        override def apply(s: S) = function(s)

        override def o[R <: ELEMENT](that: QUIVER[R, S]) =
          that.source(target)(function compose that.function)

        def ?=(that: QUIVER[S, T]) =
          new AdapterStar[S] with EqualizingStar[S] {
            private val equalizer = arrow ?= that.arrow
            override val equalizerTarget = AdapterQuiver.this.source

            override private[StarsAndQuiversLayer] val dot = equalizer.equalizerSource.asInstanceOf[DOT[Any]]

            override private[StarsAndQuiversLayer] def asElement(anArrow: ARROW[_, _]): S =
              equalizerTarget.asElement(fletch(equalizer.equalizer)(fletch(anArrow)))

            override def restrict[R <: ELEMENT](quiver: QUIVER[R, S]) =
              AdapterQuiver.fromArrow(quiver.source, this, equalizer.restrict(quiver.arrow))

            val inclusion: QUIVER[S, S] =
              this(source) { s => s}
          }

        override def equals(other: Any) = other match {
          case that: QUIVER[S, T] => arrow == that.arrow
          case _ => false
        }

        override lazy val chi = AdapterQuiver.fromArrow(target, omega, arrowChi.arrow)

        override def \[U <: ELEMENT](monic: QUIVER[U, T]) =
          AdapterQuiver.fromArrow(source, monic.source, monic.arrowChi.restrict(arrow))

        override def sanityTest = arrow.sanityTest

        private lazy val arrowChi = arrow.chi
      }

      class WrappedArrow[X](protected[StarsAndQuiversLayer] val arrow: ARROW[Any, Any]) extends ELEMENT {
        override def toString: String = s"WrappedArrow($arrow)"
      }

      private class WrappedDot[X](innerDot: DOT[X]) extends STAR[WrappedArrow[X]] {
        override private[StarsAndQuiversLayer] val dot: DOT[Any] = innerDot.asInstanceOf[DOT[Any]]

        override private[StarsAndQuiversLayer] def asElement(arrow: ARROW[_, _]) =
          new WrappedArrow(fletch(arrow))
      }

      private val memoizedWrappedDot = {
        def wrapDot[T](dot: DOT[T]) = new WrappedDot(dot)
        Memoize.generic(wrapDot)
      }

      private def fletch[X, Y](arrow: ARROW[X, Y]) =
        arrow.asInstanceOf[ARROW[Any, Any]]

      // wrapping API: TODO make this comment part of the structure

      override type WRAPPER[S] = WrappedArrow[S]

      override def star[S](dot: DOT[S]): STAR[WrappedArrow[S]] =
        memoizedWrappedDot(dot)

      override def quiver[S, T](arrow: ARROW[S, T]): QUIVER[WRAPPER[S], WRAPPER[T]] =
        AdapterQuiver.fromArrow(star(arrow.source), star(arrow.target), arrow)

      override def functionAsQuiver[S, T](
        source: STAR[WrappedArrow[S]],
        target: STAR[WrappedArrow[T]],
        f: S => T
      ) = quiver(buildArrow[S, T](
        source.dot.asInstanceOf[DOT[S]],
        target.dot.asInstanceOf[DOT[T]],
        f
      ).asInstanceOf[ARROW[WRAPPER[S], WRAPPER[T]]]
      ).asInstanceOf[QUIVER[WRAPPER[S], WRAPPER[T]]]

      override def bifunctionAsBiQuiver[L, R, T](
        left: STAR[WRAPPER[L]],
        right: STAR[WRAPPER[R]],
        target: STAR[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ) = {
        val targetProduct = star[(L, R)](
          left.dot.asInstanceOf[DOT[L]] x
            right.dot.asInstanceOf[DOT[R]]
        )
        BiQuiver(left x right, functionAsQuiver[(L, R), T](targetProduct, target, {
          case (l, r) => bifunc(l, r)
        }) o
          AdapterQuiver.fromArrow(left x right, targetProduct, targetProduct.dot.identity)
        )
      }

      private def biArrow[L <: ELEMENT, R <: ELEMENT, T <: ELEMENT](biQuiver: BiQuiver[L, R, T]) =
        BiArrow(biQuiver.product.left.dot,
          biQuiver.product.right.dot,
          biQuiver.quiver.arrow.asInstanceOf[ARROW[(Any, Any), Any]])
    }
    new FancyTopos
  }
}
