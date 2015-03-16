package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.helper.Memoize

import scala.Function._

object StarsAndQuiversLayer {
  def apply(
    Δ: BaseDiagrammaticTopos
  ): Topos with Wrappings[Any, Δ.DOT, Δ.ARROW] = {
    class FancyTopos extends Topos with Wrappings[Any, Δ.DOT, Δ.ARROW] {
      // import Δ.{ARROW, DOT, BiArrow, buildArrow}

      override type ~ = Element

      trait Element {
        protected[StarsAndQuiversLayer] val arrow: Δ.ARROW[Any, Any]
      }

      override type DOT[S <: ~] = AdapterStar[S]
      override type ARROW[S <: ~, T <: ~] = AdapterQuiver[S, T]

      override type UNIT = WrappedArrow[Unit]
      lazy val I: DOT[UNIT] = star(Δ.I).asInstanceOf[DOT[UNIT]]

      override type TRUTH = AdapterTruth
      type >[T <: ~, U <: ~] = (T => U) with ~
      type x[T <: ~, U <: ~] = (T, U) with ~

      override lazy val omega = star(Δ.omega).asInstanceOf[DOT[TRUTH]]
      override lazy val truth = quiver(Δ.truth).asInstanceOf[ARROW[UNIT, TRUTH]]

      trait AdapterTruth extends Element

      trait AdapterStar[T <: Element] extends Star[T] { self =>
        private[StarsAndQuiversLayer] val dot: Δ.DOT[Any]

        override lazy val toI: ARROW[T, UNIT] =
          quiver(dot.toI).asInstanceOf[ARROW[T, UNIT]]

        override lazy val globals: Traversable[ARROW[UNIT, T]] =
          dot.globals.map { global =>
            AdapterQuiver.fromArrow(I, this, global)
          }

        override def xUncached[U <: ~](that: DOT[U]) =
          new AdapterStar[T x U] with BiproductStar[T, U, T x U] {
            override val left = self
            override val right = that

            override def pair(t: T, u: U) = asElement(t.arrow x u.arrow)

            override private[StarsAndQuiversLayer] val dot = (self.dot x that.dot).asInstanceOf[Δ.DOT[Any]]

            override private[StarsAndQuiversLayer] def asElement(anArrow: Δ.ARROW[_, _]) =
              new (T, U)(
                self.asElement(fletch(Δ.leftProjection(self.dot, that.dot))(fletch(anArrow))),
                that.asElement(fletch(Δ.rightProjection(self.dot, that.dot))(fletch(anArrow)))
              ) with Element {
                override val arrow: Δ.ARROW[Any, Any] = fletch(anArrow)
              }
          }

        override def `>Uncached`[U <: ~](that: DOT[U]) =
          new AdapterStar[T > U] with ExponentialStar[T, U, T > U] {
            override val source = AdapterStar.this
            override val target = that

            private val exponential = target.dot A source.dot
            override private[StarsAndQuiversLayer] val dot = exponential.exponentDot.asInstanceOf[Δ.DOT[Any]]

            override def transpose[R <: ~](biQuiver: BiQuiver[R, T, U]) =
              AdapterQuiver.fromArrow(biQuiver.product.left, this, exponential.transpose(biArrow(biQuiver)))

            override private[StarsAndQuiversLayer] def asElement(anArrow: Δ.ARROW[_, _]) =
              new (T => U) with Element {
                override val arrow: Δ.ARROW[Any, Any] = fletch(anArrow)

                override def apply(s: T): U =
                  target.asElement(
                    Δ.evaluation(source.dot, target.dot)(
                      anArrow.asInstanceOf[Δ.ARROW[Any, Any => Any]],
                      s.arrow
                    ))
              }
          }

        override def sanityTest = dot.sanityTest

        override def apply[U <: ~](target: DOT[U])(f: T => U) =
          AdapterQuiver[T, U](this, target, f)

        private[StarsAndQuiversLayer] def asElement(arrow: Δ.ARROW[_, _]): T
      }

      object AdapterQuiver {
        def apply[S <: ~, T <: ~](source: DOT[S], target: DOT[T], function:  S => T) =
          new AdapterQuiver[S, T](source, target,
            () => function,
            () => function(source.asElement(source.dot.identity)).arrow
          )

        def fromArrow[S <: ~, T <: ~](source: DOT[S], target: DOT[T], arrow: Δ.ARROW[_, _]) =
          new AdapterQuiver[S, T](source, target,
            () => t => target.asElement(fletch(arrow)(t.arrow)),
            () => fletch(arrow)
          )
      }

      class AdapterQuiver[S <: ~, T <: ~](
        val source: DOT[S],
        val target: DOT[T],
        _function: () => S => T,
        _arrow: () => Δ.ARROW[Any, Any]
      ) extends Quiver[S, T] {

        private[StarsAndQuiversLayer] lazy val arrow = _arrow()
        private[StarsAndQuiversLayer] lazy val function = _function()

        override def apply(s: S) = function(s)

        override def o[R <: ~](that: ARROW[R, S]) =
          that.source(target)(function compose that.function)

        def ?=(that: ARROW[S, T]) =
          new AdapterStar[S] with EqualizingStar[S] {
            private val equalizer = arrow ?= that.arrow
            override val equalizerTarget = AdapterQuiver.this.source

            override private[StarsAndQuiversLayer] val dot = equalizer.equalizerSource.asInstanceOf[Δ.DOT[Any]]

            override private[StarsAndQuiversLayer] def asElement(anArrow: Δ.ARROW[_, _]): S =
              equalizerTarget.asElement(fletch(equalizer.equalizer)(fletch(anArrow)))

            override def restrict[R <: ~](quiver: ARROW[R, S]) =
              AdapterQuiver.fromArrow(quiver.source, this, equalizer.restrict(quiver.arrow))

            val inclusion: ARROW[S, S] =
              this(source) { s => s}
          }

        override def equals(other: Any) = other match {
          case that: ARROW[S, T] => arrow == that.arrow
          case _ => false
        }

        override lazy val chi = AdapterQuiver.fromArrow(target, omega, arrowChi.arrow)

        override def \[U <: ~](monic: ARROW[U, T]) =
          AdapterQuiver.fromArrow(source, monic.source, monic.arrowChi.restrict(arrow))

        override def sanityTest = arrow.sanityTest

        private lazy val arrowChi = arrow.chi
      }

      class WrappedArrow[X](protected[StarsAndQuiversLayer] val arrow: Δ.ARROW[Any, Any]) extends ~ {
        override def toString: String = s"WrappedArrow($arrow)"
      }

      private class WrappedDot[X](innerDot: Δ.DOT[X]) extends DOT[WrappedArrow[X]] {
        override private[StarsAndQuiversLayer] val dot: Δ.DOT[Any] = innerDot.asInstanceOf[Δ.DOT[Any]]

        override private[StarsAndQuiversLayer] def asElement(arrow: Δ.ARROW[_, _]) =
          new WrappedArrow(fletch(arrow))
      }

      private val memoizedWrappedDot = {
        def wrapDot[T](dot: Δ.DOT[T]) = new WrappedDot(dot)
        Memoize.generic(wrapDot)
      }

      private def fletch[X, Y](arrow: Δ.ARROW[X, Y]) =
        arrow.asInstanceOf[Δ.ARROW[Any, Any]]

      // wrapping API: TODO make this comment part of the structure

      override type WRAPPER[S] = WrappedArrow[S]

      override def star[S](dot: Δ.DOT[S]): DOT[WrappedArrow[S]] =
        memoizedWrappedDot(dot)

      override def quiver[S, T](arrow: Δ.ARROW[S, T]): ARROW[WRAPPER[S], WRAPPER[T]] =
        AdapterQuiver.fromArrow(star(arrow.source), star(arrow.target), arrow)

      override def functionAsQuiver[S, T](
        source: DOT[WrappedArrow[S]],
        target: DOT[WrappedArrow[T]],
        f: S => T
      ) = quiver(Δ.buildArrow[S, T](
        source.dot.asInstanceOf[Δ.DOT[S]],
        target.dot.asInstanceOf[Δ.DOT[T]],
        f
      ).asInstanceOf[Δ.ARROW[WRAPPER[S], WRAPPER[T]]]
      ).asInstanceOf[ARROW[WRAPPER[S], WRAPPER[T]]]

      override def bifunctionAsBiQuiver[L, R, T](
        left: DOT[WRAPPER[L]],
        right: DOT[WRAPPER[R]],
        target: DOT[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ) = {
        val targetProduct = star[(L, R)](
          left.dot.asInstanceOf[Δ.DOT[L]] x
            right.dot.asInstanceOf[Δ.DOT[R]]
        )
        BiQuiver(left x right, functionAsQuiver[(L, R), T](targetProduct, target, {
          case (l, r) => bifunc(l, r)
        }) o
          AdapterQuiver.fromArrow(left x right, targetProduct, targetProduct.dot.identity)
        )
      }

      private def biArrow[L <: ~, R <: ~, T <: ~](biQuiver: BiQuiver[L, R, T]) =
        Δ.BiArrow(
          biQuiver.product.left.dot,
          biQuiver.product.right.dot,
          biQuiver.quiver.arrow.asInstanceOf[Δ.ARROW[(Any, Any), Any]]
        )
    }
    new FancyTopos
  }
}
