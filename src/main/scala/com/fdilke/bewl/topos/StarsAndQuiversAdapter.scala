package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.helper.{Memoize, ResultStore, StrictRef}

import scala.Function._

class StarsAndQuiversAdapter[BASE <: BaseDiagrammaticTopos](topos : BASE)
  extends Topos with Wrappings[BASE#DOT, BASE#ARROW] {

  import topos.{ I => _, omega => _, _ }

  override type ELEMENT = Element
  trait Element {
    protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]
  }

  override type STAR[S <: ELEMENT] = AdapterStar[S]
  override type QUIVER[S <: ELEMENT, T <: ELEMENT] = AdapterQuiver[S, T]

  override type UNIT = WrappedArrow[Unit]
  lazy val I : STAR[UNIT] = star(topos.I).asInstanceOf[STAR[UNIT]]

  override type TRUTH = AdapterTruth
  override lazy val omega = star(topos.omega).asInstanceOf[STAR[TRUTH]]
  override lazy val truth = quiver(topos.truth).asInstanceOf[QUIVER[UNIT, TRUTH]]

  trait AdapterTruth extends Element {
  }

  trait AdapterStar[T <: Element] extends Star[T] {
    override lazy val toI: QUIVER[T, UNIT] =
      quiver(dot.toI).asInstanceOf[QUIVER[T, UNIT]]

    private object standardProductStar {
      private type Widget[U <: ELEMENT] = STAR[T x U]
      private def product[U <: ELEMENT](that: STAR[U]): Widget[U] =
        new AdapterStar[T x U] {
          override val dot = (AdapterStar.this.dot x that.dot).asInstanceOf[DOT[Any]]
          override def asElement(anArrow: ARROW[_, _]) = new xI[T, U] with Element {
            override val arrow: ARROW[Any, Any] = anArrow.asInstanceOf[ARROW[Any, Any]]
            override val left: T = AdapterStar.this.asElement(
              topos.leftProjection(AdapterStar.this.dot, that.dot).asInstanceOf[ARROW[Any, Any]](arrow))
            override val right: U = that.asElement(
              topos.rightProjection(AdapterStar.this.dot, that.dot).asInstanceOf[ARROW[Any, Any]](arrow))
          }
        }
      private val memoized = Memoize[STAR, Widget, ELEMENT](product)
      def apply[U <: ELEMENT](that: STAR[U]) = memoized(that)
    }

    override def x[U <: Element](that: STAR[U]) =
      standardProductStar(that)
    override def >[U <: ELEMENT](that: STAR[U]) =
      standardExponentialStar((
        StrictRef(this.asInstanceOf[STAR[WrappedArrow[Any]]]),
        StrictRef(that.asInstanceOf[STAR[WrappedArrow[Any]]])
        )).asInstanceOf[ExponentialStar[T, U] with STAR[T > U]]

    override def sanityTest = dot.sanityTest

    override def apply[U <: ELEMENT](target: STAR[U])(f: T => U) =
      AdapterQuiver[T, U](this, target, f)

    protected[StarsAndQuiversAdapter] val dot: DOT[Any]
    private[StarsAndQuiversAdapter] def apply[U <: Element] (
      target: STAR[U],
      arrow: ARROW[_, _]
      ): QUIVER[T, U] = this(target)(
      t => target.asElement(arrow.asInstanceOf[ARROW[Any, Any]](t.arrow))
    )

    def asElement(arrow: ARROW[_, _]) : T
  }

  case class AdapterQuiver[S <: Element, T <: Element] (
    source: STAR[S],
    target: STAR[T],
    function: S => T
    ) extends Quiver[S, T]  {
    override def apply(s: S) = function(s)

    override def o[R <: ELEMENT](that: QUIVER[R, S]) =
      that.source(target)(function compose that.function)

    override def x[U <: ELEMENT](that: QUIVER[S, U]) =
      source(target x that.target, arrow x that.arrow)

    def ?=(that: QUIVER[S, T]) =
      new AdapterStar[EqualizingElement[S] with Element] with EqualizingStar[S] {
        private val equalizer = arrow ?= that.arrow
        override val equalizerTarget = AdapterQuiver.this.source

        override protected[StarsAndQuiversAdapter] val dot = equalizer.equalizerSource.asInstanceOf[DOT[Any]]

        override def asElement(anArrow: ARROW[_, _]): EqualizingElement[S] with Element =
          new EqualizingElement[S] with Element {
            override protected[StarsAndQuiversAdapter] val arrow = anArrow.asInstanceOf[ARROW[Any, Any]]
            override val include =
              equalizerTarget.asElement(equalizer.equalizer.asInstanceOf[ARROW[Any, Any]](arrow))
          }

        override def restrict[R <: ELEMENT](quiver: QUIVER[R, S]) =
          quiver.source(this, equalizer.restrict(quiver.arrow))
      }

    override def equals(other: Any) = other match {
      case that: QUIVER[S, T] => arrow == that.arrow
      case _ => false
    }

    override lazy val chi =
      target(omega, arrowChi.arrow)

    override def \[U <: ELEMENT](monic: QUIVER[U, T]) =
      source(monic.source, monic.arrowChi.restrict(arrow))

    override def sanityTest = arrow.sanityTest

    private[StarsAndQuiversAdapter] lazy val arrow =
      function(source.asElement(source.dot.identity)).arrow

    private lazy val arrowChi = arrow.chi
  }

  class WrappedArrow[X](protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]) extends ELEMENT {
    override def toString: String = s"WrappedArrow($arrow)"
  }

  private class WrappedDot[X](innerDot: DOT[X]) extends STAR[WrappedArrow[X]] {
    override val dot: DOT[Any] = innerDot.asInstanceOf[DOT[Any]]
    override def asElement(arrow: ARROW[_, _]) =
      new WrappedArrow(arrow.asInstanceOf[ARROW[Any, Any]])
  }

  private def exponentialStar[S <: ELEMENT, T <: ELEMENT](
    _source : STAR[S], _target : STAR[T]
   ) = new AdapterStar[S > T] with ExponentialStar[S, T] {
    override val source = _source
    override val target = _target

    private val exponential = target.dot A source.dot
    override val dot = exponential.exponentDot.asInstanceOf[DOT[Any]]
    override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, S, T]) =
      biQuiver.left(this, exponential.transpose(
        topos.BiArrow(biQuiver.left.dot, biQuiver.right.dot, biQuiver.quiver.arrow.asInstanceOf[ARROW[(Any, Any), Any]])
      ))

    override def asElement(anArrow: ARROW[_, _]) = new ~>[S, T] with Element {
      override val arrow: ARROW[Any, Any] = anArrow.asInstanceOf[ARROW[Any, Any]]
      override def apply(s: S): T =
        target.asElement(
          topos.evaluation(source.dot, target.dot)(
            anArrow.asInstanceOf[ARROW[Any, Any => Any]],
            s.arrow
        ))
    }
  }

  private val standardExponentialStar = new ResultStore[(
    StrictRef[STAR[WrappedArrow[Any]]], StrictRef[STAR[WrappedArrow[Any]]]
    ),STAR[WrappedArrow[Any] > WrappedArrow[Any]]](tupled {
    (x, y) => exponentialStar(x.wrappedValue, y.wrappedValue)
  })

  private object standardWrappedDot {
    private type Widget[T] = STAR[WrappedArrow[T]]
    private def wrapDot[T](dot: DOT[T]): Widget[T] = new WrappedDot(dot)
    private val memoized = Memoize[DOT, Widget, Any](wrapDot)
    def apply[T](dot: DOT[T]) = memoized(dot)
  }

  // wrapping API

  override type WRAPPER[S] = WrappedArrow[S]

  override def star[S](dot: BASE#DOT[S]) : STAR[WrappedArrow[S]] =
    standardWrappedDot(
      dot.asInstanceOf[DOT[S]]
    )

  override def quiver[S, T](arrow: BASE#ARROW[S, T]) : QUIVER[WRAPPER[S], WRAPPER[T]] =
    star(arrow.source)(star(arrow.target), arrow.asInstanceOf[ARROW[S, T]])

  override def functionAsQuiver[S, T](source: STAR[WrappedArrow[S]], target: STAR[WrappedArrow[T]], f: S => T) =
    quiver(buildArrow[S, T](
      source.dot.asInstanceOf[DOT[S]],
      target.dot.asInstanceOf[DOT[T]],
      f
    ).asInstanceOf[BASE#ARROW[WRAPPER[S], WRAPPER[T]]]
  ).asInstanceOf[QUIVER[WRAPPER[S], WRAPPER[T]]]

  override def bifunctionAsBiQuiver[L, R, T] (
    left: STAR[WRAPPER[L]],
    right: STAR[WRAPPER[R]],
    target: STAR[WRAPPER[T]],
    bifunc: (L, R) => T
  ) = {
    val targetProduct = star[(L, R)](
      left.dot.asInstanceOf[DOT[L]] x
      right.dot.asInstanceOf[DOT[R]]
    )

    BiQuiver(left, right,  functionAsQuiver[(L, R), T](targetProduct, target, {
      case (l, r) => bifunc(l, r)
    }) o
      (left x right)(targetProduct, targetProduct.dot.identity)
    )
  }
}
