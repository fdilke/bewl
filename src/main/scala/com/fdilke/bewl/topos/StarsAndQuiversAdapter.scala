package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.helper.{ResultStore, StrictRef}

import scala.Function._

class StarsAndQuiversAdapter[BASE <: BaseDiagrammaticTopos](topos : BASE) extends Topos with Wrappings[BASE#DOT, BASE#ARROW] {

  import topos.{ I => _, _ }

  type ELEMENT = Element
  type STAR[S <: ELEMENT] = AdapterStar[S]
  type QUIVER[S <: ELEMENT, T <: ELEMENT] = AdapterQuiver[S, T]

  trait Element {
    protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]
  }

  type UNIT = WrappedArrow[Unit]
  lazy val I : STAR[UNIT] = star(topos.I).asInstanceOf[AdapterStar[UNIT]]

  trait AdapterStar[T <: Element] extends Star[T] {
    override lazy val toI: QUIVER[T, UNIT] =
      quiver(dot.toI).asInstanceOf[QUIVER[T, UNIT]]

    override def x[U <: Element](that: STAR[U]) =
      standardProductStar((
        StrictRef(this.asInstanceOf[STAR[WrappedArrow[Any]]]),
        StrictRef(that.asInstanceOf[STAR[WrappedArrow[Any]]])
        )).asInstanceOf[STAR[T x U]]
    override def >[U <: ELEMENT](that: STAR[U]) =
      standardExponentialStar((
        StrictRef(this.asInstanceOf[STAR[WrappedArrow[Any]]]),
        StrictRef(that.asInstanceOf[STAR[WrappedArrow[Any]]])
        )).asInstanceOf[ExponentialStar[T, U] with STAR[T > U]]

    override def sanityTest = dot.sanityTest

    override def apply[U <: ELEMENT](target: STAR[U])(f: T => U) =
      AdapterQuiver[T, U](this, target, f)

    protected[StarsAndQuiversAdapter] val dot: DOT[Any]
    private[StarsAndQuiversAdapter] def apply[X, Y, U <: Element] (
      target: STAR[U],
      arrow: ARROW[X, Y]
      ): QUIVER[T, U] = this(target)(
      t => target.asElement(arrow.asInstanceOf[ARROW[Any, Any]](t.arrow)) // TODO: lose cast?
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

    override def equals(other: Any): Boolean = other match {
      case that: QUIVER[S, T] => arrow == that.arrow
      case _ => false
    }
    override def sanityTest = arrow.sanityTest
    private[StarsAndQuiversAdapter] lazy val arrow: ARROW[Any, Any] =
      function(source.asElement(source.dot.identity)).arrow
  }

  class WrappedArrow[X](protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]) extends ELEMENT {
    override def toString: String = s"WrappedArrow($arrow)"
  }

  private class WrappedDot[X](innerDot: DOT[X]) extends STAR[WrappedArrow[X]] {
    override val dot: DOT[Any] = innerDot.asInstanceOf[DOT[Any]]
    override def asElement(arrow: ARROW[_, _]) =
      new WrappedArrow(arrow.asInstanceOf[ARROW[Any, Any]])
  }

  private def productStar[L <: ELEMENT, R <: ELEMENT](
    l: STAR[L], r: STAR[R]
  ) = new AdapterStar[L x R] {
    override val dot = (l.dot x r.dot).asInstanceOf[DOT[Any]]
    override def asElement(anArrow: ARROW[_, _]) = new xI[L, R] with Element {
      override val arrow: ARROW[Any, Any] = anArrow.asInstanceOf[ARROW[Any, Any]]
      override val left: L = l.asElement(
        topos.leftProjection(l.dot, r.dot).asInstanceOf[ARROW[Any, Any]](arrow))
      override val right: R = r.asElement(
        topos.rightProjection(l.dot, r.dot).asInstanceOf[ARROW[Any, Any]](arrow))
    }
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
      override def apply(s: S): T =
        target.asElement(
          topos.evaluation(source.dot, target.dot)(
            anArrow.asInstanceOf[ARROW[Any, Any => Any]],
            s.arrow
        ))

      override val arrow: ARROW[Any, Any] = anArrow.asInstanceOf[ARROW[Any, Any]]
    }
  }

  private val standardWrappedDot = new ResultStore[StrictRef[DOT[Any]], Star[WrappedArrow[Any]]] (
    x => new WrappedDot(x.wrappedValue)
  )

  private val standardProductStar = new ResultStore[(
    StrictRef[STAR[WrappedArrow[Any]]], StrictRef[STAR[WrappedArrow[Any]]]
    ),STAR[WrappedArrow[Any] x WrappedArrow[Any]]](tupled {
    (x, y) => productStar(x.wrappedValue, y.wrappedValue)
  })

  private val standardExponentialStar = new ResultStore[(
    StrictRef[STAR[WrappedArrow[Any]]], StrictRef[STAR[WrappedArrow[Any]]]
    ),STAR[WrappedArrow[Any] > WrappedArrow[Any]]](tupled {
    (x, y) => exponentialStar(x.wrappedValue, y.wrappedValue)
  })

  // wrappings

  type DOTWRAPPER[S] = WrappedArrow[S]

  override def star[S](dot: BASE#DOT[S]) =
    standardWrappedDot(
      StrictRef(dot.asInstanceOf[DOT[Any]])
    ).asInstanceOf[STAR[WrappedArrow[S]]]

  override def quiver[S, T](arrow: BASE#ARROW[S, T]) : QUIVER[DOTWRAPPER[S], DOTWRAPPER[T]] =
    star(arrow.source)(star(arrow.target), arrow.asInstanceOf[ARROW[S, T]])

  override def functionAsQuiver[S, T](source: STAR[WrappedArrow[S]], target: STAR[WrappedArrow[T]], f: S => T) =
    quiver(buildArrow[S, T](
      source.dot.asInstanceOf[DOT[S]],
      target.dot.asInstanceOf[DOT[T]],
      f
    ).asInstanceOf[BASE#ARROW[DOTWRAPPER[S], DOTWRAPPER[T]]]
  ).asInstanceOf[QUIVER[DOTWRAPPER[S], DOTWRAPPER[T]]]

  override def bifunctionAsBiQuiver[L, R, T] (
    left: STAR[DOTWRAPPER[L]],
    right: STAR[DOTWRAPPER[R]],
    target: STAR[DOTWRAPPER[T]],
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
