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
      quiver(getDot.toI).asInstanceOf[QUIVER[T, UNIT]]

    override val identity = AdapterQuiver[T, T](this, this, x => x)
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

    override def sanityTest = getDot.sanityTest

    override def apply[U <: ELEMENT](target: STAR[U])(f: T => U) =
      AdapterQuiver[T, U](this, target, f)

    protected[StarsAndQuiversAdapter] val getDot: DOT[Any]
    def asElement(arrow: ARROW[Any, Any]) : T
  }

  case class AdapterQuiver[S <: Element, T <: Element] (
    source: STAR[S],
    target: STAR[T],
    function: S => T
    ) extends Quiver[S, T]  {

    override def o[R <: ELEMENT](that: QUIVER[R, S]) =
      AdapterQuiver(that.source, target, function compose that.function)

    override def x[U <: ELEMENT](that: QUIVER[S, U]) = {
      val product = target x that.target
      AdapterQuiver[S, T x U](
        source,
        product,
        arrowAsFunction(product, arrow x that.arrow))
    }

    def ?=(that: QUIVER[S, T]) =
      new AdapterStar[EqualizingElement[S] with Element] with EqualizingStar[S] {
        private val equalizer = arrow ?= that.arrow
        override val equalizerTarget = AdapterQuiver.this.source

        override protected[StarsAndQuiversAdapter] val getDot = equalizer.equalizerSource.asInstanceOf[DOT[Any]]

        override def asElement(anArrow: ARROW[Any, Any]): EqualizingElement[S] with Element =
        new EqualizingElement[S] with Element {
          override val include: S =
            equalizerTarget.asElement(equalizer.equalizer.asInstanceOf[ARROW[Any, Any]](anArrow))
          override protected[StarsAndQuiversAdapter] val arrow = anArrow
        }

        def restrict[R <: ELEMENT](quiver: QUIVER[R, S]): QUIVER[R, EqualizingElement[S] with Element] =
          AdapterQuiver(quiver.source, this, // TODO: just "this"?
            arrowAsFunction[Any, EQUALIZER_SOURCE[Any], R, EqualizingElement[S] with Element](
              this, equalizer.restrict(quiver.arrow)
            ))
      }


    override def equals(other: Any): Boolean = other match {
      case that: QUIVER[S, T] => arrow == that.arrow
      case _ => false
    }
    override def sanityTest = arrow.sanityTest
    private[StarsAndQuiversAdapter] lazy val arrow: ARROW[Any, Any] =
      function(source.asElement(source.getDot.identity)).arrow
  }

  class WrappedArrow[X](protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]) extends ELEMENT

  private class WrappedDot[X](innerDot: DOT[X]) extends STAR[WrappedArrow[X]] {
    override val getDot: DOT[Any] = innerDot.asInstanceOf[DOT[Any]]  // TODO rename to dot
    override def asElement(arrow: ARROW[Any, Any]) =
      new WrappedArrow(arrow)
  }

  private def productStar[L <: ELEMENT, R <: ELEMENT](
    l: STAR[L], r: STAR[R]
  ) = new AdapterStar[L x R] {
    override val getDot = (l.getDot x r.getDot).asInstanceOf[DOT[Any]]
    override def asElement(anArrow: ARROW[Any, Any]) = new xI[L, R] with Element {
      override val left: L = l.asElement(
        topos.leftProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val right: R = r.asElement(
        topos.rightProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val arrow: ARROW[Any, Any] = anArrow
    }
  }

  private def exponentialStar[S <: ELEMENT, T <: ELEMENT](
    source: STAR[S], target: STAR[T]
  ) = new AdapterStar[S > T] with ExponentialStar[S, T] {
    override val getDot = (target.getDot ^ source.getDot).asInstanceOf[DOT[Any]]
    override def transpose[R <: ELEMENT](quiver: QUIVER[R x S, T]): QUIVER[R, S > T] = ???

    override def asElement(anArrow: ARROW[Any, Any]) = new ~>[S, T] with Element {
      override def apply(s: S): T =
        target.asElement(
          topos.evaluation(source.getDot, target.getDot)(
            anArrow.asInstanceOf[ARROW[Any, Any => Any]],
            s.arrow
        ))

      override val arrow: ARROW[Any, Any] = anArrow
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

  def arrowAsFunction[X, Y, S <: Element, T <: Element](
    target: STAR[T], arrow: ARROW[X, Y]
  ): S => T =
    (s : S) =>
      target.asElement(arrow.asInstanceOf[ARROW[Any, Any]](s.arrow))

  // wrappings

  type DOTWRAPPER[S] = WrappedArrow[S]

  override def star[S](dot: BASE#DOT[S]) =
    standardWrappedDot(
      StrictRef(dot.asInstanceOf[DOT[Any]])
    ).asInstanceOf[STAR[WrappedArrow[S]]]

  override def quiver[S, T](arrow: BASE#ARROW[S, T]) : QUIVER[DOTWRAPPER[S], DOTWRAPPER[T]] = {
    val source = star(arrow.source)
    val target = star(arrow.target)
    AdapterQuiver(source, target,
      arrowAsFunction[S, T, WrappedArrow[S], WrappedArrow[T]](target, arrow.asInstanceOf[ARROW[S, T]])
    )
  }

  override def cleverQuiver[S, T](source: STAR[WrappedArrow[S]], target: STAR[WrappedArrow[T]], f: S => T) =
    quiver(buildArrow[S, T](
      source.getDot.asInstanceOf[DOT[S]],
      target.getDot.asInstanceOf[DOT[T]],
      f
    ).asInstanceOf[BASE#ARROW[DOTWRAPPER[S], DOTWRAPPER[T]]]).asInstanceOf[QUIVER[DOTWRAPPER[S], DOTWRAPPER[T]]]
}
