package com.fdilke.bewl.topos

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos
import com.fdilke.bewl.helper.{ResultStore, StrictRef}

import scala.Function._

class StarsAndQuiversAdapter[BASE <: BaseDiagrammaticTopos](topos : BASE) extends Topos with Wrappings[BASE#DOT, BASE#ARROW] {

  import topos._

  type ELEMENT = Element
  type STAR[S <: ELEMENT] = AdapterStar[S]
  type QUIVER[S <: ELEMENT, T <: ELEMENT] = AdapterQuiver[S, T]
  trait Element {
    protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]
  }
  trait x[T <: ELEMENT, U <: ELEMENT] extends xI[T, U] with ELEMENT {
    val left: T
    val right: U
  }
  trait AdapterStar[T <: Element] extends Star[T] {
    override val identity = AdapterQuiver[T, T](this, this, x => x)
    override def x[U <: Element](that: STAR[U]): STAR[T x U] =
      standardProductStar((
        StrictRef(this.asInstanceOf[STAR[WrappedArrow[Any]]]),
        StrictRef(that.asInstanceOf[STAR[WrappedArrow[Any]]])
        )).asInstanceOf[ProductStar[T, U]]
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
    override def equals(other: Any): Boolean = other match {
      case that: QUIVER[S, T] => arrow == that.arrow
      case _ => false
    }
    override def sanityTest = arrow.sanityTest
    private lazy val arrow: ARROW[Any, Any] =
      function(source.asElement(source.getDot.identity)).arrow
  }
  class WrappedArrow[X](protected[StarsAndQuiversAdapter] val arrow: ARROW[Any, Any]) extends ELEMENT
  private class WrappedDot[X](innerDot: DOT[X]) extends STAR[WrappedArrow[X]] {
    override val getDot: DOT[Any] = innerDot.asInstanceOf[DOT[Any]]  // TODO rename to dot
    override def asElement(arrow: ARROW[Any, Any]) =
      new WrappedArrow(arrow)
  }
  private class ProductStar[L <: ELEMENT, R <: ELEMENT](
                                                 l: STAR[L], r: STAR[R]
                                                 ) extends STAR[L x R] {
    override val getDot = (l.getDot x r.getDot).asInstanceOf[DOT[Any]]
    override def asElement(anArrow: ARROW[Any, Any]) = new x[L, R] { self: x[L, R] =>
      override val left: L = l.asElement(
        topos.leftProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val right: R = r.asElement(
        topos.rightProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val arrow: ARROW[Any, Any] = anArrow
    }
  }

  private val standardWrappedDot = new ResultStore[StrictRef[DOT[Any]], Star[WrappedArrow[Any]]] (
    x => new WrappedDot(x.wrappedValue)
  )

  private val standardProductStar = new ResultStore[(
    StrictRef[STAR[WrappedArrow[Any]]], StrictRef[STAR[WrappedArrow[Any]]]
    ),ProductStar[WrappedArrow[Any], WrappedArrow[Any]]](tupled {
    (x, y) => new ProductStar(x.wrappedValue, y.wrappedValue)
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
