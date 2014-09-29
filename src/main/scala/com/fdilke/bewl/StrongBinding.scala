package com.fdilke.bewl

import com.fdilke.bewl.helper.{StrictRef, ResultStore}

import scala.Function._
import scala.collection.mutable

trait StrongBinding { topos : BaseTopos =>

  trait Element {
    val arrow: ARROW[Any, Any]
  }

  trait x[T <: Element, U <: Element] extends Element {
    val left: T
    val right: U
  }

  trait Star[T <: Element] {
    val identity = Quiver[T, T](this, this, x => x)

    def x[U <: Element](that: Star[U]): Star[T x U] =
      standardProductStar((
        StrictRef(this.asInstanceOf[Star[WrappedArrow[Any]]]),
        StrictRef(that.asInstanceOf[Star[WrappedArrow[Any]]])
        )).asInstanceOf[ProductStar[T, U]]

    val getDot: DOT[Any]
    def asElement(arrow: ARROW[Any, Any]) : T
  }

  case class Quiver[S <: Element, T <: Element](
     source: Star[S],
     target: Star[T],
     function: S => T
    ) {
    def o[R <: Element](that: Quiver[R, S]) =
      Quiver(that.source, target, function compose that.function)

    def x[U <: Element](that: Quiver[S, U]) = {
      val product = target x that.target
      Quiver[S, T x U](
        source,
        product,
        arrowAsFunction(product, arrow x that.arrow))
    }

    override def equals(other: Any): Boolean = other match {
      case that: Quiver[S, T] => arrow == that.arrow
      case _ => false
    }

    private lazy val arrow: ARROW[Any, Any] =
      function(source.asElement(source.getDot.identity)).arrow

    def sanityTest = arrow.sanityTest
  }

  case class WrappedArrow[X](val arrow: ARROW[Any, Any]) extends Element

  class WrappedDot[X](dot: DOT[X]) extends Star[WrappedArrow[X]] {
    override val getDot: DOT[Any] = dot.asInstanceOf[DOT[Any]]

    override def asElement(arrow: ARROW[Any, Any]) =
      WrappedArrow(arrow)
  }

  class ProductStar[L <: Element, R <: Element](
    l: Star[L], r: Star[R]
  ) extends Star[L x R] {
    override val getDot = (l.getDot x r.getDot).asInstanceOf[DOT[Any]]

    override def asElement(anArrow: ARROW[Any, Any]) = new x[L, R] { self: x[L, R] =>
      override val left: L = l.asElement(
        leftProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val right: R = r.asElement(
        rightProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val arrow: ARROW[Any, Any] = anArrow
    }
  }

  private val standardWrappedDot = new ResultStore[StrictRef[DOT[Any]], Star[WrappedArrow[Any]]] (
    x => new WrappedDot(x.wrappedValue)
  )

  private val standardProductStar = new ResultStore[(
    StrictRef[Star[WrappedArrow[Any]]], StrictRef[Star[WrappedArrow[Any]]]
  ),ProductStar[WrappedArrow[Any], WrappedArrow[Any]]](tupled {
    (x, y) => new ProductStar(x.wrappedValue, y.wrappedValue)
  })

  def wrapDot[X](dot: DOT[X]) =
    standardWrappedDot(
      StrictRef(dot.asInstanceOf[DOT[Any]])
    ).asInstanceOf[Star[WrappedArrow[X]]]

  def arrowAsFunction[X, Y, S <: Element, T <: Element](
    target: Star[T], arrow: ARROW[X, Y]
  ): S => T =
    (s : S) =>
      target.asElement(arrow.asInstanceOf[ARROW[Any, Any]](s.arrow))

  def wrapArrow[X, Y](arrow: ARROW[X, Y]) = {
    val source = wrapDot(arrow.source)
    val target = wrapDot(arrow.target)
    Quiver(source, target,
      arrowAsFunction[X, Y, WrappedArrow[X], WrappedArrow[Y]](target, arrow)
    )
  }

  def leftProjection[A <: Element, B <: Element](left: Star[A], right: Star[B]) =
    Quiver[A x B, A](left x right, left, _.left)

  def rightProjection[A <: Element, B <: Element](left: Star[A], right: Star[B]) =
    Quiver[A x B, B](left x right, right, _.right)
}
