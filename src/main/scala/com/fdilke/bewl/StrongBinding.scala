package com.fdilke.bewl

import com.fdilke.bewl.helper.{StrictRef, ResultStore}

import scala.Function._

trait StrongBinding { topos : BaseTopos =>

  trait Element[T <: Element[T]] {
    def asArrowFrom[S <: Element[S]](star: Star[S]): Quiver[S, T]
  }

  trait Star[T <: Element[T]] {
    val identity: Quiver[T, T]

    def x[U <: Element[U]](that: Star[U]): Star[T x U]
    def map[U <: Element[U]](f: T => U): Quiver[T, U]
    def asElement[Z <: Element[Z]](quiver: Quiver[Z, T]) : T

    def getDot: DOT[Any]
  }
  trait Quiver[S <: Element[S], T <: Element[T]] {
    val source: Star[S]
    val target: Star[T]

    def asElement : T

    def apply[R <: Element[R]](quiver: Quiver[R, S]): Quiver[R, T]
    def x[U <: Element[U]](that: Quiver[S, U]): Quiver[S, T x U]

    def sanityTest
    def getArrow: ARROW[_, _]
  }

  trait x[T <: Element[T], U <: Element[U]] extends Element[T x U] {
    val left: T
    val right: U
  }

  trait WrappedType[X] extends Element[WrappedType[X]] {}

  private abstract class StarWrapper[X, A <: Element[A]](dot: DOT[X]) extends Star[A] {
    override lazy val identity = wrapArrow[X, X, A, A](dot.identity)
    override def x[T <: Element[T]](that: Star[T]): Star[A x T] =
      standardProductStar((
        StrictRef(this.asInstanceOf[Star[WrappedType[Any]]]),
        StrictRef(that.asInstanceOf[Star[WrappedType[Any]]])
      )).asInstanceOf[Star[A x T]]
    override def map[U <: Element[U]](f: A => U): Quiver[A, U] = {
      val a: A = identity.asElement
      val u: U = f(a)
      u.asArrowFrom(this)       // TODO compactify
    }

    override def getDot: DOT[Any] = dot.asInstanceOf[DOT[Any]]
    override def equals(other: Any): Boolean = other match {
      case that: StarWrapper[X, A] => this eq that
      case _ => false
    }
  }

  private class VanillaStar[X](dot: DOT[X]) extends StarWrapper[X, WrappedType[X]](dot) {
    override def asElement[Z <: Element[Z]](quiver: Quiver[Z, WrappedType[X]]) = new WrappedType[X] {
        override def asArrowFrom[S <: Element[S]](star: Star[S]): Quiver[S, WrappedType[X]] =
          if (star.getDot == dot) {
            quiver.asInstanceOf[Quiver[S, WrappedType[X]]]
          } else {
            throw new IllegalArgumentException("Cannot rebase arrow") // Possible exception here if star is Terminal
          }
      }
  }

  private class ProductStar[L <: Element[L], R <: Element[R]](left: Star[L], right: Star[R]) extends StarWrapper[(Any, Any), WrappedType[L x R]](left.getDot x right.getDot) {
    override def asElement[Z <: Element[Z]](quiver: Quiver[Z, WrappedType[L x R]]) = new WrappedType[L x R] { // TODO Factor out this class: it's reusable!
      override def asArrowFrom[S <: Element[S]](star: Star[S]): Quiver[S, WrappedType[L x R]] =
        if (star.getDot == getDot) {
          quiver.asInstanceOf[Quiver[S, WrappedType[L x R]]]
        } else {
          throw new IllegalArgumentException("Cannot rebase arrow") // Possible exception here if star is Terminal
        }
    }
  }

  private class QuiverWrapper[X, Y, A <: Element[A], B <: Element[B]](private val arrow: ARROW[X, Y]) extends Quiver[A, B] {
    override def apply[R <: Element[R]](quiver: Quiver[R, A]): Quiver[R, B] =
      wrapArrow[Any, Y, R, B](arrow(quiver.getArrow.asInstanceOf[ARROW[Any, X]]))

    override def x[C <: Element[C]](that: Quiver[A, C]): Quiver[A, B x C] =
      wrapArrow[X, (Y, Any), A, B x C](arrow x that.getArrow.asInstanceOf[ARROW[X, Any]])
    override lazy val source = wrapDot(arrow.source).asInstanceOf[Star[A]]
    override lazy val target = wrapDot(arrow.target).asInstanceOf[Star[B]]

    override def asElement : B = target.asElement(this)

    override def getArrow: ARROW[_, _] = arrow.asInstanceOf[ARROW[_, _]]
    override def sanityTest = arrow.sanityTest
    override def equals(other: Any): Boolean = other match {
      case that: QuiverWrapper[X, Y, A, B] =>
        arrow == that.arrow
      case _ => false
    }
  }

  private val standardVanillaStar = new ResultStore[StrictRef[DOT[Any]], Star[WrappedType[Any]]]({
    x => new VanillaStar(x.wrappedValue)
  })

  private val standardProductStar = new ResultStore[(StrictRef[Star[WrappedType[Any]]], StrictRef[Star[WrappedType[Any]]]),
    ProductStar[WrappedType[Any], WrappedType[Any]]](tupled {
    (x, y) => new ProductStar(x.wrappedValue, y.wrappedValue).asInstanceOf[ProductStar[WrappedType[Any], WrappedType[Any]]]
  })

  def wrapDot[X](dot: DOT[X]) : Star[WrappedType[X]] =
    standardVanillaStar(
      StrictRef(dot.asInstanceOf[DOT[Any]])
    ).asInstanceOf[Star[WrappedType[X]]]

  def wrapArrow[X, Y, A <: Element[A], B <: Element[B]](arrow: ARROW[X, Y]) : Quiver[A, B] =
    new QuiverWrapper[X, Y, A, B](arrow)

  def leftProjection[A <: Element[A], B <: Element[B]](left: Star[A], right: Star[B]) =
    for(x <- left x right)
      yield x.left

  def rightProjection[A <: Element[A], B <: Element[B]](left: Star[A], right: Star[B]) =
    for(x <- left x right)
      yield x.right

  // TODO: make sure any unnecessary slack in asElement(...)=>asElement(...) is unwound
  // TODO: are ResultStores used correctly here?
}
