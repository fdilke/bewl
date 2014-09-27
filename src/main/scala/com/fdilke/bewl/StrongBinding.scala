package com.fdilke.bewl

import com.fdilke.bewl.helper.{StrictRef, ResultStore}

import scala.Function._
import scala.collection.mutable

trait StrongBinding { topos : BaseTopos =>

  trait Element[T <: Element[T]] {
    val arrow: ARROW[Any, Any]
    val element: T
  }

  trait x[T <: Element[T], U <: Element[U]] extends Element[T x U] {
    val left: T
    val right: U
  }

  trait Star[T <: Element[T]] {
    val identity = Quiver[T, T](this, this, x => x)

    def x[U <: Element[U]](that: Star[U]): Star[T x U] =
      standardProductStar((
        StrictRef(this.asInstanceOf[Star[WrappedArrow[Any]]]),
        StrictRef(that.asInstanceOf[Star[WrappedArrow[Any]]])
        )).asInstanceOf[ProductStar[T, U]]

    val getDot: DOT[Any]
    def asElement(arrow: ARROW[Any, Any]) : T
  }

  case class Quiver[S <: Element[S], T <: Element[T]](
     source: Star[S],
     target: Star[T],
     function: S => T
    ) {
    def o[R <: Element[R]](that: Quiver[R, S]) =
      Quiver(that.source, target, function compose that.function)

    def x[U <: Element[U]](that: Quiver[S, U]) = {
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

  case class WrappedArrow[X](val arrow: ARROW[Any, Any]) extends Element[WrappedArrow[X]] {
    override val element: WrappedArrow[X] = this
  }

  class WrappedDot[X](dot: DOT[X]) extends Star[WrappedArrow[X]] {
    override val getDot: DOT[Any] = dot.asInstanceOf[DOT[Any]]

    override def asElement(arrow: ARROW[Any, Any]) =
      WrappedArrow(arrow)
  }

  class ProductStar[L <: Element[L], R <: Element[R]](
    l: Star[L], r: Star[R]
  ) extends Star[L x R] {
    override val getDot = (l.getDot x r.getDot).asInstanceOf[DOT[Any]]

    override def asElement(anArrow: ARROW[Any, Any]) = new x[L, R] { self: x[L, R] =>
      override val left: L = l.asElement(
        leftProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val right: R = r.asElement(
        rightProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](anArrow))
      override val arrow: ARROW[Any, Any] = anArrow
      override val element: x[L, R] = self
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

  def arrowAsFunction[X, Y, S <: Element[S], T <: Element[T]](
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

  def leftProjection[A <: Element[A], B <: Element[B]](left: Star[A], right: Star[B]) =
    Quiver[A x B, A](left x right, left, _.left)

  def rightProjection[A <: Element[A], B <: Element[B]](left: Star[A], right: Star[B]) =
    Quiver[A x B, B](left x right, right, _.right)


  /*
    trait Element[T <: Element[T]] {
      val arrow: ARROW[Any, Any]
      val target: Star[T]
      val element: T
    }

    trait x[T <: Element[T], U <: Element[U]] extends Element[T x U] {
      val left: T
      val right: U
    }

    trait Star[T <: Element[T]] {
      val identity: Quiver[T, T]

      def x[U <: Element[U]](that: Star[U]): Star[T x U]
      def map[U <: Element[U]](f: T => U): Quiver[T, U]
      def asQuiver[U <: Element[U]](element: U): Quiver[T, U] =
        Quiver[T, U](this, element.target,
          t => ???, // Quiver[T, U](this, element.target, ),
          element)
        // TODO: make this a method on element?

      def getDot: DOT[Any]
    }

    class WrappedType[X](val arrow: ARROW[Any, Any]) extends Element[WrappedType[X]] {
      val target: Star[WrappedType[X]] = wrapDot[X](arrow.target.asInstanceOf[DOT[X]])
      val element: WrappedType[X] = this
    }

    object WrappedType { // needed?
      def identity[X](dot: DOT[X]) = new WrappedType[X](dot.identity.asInstanceOf[ARROW[Any, Any]])
    }

    private abstract class StarWrapper[X, A <: Element[A]](dot: DOT[X], calcIdentityElement: () => A) extends Star[A] {
      override lazy val identity = Quiver[A, A](this, this, a => a, calcIdentityElement())
      override def x[T <: Element[T]](that: Star[T]): Star[A x T] =
        standardProductStar((
          StrictRef(this.asInstanceOf[Star[WrappedType[Any]]]),
          StrictRef(that.asInstanceOf[Star[WrappedType[Any]]])
        )).asInstanceOf[Star[A x T]]
      override def map[U <: Element[U]](f: A => U): Quiver[A, U] = {
        println("Applying map... 1")
        val a: A = identity.element
        println("Applying map... 2")
        println(s"Applying map... a is: $a :=> ${a.arrow}")
        val u: U = f(a)
        println(s"Applying map... 3. u = $u :=> ${u.arrow}")
        println(s"Applying map... 4. rebasing from: ${this.getDot}")
        asQuiver(u)
        // TODO compactify
      }

      override def getDot: DOT[Any] = dot.asInstanceOf[DOT[Any]]
      override def equals(other: Any): Boolean = other match {
        case that: StarWrapper[X, A] => this eq that
        case _ => false
      }
    }

    private class VanillaStar[X](dot: DOT[X]) extends
      StarWrapper[X, WrappedType[X]](dot, () => WrappedType.identity(dot)) {

      if (dot.isInstanceOf[mutable.WrappedArray[_]])
        throw new IllegalArgumentException("whaaat?")
      else
        println(s"Perfectly sensible: ${dot.getClass} ::== $dot")
    }

    def productIdentity[L <: Element[L], R <: Element[R]](
      l: Star[L], r: Star[R]
    ): L x R = ??? // sensible?

    private class ProductStar[L <: Element[L], R <: Element[R]](l: Star[L], r: Star[R])
      extends StarWrapper[(Any, Any), L x R](l.getDot x r.getDot, () => productIdentity(l, r)) {
  //      override lazy val left: L =
  //        new QuiverWrapper(
  //          quiver.source,
  //          l,
  //          leftProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](quiver.getArrow)
  //        ).element
  //      override lazy val right: R =
  //        new QuiverWrapper(
  //          quiver.source,
  //          r,
  //          rightProjection(l.getDot, r.getDot).asInstanceOf[ARROW[Any, Any]](quiver.getArrow)
  //        ).element
    }

    case class Quiver[S <: Element[S], T <: Element[T]](
        source: Star[S],
        target: Star[T],
        function: S => T,
        element: T
    ) extends Element[T] {
      val arrow = element.arrow // TODO can this redirection be avoided?
      def apply[R <: Element[R]](quiver: Quiver[R, S]) =
        Quiver[R, T](
          quiver.source,
          target,
          function compose quiver.function, // r => function(quiver.function(r)),
          function(quiver.element)
        )

      def x[U <: Element[U]](that: Quiver[S, U]): Quiver[S, T x U] = {
        val product = target x that.target
        val productArrow = element.arrow x that.element.arrow
        Quiver[S, T x U](
          source,
          product,
          s => ???,
          new x[T, U] {
            override val left: T = Quiver.this.element
            override val right: U = that.element
            override val target: Star[x[T, U]] = product
            override val arrow: ARROW[Any, Any] = productArrow.asInstanceOf[ARROW[Any, Any]]
            override val element: x[T, U] = this
          }
        )
      }

      def sanityTest = arrow.sanityTest
      override def equals(other: Any): Boolean = other match {
        case that: Quiver[S, T] =>
          element.arrow == that.element.arrow
        case _ => false
      }
    }

    private val standardVanillaStar = new ResultStore[StrictRef[DOT[Any]], Star[WrappedType[Any]]]({
      x => new VanillaStar(x.wrappedValue)
    })

    private val standardProductStar = new ResultStore[(StrictRef[Star[WrappedType[Any]]], StrictRef[Star[WrappedType[Any]]]),
      ProductStar[WrappedType[Any], WrappedType[Any]]](tupled {
      (x, y) => new ProductStar(x.wrappedValue, y.wrappedValue)
    })

    def wrapDot[X](dot: DOT[X]) : Star[WrappedType[X]] =
  //  if (true) throw new IllegalArgumentException("xxx!")
  //  else
      standardVanillaStar(
        StrictRef(dot.asInstanceOf[DOT[Any]])
      ).asInstanceOf[Star[WrappedType[X]]]

    def wrapArrow[X, Y](arrow: ARROW[X, Y]) : Quiver[WrappedType[X], WrappedType[Y]] =
        Quiver[WrappedType[X], WrappedType[Y]] (
          wrapDot(arrow.source),
          wrapDot(arrow.target),
          a => wrapArrow[Any, Y](arrow.asInstanceOf[ARROW[Any, Y]](a.element.arrow)).element,
          new WrappedType[Y](arrow.asInstanceOf[ARROW[Any, Any]])
        )

  //  def wrapArrow[X, Y, A <: Element[A], B <: Element[B]](arrow: ARROW[X, Y]) : Quiver[A, B] =
  //    new QuiverWrapper[X, Y, A, B](
  //      wrapDot(arrow.source).asInstanceOf[Star[A]],
  //      wrapDot(arrow.target).asInstanceOf[Star[B]],
  //      arrow)

    def leftProjection[A <: Element[A], B <: Element[B]](left: Star[A], right: Star[B]) = {
      println(s"left x right is: ${left x right}")
      for (x <- left x right)
        yield {
          println(s"x is: $x")
          println(s"x.left is: ${x.left}")
          x.left
        }
    }

    def rightProjection[A <: Element[A], B <: Element[B]](left: Star[A], right: Star[B]) =
      for(x <- left x right)
        yield x.right

    // TODO: make sure any unnecessary slack in asElement(...)=>asElement(...) is unwound
    // TODO: are ResultStores used correctly here?
  */
}
