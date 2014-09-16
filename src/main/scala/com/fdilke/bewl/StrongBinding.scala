package com.fdilke.bewl

trait StrongBinding { topos : BaseTopos =>

  trait Element[T <: Element[T]] {}

  trait Star[T <: Element[T]] {
    def identity: Quiver[T, T]
  }
  trait Quiver[S <: Element[S], T <: Element[T]] {
    def apply[R <: Element[R]](quiver: Quiver[R, S]): Quiver[R, T]

    def getArrow: ARROW[_, _]
  }

  trait WrappedType[X] extends Element[WrappedType[X]] {}

  private class StarWrapper[X](dot: DOT[X]) extends Star[WrappedType[X]] {
    def identity = wrap(dot.identity)
  }

  private class QuiverWrapper[X, Y](private val arrow: ARROW[X, Y]) extends Quiver[WrappedType[X], WrappedType[Y]] {
    def apply[R <: Element[R]](quiver: Quiver[R, WrappedType[X]]): Quiver[R, WrappedType[Y]] =
      wrap(arrow(quiver.getArrow.asInstanceOf[ARROW[R, X]])).asInstanceOf[Quiver[R, WrappedType[Y]]]

    def getArrow: ARROW[_, _] = arrow.asInstanceOf[ARROW[_, _]]

    override def equals(other: Any): Boolean = other match {
      case that: QuiverWrapper[X, Y] =>
        arrow == that.arrow
      case _ => false
    }

  }

  def wrap[X](dot: DOT[X]) : Star[WrappedType[X]] = new StarWrapper[X](dot)

  def wrap[X, Y](arrow: ARROW[X, Y]) : Quiver[WrappedType[X], WrappedType[Y]] =
    new QuiverWrapper[X, Y](arrow)
}
