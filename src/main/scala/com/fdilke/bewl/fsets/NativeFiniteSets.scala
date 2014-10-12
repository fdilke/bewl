package com.fdilke.bewl.fsets

import com.fdilke.bewl.helper.{ResultStore, StrictRef}
import com.fdilke.bewl.topos.{Wrappings, Topos}

class NativeFiniteSets extends Topos
  with Wrappings[Traversable, FiniteSetsPreQuiver] {

  override type ELEMENT = Any
  override type STAR[S <: ELEMENT] = FiniteSetsStar[S]
  override type QUIVER[S <: ELEMENT, T <: ELEMENT] = FiniteSetsQuiver[S, T]
  override type UNIT = Unit
  override type TRUTH = Boolean
  override val I: STAR[UNIT] = ???
  override val omega: STAR[TRUTH] = ???
  override val truth: QUIVER[UNIT, TRUTH] = ???

  class FiniteSetsStar[S](elements: Traversable[S]) extends Star[S] with Traversable[S] {
    override val toI: QUIVER[S, UNIT] = ???

    override def >[T <: ELEMENT](that: STAR[T]) = ???

    override def x[T <: ELEMENT](that: STAR[T]): STAR[x[S, T]] = ???

    override def apply[T <: ELEMENT](target: STAR[T])(f: (S) => T): QUIVER[S, T] = ???

    override def sanityTest =
      for (x <- elements ; y <- elements)
        x == y

    override def foreach[U](f: S => U) = elements.foreach(f)
  }

  class FiniteSetsQuiver[S, T](
    val source: FiniteSetsStar[S],
    val target: FiniteSetsStar[T],
    private[NativeFiniteSets] val function: S => T
  ) extends Quiver[S, T] {

    override def \[U <: ELEMENT](monic: QUIVER[U, T]): QUIVER[S, U] = ???

    override def sanityTest =
      if (!source.map(function).forall(x => target.exists(_ == x))) {
        throw new IllegalArgumentException("Map values not in target")
      }

    override def ?=(that: QUIVER[S, T]): EqualizingStar[S] with STAR[EqualizingElement[S] with ELEMENT] = ???

    override def x[U <: ELEMENT](that: QUIVER[S, U]): QUIVER[S, x[T, U]] = ???

    override def apply(s: S): T = function(s)

    override def o[R <: ELEMENT](that: QUIVER[R, S]) =
      new FiniteSetsQuiver[R, T](that.source, target, function compose that.function)

    override val chi: QUIVER[T, TRUTH] = ???
  }

  private val standardStar = new ResultStore[StrictRef[Traversable[Any]], STAR[Any]] (
    x => new FiniteSetsStar(x.wrappedValue)
  )

  // wrapping API

  override type WRAPPER[T] = T

  override def functionAsQuiver[S, T](source: STAR[S], target: STAR[T], f: S => T): QUIVER[S, T] = ???

  override def quiver[S, T](prequiver: FiniteSetsPreQuiver[S, T]) =
    new FiniteSetsQuiver[S, T](star(prequiver.source), star(prequiver.target), prequiver.function)

  override def star[T](input: Traversable[T]) =
    standardStar(
      StrictRef(input.asInstanceOf[Traversable[T]])
    ).asInstanceOf[STAR[T]]

  override def bifunctionAsBiQuiver[L, R, T](left: STAR[L], right: STAR[R], target: STAR[T], bifunc: (L, R) => T): BiQuiver[L, R, T] = ???
}

case class FiniteSetsPreQuiver[S, T](
  source: Traversable[S],
  target: Traversable[T],
  function: S => T
)
