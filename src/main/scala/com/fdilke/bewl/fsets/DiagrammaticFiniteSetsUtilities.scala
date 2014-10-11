package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.{FiniteSetsDot, FiniteSetsArrow}
import DiagrammaticFiniteSets.Power

object DiagrammaticFiniteSetsUtilities {
  def set[T](elements: T*) = FiniteSetsDot(elements)

  def arrow[S, T](source: FiniteSetsDot[S], target: FiniteSetsDot[T], map: (S, T)*) =
    FiniteSetsArrow[S, T](source, target, Map(map:_*))

  def cartesian[A](factors: Seq[Seq[A]]): Traversable[Seq[A]] = factors match {
    case Nil => Traversable(Seq())
    case head :: tail =>
      for (h <- head; sequence <- cartesian[A](tail))
      yield(h +: sequence)
  }

  def allMaps[A, B](source: Traversable[A], target: Traversable[B]): Traversable[A=>B] =
    new Traversable[A => B] {
      override def foreach[U](func: (A => B) => U): Unit =
        if (source.isEmpty)
          func(_ => ???)
        else
          for (f <- allMaps(source.tail, target);
               choice <- target) {
            func { x => if (x == source.head) choice else f(x)}
          }
    }

  def nullaryOperator[X](dot: FiniteSetsDot[X], constant: X) =
    FiniteSetsArrow[Power[X], X](
      dot ^ 0, dot, Map(() -> constant)
    )

  def unaryOperator[X](dot: FiniteSetsDot[X], entries: (X, X)*) =
    FiniteSetsArrow[Power[X], X](
      dot ^ 1, dot, Map(entries:_*)
    )

  def unaryOperator[X](dot: FiniteSetsDot[X], unaryFunc: X => X): FiniteSetsArrow[Power[X], X] =
    unaryOperator(dot, dot.map { x => (x, unaryFunc(x)) }.toList: _*)

  def binaryOperator[X](dot: FiniteSetsDot[X], entries: ((X, X), X)*) =
    FiniteSetsArrow[Power[X], X](
      dot ^ 2, dot, Map(entries:_*)
    )

  def binaryOperator[X](dot: FiniteSetsDot[X], binaryFunc: (X, X) => X): FiniteSetsArrow[Power[X], X] =
    binaryOperator(dot, (dot x dot).map { case (x, y) => ((x, y), binaryFunc(x,y)) }.toList: _*)
}

