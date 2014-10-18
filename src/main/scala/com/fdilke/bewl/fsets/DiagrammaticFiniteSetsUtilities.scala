package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.{DiagrammaticFiniteSetsDot, DiagrammaticFiniteSetsArrow}
import DiagrammaticFiniteSets.Power

object DiagrammaticFiniteSetsUtilities {
  def set[T](elements: T*) = DiagrammaticFiniteSetsDot(elements)

  def arrow[S, T](source: DiagrammaticFiniteSetsDot[S], target: DiagrammaticFiniteSetsDot[T], map: (S, T)*) =
    DiagrammaticFiniteSetsArrow[S, T](source, target, Map(map:_*))

  def cartesian[A](factors: Seq[Seq[A]]): Traversable[Seq[A]] = factors match {
    case Nil => Traversable(Seq())
    case head :: tail =>
      for (h <- head; sequence <- cartesian[A](tail))
        yield(h +: sequence)
  }

  def allMaps[A, B](source: Traversable[A], target: Traversable[B]) : Traversable[A => B] =
    new Traversable[A => B] {
      override def foreach[U](enumerate: (A => B) => U) =
        if (source.isEmpty)
          enumerate { _ => ??? }
        else
          for (f <- allMaps(source.tail, target);
               choice <- target) {
            enumerate { x => if (x == source.head) choice else f(x)}
          }}

  def nullaryOperator[X](dot: DiagrammaticFiniteSetsDot[X], constant: X) =
    DiagrammaticFiniteSetsArrow[Power[X], X](
      dot ^ 0, dot, Map(() -> constant)
    )

  def unaryOperator[X](dot: DiagrammaticFiniteSetsDot[X], entries: (X, X)*) =
    DiagrammaticFiniteSetsArrow[Power[X], X](
      dot ^ 1, dot, Map(entries:_*)
    )

  def unaryOperator[X](dot: DiagrammaticFiniteSetsDot[X], unaryFunc: X => X): DiagrammaticFiniteSetsArrow[Power[X], X] =
    unaryOperator(dot, dot.map { x => (x, unaryFunc(x)) }.toList: _*)

  def binaryOperator[X](dot: DiagrammaticFiniteSetsDot[X], entries: ((X, X), X)*) =
    DiagrammaticFiniteSetsArrow[Power[X], X](
      dot ^ 2, dot, Map(entries:_*)
    )

  def binaryOperator[X](dot: DiagrammaticFiniteSetsDot[X], binaryFunc: (X, X) => X): DiagrammaticFiniteSetsArrow[Power[X], X] =
    binaryOperator(dot, (dot x dot).map { case (x, y) => ((x, y), binaryFunc(x,y)) }.toList: _*)
}

