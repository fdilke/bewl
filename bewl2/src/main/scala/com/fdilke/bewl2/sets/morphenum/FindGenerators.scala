package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.{BaseSets, Sets}
import com.fdilke.bewl2.helper.{ Memoize}

trait FindGenerators extends BaseSets:
  trait GeneratorFinder[M, ACTION[_]]:
    def apply[A](action: ACTION[A]): Seq[A]

  object GeneratorFinder:
    def forMonoid[M](
      monoid: Monoid[M]
    ): GeneratorFinder[M, monoid.Action] =
      val monoidElements: Set[M] =
        monoid.dot.dot
      new GeneratorFinder[M, monoid.Action]:
        override def apply[A](
          action: monoid.Action[A]
        ): Seq[A] =
          def predecessors(a: A): Set[A] =
            monoidElements map { action.actionMultiply(a, _) }
          qoFindGenerators[A](action.dot.dot.toSeq, predecessors)

  private def qoFindGenerators[A](
    elements: Seq[A],
    uncachedPredecessors: A => Set[A]
  ): Seq[A] =
    val predecessors: A => Set[A] =
      Memoize[A, Set[A]](uncachedPredecessors)
    class Accumulator(
      val generators: Seq[A] = Seq.empty,
      val generated: Set[A] = Set.empty
    ):
      def foldIn(x: A): Accumulator =
        if generated contains x then
          this
        else
          Accumulator(
            generators = x +: generators,
            generated = generated union predecessors(x)
          )
    def pass(generators: Seq[A]): Seq[A] =
      generators.foldLeft(new Accumulator) { _.foldIn(_) }.generators
    pass(pass(elements))
