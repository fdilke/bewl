package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets

trait FindGroupActionGenerators extends BaseSets:
  trait GroupActionGeneratorFinder[ACTION[_]]:
    def apply[A](action: ACTION[A]): Seq[A]

  object GroupActionGeneratorFinder:
    def forGroup[G](
      group: Group[G]
    ): GroupActionGeneratorFinder[group.Action] =
      val groupElements: Set[G] =
        group.dot.dot
      new GroupActionGeneratorFinder[group.Action]:
        def apply[A](action: group.Action[A]): Seq[A] =
          def orbit(a: A): Set[A] =
            groupElements map { action.actionMultiply(a, _) }
          orbitFindGenerators[A](action.dot.dot.toSeq, orbit)

  private def orbitFindGenerators[A](
    elements: Seq[A],
    orbit: A => Set[A]
  ): Seq[A] =
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
            generated = generated union orbit(x)
          )
    elements.foldLeft(new Accumulator) { _.foldIn(_) }.generators

