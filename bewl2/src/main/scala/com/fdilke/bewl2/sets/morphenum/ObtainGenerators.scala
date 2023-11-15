package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.{BaseSets, Sets}
import com.fdilke.bewl2.helper.Memoize

trait ObtainGenerators extends BaseSets:
  trait GeneratorObtainer[M, ACTION[_]]:
    def findGenerators[A](action: ACTION[A]): Seq[A]

  object GeneratorObtainer:
    def forMonoid[M](
      monoid: Monoid[M]
    ): GeneratorObtainer[M, monoid.Action] =
      val monoidElements: Set[M] =
        monoid.dot.dot
      new GeneratorObtainer[M, monoid.Action]:
        def findGenerators[A](
          action: monoid.Action[A]
        ): Seq[A] =
          val monogenic: A => Set[A] =
            Memoize[A, Set[A]]:
              a => monoidElements map { m =>
                action.actionMultiply(a, m)
              }
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
                  generated = generated union monogenic(x)
                )
          def pass(generators: Seq[A]): Accumulator =
            generators.foldLeft(new Accumulator()) { _.foldIn(_) }
          val firstPass: Accumulator =
            pass(action.dot.dot.toSeq)
          val secondPass: Accumulator =
            pass(firstPass.generators)
          secondPass.generators
