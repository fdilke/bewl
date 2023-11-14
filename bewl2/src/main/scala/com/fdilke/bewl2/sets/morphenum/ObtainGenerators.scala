package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.{BaseSets, Sets}

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
                  generated = monoidElements map { m =>
                    action.actionMultiply(x, m)
                  } union generated
                )
          val firstPass: Accumulator =
            action.dot.dot.toSeq.foldLeft(
              new Accumulator()
            ) { 
                _.foldIn(_)
              }
          val secondPass: Accumulator =
            firstPass.generators.foldLeft(
              new Accumulator()
            ) { 
                _.foldIn(_)
              }
          secondPass.generators
