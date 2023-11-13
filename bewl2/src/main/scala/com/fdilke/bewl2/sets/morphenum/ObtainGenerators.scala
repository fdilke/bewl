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
          action.dot.dot.toSeq
