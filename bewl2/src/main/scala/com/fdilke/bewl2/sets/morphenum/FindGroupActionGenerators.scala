package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets

trait FindGroupActionGenerators extends BaseSets:
  trait GroupActionGeneratorFinder[ACTION[_]]:
    def apply[A](action: ACTION[A]): Seq[A]

  object GroupActionGeneratorFinder:
    def forGroup[G](
      group: Group[G]
    ): GroupActionGeneratorFinder[group.Action] =
      ???

