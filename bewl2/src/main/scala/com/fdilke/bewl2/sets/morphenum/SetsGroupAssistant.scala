package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets

trait SetsGroupAssistant extends BaseSets:
  Ɛ: FindGroupGenerators =>

  override protected val groupAssistant: GroupAssistant = LocalGroupAssistant

  object LocalGroupAssistant extends GroupAssistant:
    override def actionAnalyzer[G : Dot](group: Group[G]) : GroupActionAnalyzer[group.Action] =
      new GroupActionAnalyzer[group.Action]:
        override type ACTION_ANALYSIS[A] = LocalGroupActionAnalysis[A]
        override def analyze[A](
          action: group.Action[A]
        ) : LocalGroupActionAnalysis[A] =
          new LocalGroupActionAnalysis[A](action)        

        override def enumerateMorphisms[A, B](
          src: LocalGroupActionAnalysis[A],
          target: LocalGroupActionAnalysis[B]
        ): Iterable[A ~> B] =
          src.enumerateMorphisms(target)

        class LocalGroupActionAnalysis[A](
          val action: group.Action[A]
        ):
          def enumerateMorphisms[B](
            analysisB: LocalGroupActionAnalysis[B]
          ): Iterable[A ~> B] =
            ???
