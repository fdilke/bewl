package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets

trait SetsGroupAssistant extends BaseSets:
  Ɛ: FindGroupActionGenerators =>

  override protected val groupAssistant: GroupAssistant = LocalGroupAssistant

  object LocalGroupAssistant extends GroupAssistant:
    def findCandidates[G, X, Y](group: Group[G])(
      x: X, 
      sourceAction: group.Action[X],
      targetAction: group.Action[Y]
    ): Set[Y] =
      val groupElements: Set[G] = group.dot.dot
      val stabilizer: Set[G] = groupElements.filter:
        g => x == sourceAction.actionMultiply(x, g)
      println("Stabilizer is: " + stabilizer)
      targetAction.dot.dot.filter:
        y => stabilizer.map { targetAction.actionMultiply(y, _) }.size == 1

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
