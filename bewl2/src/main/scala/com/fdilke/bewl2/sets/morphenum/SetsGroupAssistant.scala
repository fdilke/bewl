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

        val findGenerators: GroupActionGeneratorFinder[group.Action] =
          GroupActionGeneratorFinder.forGroup(group)          

        def bulkJoin[X, Y, Z](
          inputs: Seq[X],
          candidates: X => Iterable[Y],
          assignment: (X, Y) => Z,
          assignmentZero: Z,
          join: (Z, Z) => Z
        ): Iterable[Z] =
          // class Accumulator(base: Z = assignmentZero) extends Iterable[Z]:
          def foldIn(it: Iterable[Z], x: X): Iterable[Z] =
            for
              y <- candidates(x)
              assign = assignment(x, y)
              z <- it
            yield
              join(z, assign)
            
          inputs.foldLeft(Iterable[Z](assignmentZero))(foldIn)

        class LocalGroupActionAnalysis[A](
          val action: group.Action[A]
        ):
          lazy val generators: Seq[A] =
            findGenerators(action)
          def enumerateMorphisms[B](
            analysisB: LocalGroupActionAnalysis[B]
          ): Iterable[A ~> B] =
            def assign(a: A, b: B): Map[A, B] =
              (group.dot.dot.map:
                g => action.actionMultiply(a, g) ->
                  analysisB.action.actionMultiply(b, g)
              ).toMap
            def join(map1: Map[A, B], map2: Map[A, B]): Map[A, B] =
              map1 ++ map2
            bulkJoin[A, B, Map[A, B]](
              inputs = generators,
              candidates = a => findCandidates[G, A, B](group)(a, action, analysisB.action),
              assignment = assign,
              assignmentZero = Map.empty[A, B],
              join = join
            )
            // val generatorAssignments: Seq[(A, Map[A, B])] =
            //   for
            //     x <- generators
            //     y <- findCandidates(group)(x, action, analysisB.action)
            //     assignment = assign(x, y)
            //   yield
            //     x -> assignment
            // class Accumulator(
            //   assignments: Set[Map[A, B]] = Set.empty
            // ):
            //   def foldIn(assignment: (A, Map[A, B])): Accumulator =
            //     ???
            //   val thing: Iterable[A ~> B] =
            //     assignments
            // generatorAssignments.foldLeft(
            //   new Accumulator
            // ) { _.foldIn(_) }.thing
