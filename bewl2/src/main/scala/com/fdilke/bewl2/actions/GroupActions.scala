package com.fdilke.bewl2.actions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.topos.ProductMappable
import com.fdilke.bewl2.topos.{ PreTopos, Topos }
import com.fdilke.bewl2.topos.ProductMappable.⊕

trait GroupActions[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >]:
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  def toposOfGroupActions[M: Dot](
    group: Group[M]
  ): Topos[group.Action, CTXT, VOID, UNIT, BEWL, >] =
    new Topos[group.Action, CTXT, VOID, UNIT, BEWL, >](      
      new PreTopos[
        group.Action, CTXT, VOID, UNIT, BEWL, >
      ]:
        override val unitDot: group.Action[UNIT] =
          group.withTrivialAction[UNIT, group.Action[UNIT]]:
            summon[group.Action[UNIT]]

        override val zeroDot: group.Action[VOID] =
          group.withVoidAction:
            summon[group.Action[VOID]]

        override val omegaDot: group.Action[BEWL] =
          group.withTrivialAction[BEWL, group.Action[BEWL]]:
            summon[group.Action[BEWL]]

        override val truth: UNIT ~> BEWL =
          Ɛ.truth

        override def equalArrows[X, Y](
          dotX: group.Action[X],
          dotY: group.Action[Y],
          f1: X ~> Y,
          f2: X ~> Y
        ): Boolean =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.RichArrow(f1) =!= f2

        override def uncachedProductObject[X, Y](
          dotX: group.Action[X],
          dotY: group.Action[Y]
        ): group.Action[(X, Y)] =
          given Ɛ.Dot[Y] = dotY.dot
          dotX.x(dotY)

        override def uncachedExponentialObject[X, Y](
          dotX: group.Action[X],
          analysisX: analyzer.ACTION_ANALYSIS[X],
          dotY: group.Action[Y],
          analysisY: analyzer.ACTION_ANALYSIS[Y]
        ): group.Action[X > Y] =
          analyzer.makeExponential(analysisX, analysisY)

        override def evaluation[X, Y](
          dotX: group.Action[X],
          dotY: group.Action[Y]
        ): (X > Y, X) ~> Y =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.evaluation[X, Y]

        override def transpose[X, Y, Z](
          dotX: group.Action[X],
          dotY: group.Action[Y],
          dotZ: group.Action[Z],
          xy2z: ((X, Y)) ~> Z
        ): X ~> (Y > Z) =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          given Ɛ.Dot[Z] = dotZ.dot
          Ɛ.transpose[X, Y, Z](xy2z)

        override def sanityTest[X](
          dotX: group.Action[X]
        ): Unit =
          dotX.sanityTest

        override def sanityTest[X, Y](
          dotX: group.Action[X],
          dotY: group.Action[Y],
          f: X ~> Y
        ): Unit =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          given group.Action[X] = dotX
          given group.Action[Y] = dotY
          Ɛ.sanityTest[X, Y](f)
          assert { group.actions.isMorphism(f) }

        override def enumerateMorphisms[X, Y](
          dotX: group.Action[X],
          analysisX: analyzer.ACTION_ANALYSIS[X],
          dotY: group.Action[Y],
          analysisY: analyzer.ACTION_ANALYSIS[Y]
        ): Iterable[X ~> Y] =
          analyzer.enumerateMorphisms(analysisX, analysisY)

        override def fromZero[X](
          dotX: group.Action[X]
        ): VOID ~> X =
          given Ɛ.Dot[X] = dotX.dot
          Ɛ.fromZero[X]

        override def toUnit[X](
          dotX: group.Action[X]
        ): X ~> UNIT =
          given Ɛ.Dot[X] = dotX.dot
          Ɛ.toUnit[X]

        override def doEqualizer[X, Y, RESULT](
          dotX: group.Action[X],
          dotY: group.Action[Y],
          f: X ~> Y,
          f2: X ~> Y
        )(
          block: [A] => RawEqualizer[A, X] => group.Action[A] => RESULT
        ): RESULT =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.doEqualizer(f, f2):
            [A] => (_: Dot[A]) ?=> (equalizer: Ɛ.Equalizer[A, X]) =>
              block[A](
                new RawEqualizer[A, X]:
                  override val inclusion: A ~> X =
                    equalizer.inclusion
                  override def restrict[R](
                    dotR: group.Action[R],
                    arrow: R ~> X
                  ): R ~> A =
                    given Ɛ.Dot[R] = dotR.dot
                    equalizer.restrict[R](arrow)
              )(
                group.Action[A](
                  equalizer.restrict {
                    case a ⊕ m =>
                      dotX.actionMultiply(
                        equalizer.inclusion(a),
                        m
                      )
                  }
                )
              )

        override def chiForMonic[X, Y](
          dotX: group.Action[X],
          dotY: group.Action[Y],
          monic: X ~> Y
        ): Y ~> BEWL =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.chiForMonic[X, Y](monic)

        override def backDivideMonic[X, Y, A](
          dotX: group.Action[X],
          dotY: group.Action[Y],
          dotA: group.Action[A],
          arrow: X ~> Y,
          monic: A ~> Y
        ): X ~> A =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          given Ɛ.Dot[A] = dotA.dot
          Ɛ.backDivideMonic[X, Y, A](arrow, monic)

        val analyzer: GroupActionAnalyzer[group.Action] =
          Ɛ.groupAssistant.actionAnalyzer[M](group)

        override type TOOLKIT[A] = analyzer.ACTION_ANALYSIS[A]
        override val toolkitBuilder: ToolkitBuilder = new ToolkitBuilder:
            override def buildToolkit[A](
              theAction: group.Action[A]
            ): analyzer.ACTION_ANALYSIS[A] =
              analyzer.analyze(theAction)
    )

  protected val groupAssistant: GroupAssistant =
    DefaultGroupAssistant

  trait GroupAssistant:
    def actionAnalyzer[G : Dot](group: Group[G]) : GroupActionAnalyzer[group.Action]

  trait GroupActionAnalyzer[ACTION[_]]:
    type ACTION_ANALYSIS[A]
    def analyze[A](
      action: ACTION[A]
    ) : ACTION_ANALYSIS[A]
    def makeExponential[A, B](
      analysisA: ACTION_ANALYSIS[A],
      analysisB: ACTION_ANALYSIS[B]
    ): ACTION[A > B]
    def enumerateMorphisms[A, B](
      analysisA: ACTION_ANALYSIS[A],
      analysisB: ACTION_ANALYSIS[B]
    ): Iterable[A ~> B]

  object DefaultGroupAssistant extends GroupAssistant:
    override def actionAnalyzer[G : Dot](
      group: Group[G]
    ) : GroupActionAnalyzer[group.Action] =
      new GroupActionAnalyzer[group.Action]:
        override type ACTION_ANALYSIS[A] = DefaultGroupActionAnalysis[A]
        override def analyze[A](
          action: group.Action[A]
        ) : DefaultGroupActionAnalysis[A] =
          new DefaultGroupActionAnalysis[A](action)

        override def makeExponential[A, B](
          analysisA: DefaultGroupActionAnalysis[A],
          analysisB: DefaultGroupActionAnalysis[B]
        ): group.Action[A > B] = 
          analysisA.makeExponential(analysisB)

        override def enumerateMorphisms[A, B](
          src: DefaultGroupActionAnalysis[A],
          target: DefaultGroupActionAnalysis[B]
        ): Iterable[A ~> B] =
          src.enumerateMorphisms(target)

        class DefaultGroupActionAnalysis[A](
          val action: group.Action[A]
        ):
          given Dot[A] = action.dot
          given Dot[G] = group.dot
          def makeExponential[B](
            analysisB: DefaultGroupActionAnalysis[B]
          ): group.Action[A > B] =
            given Dot[B] = analysisB.action.dot
            val eval: (A > B, A) ~> B =
              evaluation[A, B]
            val isMorphism: (A > B) ~> BEWL =
              ∀[A > B, A, G] {
                (phi, a, g) =>
                  analysisB.action.actionMultiply(eval(phi, a), g) =?= 
                    eval(phi, action.actionMultiply(a, g))
              }
            isMorphism.whereTrue {
              [F] => (_ : Dot[F]) ?=> (equalizer: Equalizer[F, A > B]) =>
                val action: group.Action[F] =
                  group.Action {
                    equalizer.restrict(
                      transpose[(F, G), A, B]{ case (f ⊕ m) ⊕ a =>
                        val phi: CTXT[A > B] =
                          equalizer.inclusion(f)
                        eval(phi, a)
                      }
                    )
                  }
                // awful, fix via refactoring Equalizer via Tagged to provide an implicit =:= rather than inclusion
                action.asInstanceOf[group.Action[A > B]]
              }
              
          def enumerateMorphisms[B](
            analysisB: DefaultGroupActionAnalysis[B]
          ): Iterable[A ~> B] =
            given Dot[B] = analysisB.action.dot
            morphisms[A, B] filter:
              (f: A ~> B) => ∀[(A, G)]: 
                case a ⊕ g =>
                  f(action.actionMultiply(a, g)) =?=
                    analysisB.action.actionMultiply(f(a), g)

