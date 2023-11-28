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
          analysisX: Unit,
          dotY: group.Action[Y],
          analysisY: Unit
        ): group.Action[X > Y] =
          ???

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
          analysisX: Unit,
          dotY: group.Action[Y],
          analysisY: Unit
        ): Iterable[X ~> Y] =
          ??? // TODO: use modified/simplified version of action algorithm?

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

        // TODO: will need an analyzer of some kind, but for groups
        // val analyzer: ActionAnalyzer[monoid.Action, monoid.InternalMap] =
        //   Ɛ.monoidAssistant.actionAnalyzer[M](monoid)

        override type TOOLKIT[A] = Unit
        override val toolkitBuilder: ToolkitBuilder = new ToolkitBuilder:
            override def buildToolkit[A](
              theAction: group.Action[A]
            ): Unit = ()
        // override type TOOLKIT[A] = analyzer.ACTION_ANALYSIS[A]
        // override val toolkitBuilder: MyToolkitBuilder.type =
        //   MyToolkitBuilder

        // object MyToolkitBuilder extends ToolkitBuilder:
        //     override def buildToolkit[A](
        //       theAction: monoid.Action[A]
        //     ): analyzer.ACTION_ANALYSIS[A] =
        //       analyzer.analyze(theAction)
    )

  // protected val monoidAssistant: MonoidAssistant =
  //   DefaultMonoidAssistant

  // trait MonoidAssistant:
  //   def actionAnalyzer[M : Dot](monoid: Monoid[M]) : ActionAnalyzer[monoid.Action, monoid.InternalMap]

  // trait ActionAnalyzer[ACTION[_], INTERNAL_MAP[_, _]]:
  //   type ACTION_ANALYSIS[A]
  //   def analyze[A](
  //     action: ACTION[A]
  //   ) : ACTION_ANALYSIS[A]
  //   def makeExponential[A, B](
  //     analysisA: ACTION_ANALYSIS[A],
  //     analysisB: ACTION_ANALYSIS[B]
  //   ): ACTION[INTERNAL_MAP[A, B]]
  //   def enumerateMorphisms[A, B](
  //     analysisA: ACTION_ANALYSIS[A],
  //     analysisB: ACTION_ANALYSIS[B]
  //   ): Iterable[A ~> B]

  // object DefaultMonoidAssistant extends MonoidAssistant:
  //   override def actionAnalyzer[M : Dot](
  //     monoid: Monoid[M]
  //   ) : ActionAnalyzer[monoid.Action, monoid.InternalMap] =
  //     new ActionAnalyzer[monoid.Action, monoid.InternalMap]:
  //       override type ACTION_ANALYSIS[A] = DefaultActionAnalysis[A]
  //       override def analyze[A](
  //         action: monoid.Action[A]
  //       ) : DefaultActionAnalysis[A] =
  //         new DefaultActionAnalysis[A](action)

  //       override def makeExponential[A, B](
  //         analysisA: DefaultActionAnalysis[A],
  //         analysisB: DefaultActionAnalysis[B]
  //       ): monoid.Action[monoid.InternalMap[A, B]] = 
  //         analysisA.makeExponential(analysisB)

  //       override def enumerateMorphisms[A, B](
  //         src: DefaultActionAnalysis[A],
  //         target: DefaultActionAnalysis[B]
  //       ): Iterable[A ~> B] =
  //         src.enumerateMorphisms(target)

  //       class DefaultActionAnalysis[A](
  //         val action: monoid.Action[A]
  //       ):
  //         given Dot[A] = action.dot
  //         given Dot[M] = monoid.dot
  //         def makeExponential[B](
  //           analysisB: DefaultActionAnalysis[B]
  //         ): monoid.Action[monoid.InternalMap[A, B]] =
  //           given Dot[B] = analysisB.action.dot
  //           val eval: ((M, A) > B, (M, A)) ~> B =
  //             evaluation[(M, A), B]
  //           val isMorphism: ((M, A) > B) ~> BEWL =
  //             ∀[(M, A) > B, M, A, M] {
  //               (phi, m, a, n) =>
  //                 analysisB.action.actionMultiply(eval(phi, m ⊕ a), n) =?= 
  //                   eval(phi, monoid.multiply(m, n) ⊕ action.actionMultiply(a, n))
  //             }
  //           isMorphism.whereTrue {
  //             [F] => (_ : Dot[F]) ?=> (equalizer: Equalizer[F, ((M, A) > B)]) =>
  //               val action: monoid.Action[F] =
  //                 monoid.Action {
  //                   equalizer.restrict(
  //                     transpose[(F, M), (M, A), B]{ case (f ⊕ m) ⊕ (n ⊕ a) =>
  //                       val phi: CTXT[(M, A) > B] =
  //                         equalizer.inclusion(f)
  //                       eval(phi, monoid.multiply(m, n) ⊕ a)
  //                     }
  //                   )
  //                 }
  //               // awful, fix via refactoring Equalizer via Tagged to provide an implicit =:= rather than inclusion
  //               action.asInstanceOf[monoid.Action[monoid.InternalMap[A, B]]]
  //             }
              
  //         def enumerateMorphisms[B](
  //           analysisB: DefaultActionAnalysis[B]
  //         ): Iterable[A ~> B] =
  //           given Dot[B] = analysisB.action.dot
  //           morphisms[A, B] filter:
  //             (f: A ~> B) => ∀[(A, M)]: 
  //               case a ⊕ m =>
  //                 f(action.actionMultiply(a, m)) =?=
  //                   analysisB.action.actionMultiply(f(a), m)

