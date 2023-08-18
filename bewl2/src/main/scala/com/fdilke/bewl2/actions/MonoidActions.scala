package com.fdilke.bewl2.actions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.ProductMappable
import com.fdilke.bewl2.{ PreTopos, Topos }
import com.fdilke.bewl2.ProductMappable.⊕

trait MonoidActions[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >]:
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  def toposOfMonoidActions[M: Dot](
    monoid: Monoid[M]
  ): Topos[monoid.Action, CTXT, VOID, UNIT, monoid.RightIdeal, monoid.InternalMap] =
    new Topos[monoid.Action, CTXT, VOID, UNIT, monoid.RightIdeal, monoid.InternalMap](      
      new PreTopos[
        monoid.Action, CTXT, VOID, UNIT, monoid.RightIdeal, monoid.InternalMap
      ]:
        override val unitDot: monoid.Action[UNIT] =
          monoid.withTrivialAction[UNIT, monoid.Action[UNIT]]:
            summon[monoid.Action[UNIT]]

        override val zeroDot: monoid.Action[VOID] =
          monoid.withVoidAction:
            summon[monoid.Action[VOID]]

        override val omegaDot: monoid.Action[monoid.RightIdeal] =
          val evalPredicate: BiArrow[M > BEWL, M, BEWL] =
            Ɛ.evaluation[M, BEWL]
          val respectsAction: (M > BEWL) ~> BEWL =
            Ɛ.∀[M > BEWL, (M, M)]{ (χ, a_m) =>
              a_m match {
                case a ⊕ m =>
                  evalPredicate(χ, a) → 
                    evalPredicate(χ, monoid.multiply(a, m))
              }
            }
          respectsAction.whereTrue[monoid.Action[monoid.RightIdeal]] { 
          [I] => (_: Dot[I]) ?=> (equalizer: Equalizer[I, M > BEWL]) =>

            val leftDivide: (I, M) ~> (M > BEWL) =
              Ɛ.transpose[(I, M), M, BEWL]{
                case (i ⊕ m) ⊕ n =>
                  evalPredicate(
                    equalizer.inclusion(i),
                    monoid.multiply(m, n)
                  )
            }

            val rightIdeals: monoid.Action[I] =
              monoid.Action(equalizer.restrict(leftDivide))
            rightIdeals.asInstanceOf[monoid.Action[monoid.RightIdeal]]
          }

        override val truth: UNIT ~> monoid.RightIdeal =
          val fullRightIdeal: UNIT ~> (M > BEWL) =
            Ɛ.transpose[UNIT, M, BEWL](toTrue[(UNIT, M)])
          fullRightIdeal.asInstanceOf[UNIT ~> monoid.RightIdeal]

        override def equalArrows[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          f1: X ~> Y,
          f2: X ~> Y
        ): Boolean =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.RichArrow(f1) =!= f2

        override def uncachedProductObject[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): monoid.Action[(X, Y)] =
          given Ɛ.Dot[Y] = dotY.dot
          dotX.x(dotY)

        override def uncachedExponentialObject[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): monoid.Action[monoid.InternalMap[X, Y]] =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          val eval: ((M, X) > Y, (M, X)) ~> Y =
            Ɛ.evaluation[(M, X), Y]
          val isMorphism: ((M, X) > Y) ~> BEWL =
            Ɛ.∀[(M, X) > Y, M, X, M] {
              (phi, m, x, n) =>
                dotY.actionMultiply(eval(phi, m ⊕ x), n) =?= 
                  eval(phi, monoid.multiply(m, n) ⊕ dotX.actionMultiply(x, n))
            }
          isMorphism.whereTrue {
            [A] => (_ : Ɛ.Dot[A]) ?=> (equalizer: Ɛ.Equalizer[A, ((M, X) > Y)]) =>
              val action: monoid.Action[A] =
                monoid.Action {
                  equalizer.restrict(
                    Ɛ.transpose[(A, M), (M, X), Y]{ case (a ⊕ m) ⊕ (n ⊕ x) =>
                      val phi: CTXT[(M, X) > Y] =
                        equalizer.inclusion(a)
                      eval(phi, monoid.multiply(m, n) ⊕ x)
                    }
                  )
                }
              action.asInstanceOf[monoid.Action[monoid.InternalMap[X, Y]]]
            }

        override def evaluation[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): (monoid.InternalMap[X, Y], X) ~> Y =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          val eval: ((M, X) > Y, (M, X)) ~> Y =
            Ɛ.evaluation[(M, X), Y]
          val higherEval: ((M, X) > Y, X) ~> Y = {
            case phi ⊕ x =>
              val i: CTXT[M] = globalElement(monoid.unit, x)
              eval(phi, i ⊕ x)
          }
          higherEval.asInstanceOf[(monoid.InternalMap[X, Y], X) ~> Y]

        override def transpose[X, Y, Z](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          dotZ: monoid.Action[Z],
          xy2z: ((X, Y)) ~> Z
        ): X ~> monoid.InternalMap[Y, Z] =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          given Ɛ.Dot[Z] = dotZ.dot
          val pre: (X, (M, Y)) ~> Z = {
            case x ⊕ (m ⊕ y) =>
              xy2z(dotX.actionMultiply(x, m) ⊕ y)
          }
          val xy2zTran: X ~> ((M, Y) > Z) =
            Ɛ.transpose[X, (M, Y), Z](pre)
          xy2zTran.asInstanceOf[X ~> monoid.InternalMap[Y, Z]]

        override def sanityTest[X](
          dotX: monoid.Action[X]
        ): Unit =
          dotX.sanityTest

        override def sanityTest[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          f: X ~> Y
        ): Unit =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          given monoid.Action[X] = dotX
          given monoid.Action[Y] = dotY
          Ɛ.sanityTest[X, Y](f)
          assert { monoid.actions.isMorphism(f) }

        override def enumerateMorphisms[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): Iterable[X ~> Y] =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.morphisms[X, Y] filter { (f: X ~> Y) =>
            Ɛ.∀[(X, M)] { case x ⊕ m =>
              f(dotX.actionMultiply(x, m)) =?=
                dotY.actionMultiply(f(x), m)
            }
          }

        override def fromZero[X](
          dotX: monoid.Action[X]
        ): VOID ~> X =
          given Ɛ.Dot[X] = dotX.dot
          Ɛ.fromZero[X]

        override def toUnit[X](
          dotX: monoid.Action[X]
        ): X ~> UNIT =
          given Ɛ.Dot[X] = dotX.dot
          Ɛ.toUnit[X]

        override def doEqualizer[X, Y, RESULT](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          f: X ~> Y,
          f2: X ~> Y
        )(
          block: [A] => RawEqualizer[A, X] => monoid.Action[A] => RESULT
        ): RESULT =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          Ɛ.doEqualizer(f, f2) {
            [A] => (_: Dot[A]) ?=> (equalizer: Ɛ.Equalizer[A, X]) =>
              block[A](
                new RawEqualizer[A, X]:
                  override val inclusion: A ~> X =
                    equalizer.inclusion
                  override def restrict[R](
                    dotR: monoid.Action[R],
                    arrow: R ~> X
                  ): R ~> A =
                    given Ɛ.Dot[R] = dotR.dot
                    equalizer.restrict[R](arrow)
              )(
                monoid.Action[A](
                  equalizer.restrict {
                    case a ⊕ m =>
                      dotX.actionMultiply(
                        equalizer.inclusion(a),
                        m
                      )
                  }
                )
              )
          }

        override def chiForMonic[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          monic: X ~> Y
        ): Y ~> monoid.RightIdeal =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          val chi: Y ~> (M > BEWL) =
            Ɛ.transpose[Y, M, BEWL]( // define a (Y, M) ~> BEWL
              Ɛ.∃[(Y, M), X]{
                case (y ⊕ m) ⊕ x =>
                  dotY.actionMultiply(y, m) =?= monic(x)
              }
            )
          chi.asInstanceOf[Y ~> monoid.RightIdeal]

        override def backDivideMonic[X, Y, A](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          dotA: monoid.Action[A],
          arrow: X ~> Y,
          monic: A ~> Y
        ): X ~> A =
          given Ɛ.Dot[X] = dotX.dot
          given Ɛ.Dot[Y] = dotY.dot
          given Ɛ.Dot[A] = dotA.dot
          Ɛ.backDivideMonic[X, Y, A](arrow, monic)
    )
  