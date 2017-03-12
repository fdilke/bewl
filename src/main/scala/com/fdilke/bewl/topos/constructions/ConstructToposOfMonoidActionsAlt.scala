package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import com.fdilke.bewl.helper.{Memoize, ⊕}

trait ConstructToposOfMonoidActionsAlt extends
  BaseTopos with
  ToposEnrichments {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfMonoidActions {
    def of[
      M <: ~
    ] (
      monoid: Ɛ.Monoid[M]
    ) : Topos[~] =
      new Topos[~] with Wrappings[
    ~,
    ~,
    ({type λ[X <: ~] = monoid.Action[X]}) # λ,
    ({type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y]}) # λ,
    ({type λ[T <: ~] = T}) # λ
  ] {
    import monoid.{ carrier, action, Action }
    
    override type DOT[A <: ~] = ActionDot[A]
    override type >[A <: ~, B <: ~] = ActionArrowFacade[A, B]
    override type UNIT = Ɛ.UNIT
    override type →[T <: ~, U <: ~] = Ɛ.→[T, U]
    
    type IDEAL = M → Ɛ.TRUTH
    override type TRUTH = IDEAL

    override lazy val I = 
      ActionDot(Ɛ.I) {
        (i, m) => i
      }

    private object Ideals {
      private val possibleIdeals = carrier.power
      import possibleIdeals.{ evaluate => $ }

      private val ideals =
        possibleIdeals.whereAll(carrier, carrier) {
          (f, m, n) => 
            Ɛ.OmegaEnrichments($(f, m)) → 
              $(f, monoid.multiply(m, n))
        }

      def restrict[
        H <: ~
      ](
        that: Ɛ.DOT[H]
      )(
        bifunc: (H, M) => Ɛ.TRUTH
      ): Ɛ.>[H, IDEAL] =
        ideals.restrict(
            possibleIdeals.transpose(that)(bifunc)
        )

      private val idealMultiply =
        restrict(ideals x carrier) {
          case (i ⊕ s, t) => 
            $(ideals.inclusion(i),
                monoid.multiply(s, t)
            )
        }

        val omega = 
          ActionDot[IDEAL](
              ideals
          )(
            Ɛ.BiArrow[IDEAL, M, IDEAL](
                ideals x carrier, 
                idealMultiply
            )(_, _)
          )
      }
          
      override lazy val omega = 
        Ideals.omega
        
      override lazy val truth = 
        new ActionArrow[Ɛ.UNIT, IDEAL](
          I,
          omega,
          Ideals.restrict(Ɛ.I) {
            (i, m) => Ɛ truth i
          }
        )

      class ActionDot[
        S <: ~
      ](
        val action: Action[S]
      ) extends Dot[S] { actionDot =>
        override def `>Uncached`[
          T <: ~
        ](
          that: ActionDot[T]
        ): EXPONENTIAL[S,T] = 
          ???
          
        override def apply[
          T <: ~
        ](
            target: ActionDot[T]
        )(
            f: S => T
        ): S > T = 
          new ActionArrow[S, T](
            actionDot,
            target,
            f
          )
          
        override lazy val globals: Traversable[UNIT > S] = {
          val fixedPoints =
            action.actionCarrier.whereAll(
                carrier
            ) {
              (a, m) => 
                action.actionCarrier.=?=(
                  a,
                  action.actionMultiply(a, m)
                )
            }

          fixedPoints.globals.map { global =>
            new ActionArrow(
              I, 
              actionDot, 
              fixedPoints.inclusion o global
            )
          }
        }
          
        override def sanityTest(): Unit = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }
              
        override lazy val toI: S > UNIT = 
          new ActionArrow(
            actionDot, 
            I, 
            action.actionCarrier.toI
          )
          
        override def xUncached[
          T <: ~
        ](
          that: ActionDot[T]
        ): BIPRODUCT[S, T] = {
          val product: Ɛ.BIPRODUCT[S, T] =
            action.actionCarrier x that.action.actionCarrier

          new ActionDot[S x T](
            monoid.action(product) {
              case (s ⊕ t, m) => product.pair(
                action.actionMultiply(s, m),
                that.action.actionMultiply(t, m)
              )
            }
          ) with BiproductDot[S, T, S x T] {
            override val left = actionDot
            override val right = that
            def pair(l: S,r: T) = 
              product.pair(l, r)
          }
        }
      }
      
      trait ActionArrowFacade[ // TODO: do we need this trait? Is there more than one implementation?
        S <: ~,
        T <: ~
      ] extends Arrow[S, T] {
        val arrow: Ɛ.>[S, T]
      }
      
      class ActionArrow[
        S <: ~,
        T <: ~
      ](
        val source: ActionDot[S],
        val target: ActionDot[T],
        override val arrow: Ɛ.>[S, T]
      ) extends ActionArrowFacade[S, T] {
        def this(
          source: ActionDot[S],
          target: ActionDot[T],
          function: S => T
        ) =
          this(
            source,
            target,
            source.action.carrier(
              target.action.carrier
            )(
              function
            )
          )
            
        override def \[
          U <: ~
        ](
          monic: U > T
        ): S > U = 
          new ActionArrow[S, U](
              source,
              monic.source,
              arrow \ monic.arrow
          )
          
        override def ?=(
          that: S > T
        ): EQUALIZER[S] = {
          val equalizer : Ɛ.EQUALIZER[S]= 
            arrow ?= that.arrow
          val equalizerCarrier =
            equalizer x carrier  
          val restrictedMultiply =
            equalizer.restrict(
                equalizerCarrier.biArrow(
                  source.action.carrier
                ) {
                  (s, m) => 
                    source.action.actionMultiply(
                      equalizer.inclusion(s), 
                      m
                    )
                }.arrow
            )
          new ActionDot(
              action(
                  equalizer
              ){ (s, m) =>
                restrictedMultiply(
                  equalizerCarrier.pair(s, m)
                )
              }
          ) with EqualizingDot[S] { equalizingDot =>
            val equalizerTarget = source 
            def restrict[
              R <: ~
            ](
              actionArrow: ActionArrowFacade[R, S]
            ) = 
              new ActionArrow[R, S](
                actionArrow.source,
                equalizingDot,
                equalizer.restrict(
                  actionArrow.arrow
                )
              )
          }
        }
          
        override def apply(
          s: S
        ): T = 
          arrow(s)
          
        override lazy val chi: T > TRUTH = 
          new ActionArrow(
            target,
            omega,
            Ideals.restrict(
              target.action.actionCarrier
            ) {
              (t, m) => arrow.chi(target.action.actionMultiply(t, m))
            }
          )
          
        override def o[
          R <: ~
        ](
          that: R > S
        ): R > T = 
          new ActionArrow[R, T](
            that.source,
            target,
            arrow o that.arrow
          )
          
        override def sanityTest() {
          arrow.sanityTest
          assert(
            monoid.actions.isMorphism(
              source.action, 
              target.action, 
              arrow
            )
          )
        }
      }
      
      object ActionDot {
        def apply[
          A <: ~
        ](
          actionCarrier: Ɛ.DOT[A]
        )(
          actionMultiply: (A, M) => A
        ): ActionDot[
          A
        ] =
          new ActionDot(
            action(
                actionCarrier
            )(
                actionMultiply
            )
          )
      }
          
      override def bifunctionAsBiArrow[
        L <: ~, 
        R <: ~, 
        T <: ~
      ](
        left: ActionDot[L], 
        right: ActionDot[R], 
        target: ActionDot[T]
      )(
        bifunc: (L, R) ⇒ T
      ): BiArrow[L,R,T] =
        ???
        
      override def functionAsArrow[
        S <: ~, 
        T <: ~
      ](
        source: ActionDot[S], 
        target: ActionDot[T], 
        f: S ⇒ T
      ): ActionArrowFacade[S,T] =
        ???
      
      override def makeArrow[
        S <: ~, 
        T <: ~
      ](
        prearrow: monoid.ActionPreArrow[S,T]
      ): ActionArrowFacade[S,T] = ??? 
      
      override def makeDot[T <: ~](
        predot: monoid.Action[T]
      ): ActionDot[T] = 
        ???          
    }
  }
}
