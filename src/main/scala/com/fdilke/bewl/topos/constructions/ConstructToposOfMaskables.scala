package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.{Memoize, ⊕}
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import Wrappings.NO_WRAPPER
import ⊕._

trait ConstructToposOfMaskables extends
  BaseTopos with
  ToposEnrichments {
  
   Ɛ: AlgebraicStructures with AlgebraicMachinery =>

   case class Mask[
     A <: ~,
     B <: ~
   ](
		 / : A > B, 
     \ : B > A 
   ) {
     def sanityTest() { 
    	 /.sanityTest
       \.sanityTest
     }
   }

   object Mask {
     def apply[
       A <: ~
     ] (
       dot: DOT[A]
     ) =
       new Mask(
         dot.identity, 
         dot.identity
       )
   }
   
  type MASKED_PREDOT[A <: ~] = 
      Mask[_ <: ~, A]

  case class MaskablePreArrow[
    U <: ~,
    V <: ~,
    A <: ~,
    B <: ~
  ](
    innerArrow: U > V,
    `⇄1` : Mask[U, A],   
    `⇄2` : Mask[V, B]   
  )

  object MaskablePreArrow {
    def apply[
      A <: ~,
      B <: ~
    ](
      innerArrow: A > B
    ): MaskablePreArrow[A, B, A, B] =
      MaskablePreArrow(
        innerArrow,
        Mask(innerArrow.source),
        Mask(innerArrow.target)
      )
  }
  
  type MASKED_PREARROW[
    A <: ~, 
    B <: ~
  ] = 
      Ɛ.MaskablePreArrow[
      _ <: ~, 
      _ <: ~, 
      A, 
      B
    ]
  
  object Maskables extends Topos[
    ~
  ] with Wrappings[
    ~,
    ~,
    MASKED_PREDOT,
    MASKED_PREARROW,
    ({type λ[T <: ~] = T}) # λ
  ] {
    import Ɛ.{ Mask => ⇄ }
//    class ⇄[
//      U <: ~,
//      A <: ~
//    ](
//    		/: Ɛ.>[U, A],
//        \: Ɛ.>[A, U]
//    ) extends Ɛ.Mask[U, A](/,\) {
//      def >[
//        T <: ~,
//        Z <: ~
//      ]: ⇄[T → U, Z → A] = {
//      val to: Ɛ.>[T → U, Z → U] =
//      			 innerSource > pre.⇄.\
//      val to2: Ɛ.>[Z → U, Z → A] =
//             ⇄./ > pre.innerTarget
//      }
//    }

    override type DOT[A <: ~] = MaskableDotFacade[A]
    override type >[A <: ~, B <: ~] = MaskableArrowFacade[A, B]
    
    override type UNIT = Ɛ.UNIT
    override type TRUTH = Ɛ.TRUTH

    override lazy val I = 
      cachedDot(Ɛ.I)
    
    override lazy val omega = 
      cachedDot(Ɛ.omega)
    
    override lazy val truth = 
      cachedArrow(Ɛ.truth)
    
    trait MaskableDotFacade[
      A <: ~
    ] extends Dot[A] {
      def preApply[
        T <: ~,
        Z <: ~
      ](
         source: MaskableDot[T, Z],
         f: Z => A
      ): MaskableArrowFacade[Z, A]
      
      def pre_xUncached[
        T <: ~,
        Z <: ~
      ] (
        left: MaskableDot[T, Z]
      ): BIPRODUCT[Z, A]
      
      def preExponential[
        T <: ~,
        Z <: ~
      ] (
        pre: MaskableDot[T, Z]
      ): EXPONENTIAL[Z, A]
      
      def preTranspose[
        T <: ~,
        Z <: ~,
        V <: ~,
        B <: ~
      ] (
        pre: MaskableDot[T, Z],
        post: MaskableDot[V, B],
        exponential: Ɛ.EXPONENTIAL[T, V],
        expEquiv: ⇄[T → V, Z → B],
        biarrow: BiArrow[A, Z, B]
      ): MaskableArrowFacade[A, Z → B] 
    }

    trait MaskableArrowFacade[
      S <: ~,
      T <: ~
    ] extends Arrow[S, T] {
      def preCompose[
        P <: ~, 
        Q <: ~,
        U <: ~
      ] (
        left: MaskableArrow[P, Q, T, U] 
      ): MaskableArrowFacade[S, U]

      def preEqualize[
        P <: ~,
        Q <: ~
      ] (
        pre: MaskableArrow[P, Q, S, T]
      ): EQUALIZER[S] 

      def preRestrict[
        P <: ~,
        Q <: ~,
        R <: ~,
        X <: ~,
        Y <: ~
      ](
        pre: MaskableArrow[P, Q, T, R],
        alt: MaskableArrow[X, Y, T, R],
        innerEqualizer: Ɛ.EQUALIZER[X]
      ): MaskableArrowFacade[S, T]
    }
    
    class MaskableDot[
      U <: ~,
      A <: ~
    ](
      val ⇄ : ⇄[U, A]   
    ) extends MaskableDotFacade[A] { dot =>
       lazy val innerSource: Ɛ.DOT[U] =
          ⇄./.source
       lazy val innerTarget: Ɛ.DOT[A] =
          ⇄./.target

       override def `>Uncached`[
         T <: ~
       ](
         that: MaskableDotFacade[T]
       ): EXPONENTIAL[A,T] =
         that.preExponential(
           dot
         )

       def exponentialEquivalence[
         T <: ~,
         Z <: ~
       ] (
         pre: MaskableDot[T, Z]
       ) =
         new ⇄[T → U, Z → A](
      		 (⇄./ > pre.innerTarget) o (innerSource > pre.⇄.\),
           (innerSource > pre.⇄./) o (⇄.\ > pre.innerTarget)
         )
         
       override def preExponential[
         T <: ~,
         Z <: ~
       ](
         pre: MaskableDot[T, Z]
       ): EXPONENTIAL[Z, A] = {
        val expEquiv =
          exponentialEquivalence(pre)
        val underlyingExponential =
          pre.innerSource > innerSource
        new MaskableDot[
           T → U,
           Z → A
         ](
           expEquiv
         ) with ExponentialDot[Z, A, Z → A] {
           override val source = pre 
           override val target = dot 
           override def transpose[R <: ~](
             biArrow: BiArrow[R, Z, A]
           ): MaskableArrowFacade[R, Z → A] =
             biArrow.product.left.preTranspose[T, Z, U, A](
                 pre,
                 dot,
                 underlyingExponential,
                 expEquiv,
                 biArrow
             )
         }
      }
       
      override def preTranspose[
        T <: ~,
        Z <: ~,
        V <: ~,
        B <: ~
      ] (
        pre: MaskableDot[T, Z],
        post: MaskableDot[V, B],
        exponential: Ɛ.EXPONENTIAL[T, V],
        expEquiv: ⇄[T → V, Z → B],
        biArrow: BiArrow[A, Z, B]
      ): MaskableArrowFacade[A, Z → B] = 
        new MaskableArrow[U, T → V, A, Z → B](
          exponential.transpose(
            innerSource
          ){
            (u, t) => post.⇄.\(
              biArrow(
                  ⇄./(u),
                  pre.⇄./(t)
              )
            )
          },
          ⇄,
          expEquiv
        )

        override def apply[
         B <: ~
       ](
         target: MaskableDotFacade[B]
       )(
         f: A => B
       ): MaskableArrowFacade[A, B] = 
         target.preApply(dot, f)
         
       override def preApply[
         T <: ~,
         Z <: ~
       ](
         source: MaskableDot[T, Z],
         f: Z => A
       ): MaskableArrowFacade[Z, A] = {
           val newArrow: Ɛ.>[T, U] = 
             source.innerSource(innerSource) { t =>
               ⇄ \ f(source.⇄ / t)
             }
           new MaskableArrow[T, U, Z, A](
             newArrow,
             source.⇄,
             ⇄,
             Some(source),
             Some(dot)
           )
         }
         
       override val globals: Traversable[
         MaskableArrowFacade[UNIT, A]
       ] = 
         innerSource.globals map { 
           cachedArrow(_, Ɛ.Mask(Ɛ.I), ⇄)
         }
         
       override def sanityTest() = 
         ⇄.sanityTest
         
       override lazy val toI: MaskableArrowFacade[A, UNIT] = 
         cachedArrow(
           innerSource.toI,
           ⇄,
           Ɛ.Mask(Ɛ.I)
         )
         
       override def xUncached[
         T <: ~
       ](
         that: MaskableDotFacade[T]
       ): BIPRODUCT[A,T] =
         that.pre_xUncached(dot)
         
       override def pre_xUncached[
         T <: ~,
         Z <: ~
       ] (
           leftDot: MaskableDot[T, Z]
       ): BIPRODUCT[Z, A] = {
         val sourceProduct = leftDot.innerSource x innerSource
         val targetProduct = leftDot.innerTarget x innerTarget

         new MaskableDot[T x U, Z x A](
           new ⇄[T x U, Z x A] (
               sourceProduct(targetProduct) {
                 case t ⊕ u => 
                   targetProduct.pair(
                     leftDot.⇄ / t,
                     ⇄ / u
                   )
               },
               targetProduct(sourceProduct) {
                 case z ⊕ a => 
                   sourceProduct.pair(
                     leftDot.⇄ \ z,
                     ⇄ \ a
                   )
               }
            )
        ) with BiproductDot[
          Z,
          A,
          Z x A
        ] {
           override val left = leftDot
           override val right = dot        
           override def pair(z: Z, a: A) = 
             targetProduct.pair(z, a) 
         }
       }
    }

    class MaskableArrow[
      A <: ~,
      B <: ~,
      S <: ~,
      T <: ~
    ] (
      val innerArrow: Ɛ.>[A, B],
      val `⇄1` : A ⇄ S,   
      val `⇄2` : B ⇄ T,
      val defaultSource: Option[MaskableDot[A, S]] = None,
      val defaultTarget: Option[MaskableDot[B, T]] = None
    ) extends MaskableArrowFacade[S, T] { arrow =>
      private val applyArrow = 
        `⇄2`./ o innerArrow o `⇄1`.\

      override lazy val source = 
        defaultSource.getOrElse(
          cachedDot(
            `⇄1`
          )
      )
      
      override lazy val target = 
        defaultTarget.getOrElse(
          cachedDot(
            `⇄2`
          )
      )
      
      override def \[U <: ~](
        monic: MaskableArrowFacade[U,T]
      ): MaskableArrowFacade[S,U] = ???
      
      override def ?=(
        that: MaskableArrowFacade[S, T]
      ): EQUALIZER[S] = 
        that.preEqualize(this)
        
      override def preEqualize[
        P <: ~,
        Q <: ~
      ] (
        pre: MaskableArrow[P, Q, S, T]
      ): EQUALIZER[S] = 
        new MaskableDot[A, S](
            `⇄1`
        ) with EqualizingDot[S] {
          override val equalizerTarget = source
          
          private val parallelInner: Ɛ.>[A, B] =
            (`⇄2`.\) o (pre.`⇄2`./) o 
              pre.innerArrow o 
              (pre.`⇄1`.\) o (`⇄1`./) 

          private val innerEqualizer: Ɛ.EQUALIZER[A] =
            innerArrow ?= parallelInner
            
          override def restrict[
            R <: ~
          ](
            parallelArrow: MaskableArrowFacade[R, S]
          ): MaskableArrowFacade[R, S] = 
            parallelArrow.preRestrict[P, Q, T, A, B](
              pre,
              arrow,
              innerEqualizer
            )
        }
      
      override def preRestrict[
        P <: ~,
        Q <: ~,
        R <: ~,
        X <: ~,
        Y <: ~
      ](
        pre: MaskableArrow[P, Q, T, R],
        alt: MaskableArrow[X, Y, T, R],
        innerEqualizer: Ɛ.EQUALIZER[X]
      ): MaskableArrowFacade[S, T] =
        ???
        
      override def apply(
        s: S
      ): T =
        applyArrow(s)
        
      override lazy val chi: MaskableArrowFacade[T, TRUTH] = 
        cachedArrow(
          innerArrow.chi,
          `⇄2`,
          Ɛ.Mask(Ɛ.omega)
        )
        
      override def o[R <: ~](
        that: MaskableArrowFacade[R, S]
      ): MaskableArrowFacade[R,T] = 
        that.preCompose(
          arrow
        )
        
      override def preCompose[
        P <: ~, 
        Q <: ~,
        U <: ~
      ] (
        left: MaskableArrow[P, Q, T, U] 
      ) =
        new MaskableArrow[A, Q, S, U](
            left.innerArrow o left.`⇄1`.\ o `⇄2`./ o innerArrow,
            `⇄1`,
            left.`⇄2`
        )
        
      override def sanityTest() {
        innerArrow.sanityTest()
        `⇄1`.sanityTest()
        `⇄2`.sanityTest()
      }
        
      override def equals(other: Any): Boolean = 
          other match {
            case that: MaskableArrow[S, T, A, B] =>
              (that.source eq source) &&
                (that.target eq target) &&
                (that.innerArrow == innerArrow)
            case _ => false
          }
        
      override def hashCode = 0
    }
    
    // unusually simple generic definition for this topos because WRAPPER is trivial
    override def bifunctionAsBiArrow[
      L <: ~,
      R <: ~,
      T <: ~
    ] (
      left: DOT[L],
      right: DOT[R],
      target: DOT[T]
    ) (
      bifunc: (L, R) => T
    ): BiArrow[L, R, T] =
      (left x right).biArrow(target) { bifunc }
  
    override def functionAsArrow[
      S <: ~,
      T <: ~
    ](
      source: DOT[S],
      target: DOT[T],
      f: S => T
    ): S > T =
      ???

    private def subMakeArrow[
      S <: ~, 
      T <: ~,
      A <: ~, 
      B <: ~
    ](
      preArrow: Ɛ.MaskablePreArrow[A, B, S, T]
    ): >[S,T] = 
      new MaskableArrow(
        preArrow.innerArrow,
        preArrow.`⇄1`,
        preArrow.`⇄2`
      )
      
    override def makeArrow[
      S <: ~, 
      T <: ~
    ](
      preArrow: Ɛ.MASKED_PREARROW[S, T]
    ): >[S,T] = 
      subMakeArrow(preArrow)

  private val memoizedDotWrapper = {
    def subwrap[U <: ~, T <: ~](
      predot: U ⇄ T
    ) =
      new MaskableDot[U, T](
        predot
      )

    def wrap[T <: ~](
      predot: Ɛ.MASKED_PREDOT[T]
    ) : MaskableDotFacade[T] =
      subwrap(predot)
      
    Memoize.generic.withLowerBound[
        Ɛ.MASKED_PREDOT,
      MaskableDotFacade,
      ~
    ](wrap)
  }
      
  override def makeDot[
    T <: ~
  ](
    predot: Ɛ.MASKED_PREDOT[T]
  ): DOT[T] = 
    memoizedDotWrapper(
      predot
    )
  
  private def cachedDot[
    T <: ~,
    U <: ~
  ](
    ⇄ : T ⇄ U  
  ): DOT[U] =
    makeDot(
       ⇄
    )

  private def cachedDot[
    T <: ~
  ](
    innerDot: Ɛ.DOT[T] 
  ) =
    makeDot(
      Ɛ.Mask(
        innerDot 
      )
    )
  
  private def cachedArrow[
    S <: ~,
    T <: ~,
    U <: ~,
    V <: ~
  ](
    innerArrow: Ɛ.>[S, T],
    `⇄1` : S ⇄ U,  
    `⇄2` : T ⇄ V  
  ): U > V =
    makeArrow(
        Ɛ.MaskablePreArrow(
      innerArrow,
      `⇄1`,
      `⇄2`
     )
    )
    
  private def cachedArrow[
    S <: ~,
    T <: ~
  ](
    innerArrow: Ɛ.>[S, T]
  ): S > T =
    makeArrow(
        Ɛ.MaskablePreArrow(
        innerArrow
     )
    )
  }
}
