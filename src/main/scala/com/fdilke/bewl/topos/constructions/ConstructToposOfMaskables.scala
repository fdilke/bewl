package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.↔
import com.fdilke.bewl.helper.{ ↔ => equivalence }
import com.fdilke.bewl.helper.{Memoize, ⊕}
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import Wrappings.NO_WRAPPER
import ⊕._

trait ConstructToposOfMaskables extends
  BaseTopos with
  ToposEnrichments {
  
   Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  case class MaskablePreDot[
    U <: ~,
    A <: ~
  ](
    innerDot: DOT[U],
    ↔ : U ↔ A   
  )
  
  object MaskablePreDot {
    def apply[
      A <: ~
    ](
      innerDot: DOT[A]
    ): MaskablePreDot[A, A] =
      MaskablePreDot(
        innerDot,
        ↔.identity[A]
      )
  }
  
  type MASKED_PREDOT[A <: ~] = 
      Ɛ.MaskablePreDot[_ <: ~, A]

  case class MaskablePreArrow[
    U <: ~,
    V <: ~,
    A <: ~,
    B <: ~
  ](
    innerArrow: U > V,
    `↔1` : U ↔ A,   
    `↔2` : V ↔ B   
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
        ↔.identity[A],
        ↔.identity[B]
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
    }

    trait MaskableArrowFacade[
      S <: ~,
      T <: ~
    ] extends Arrow[S, T] 
    
    class MaskableDot[
      U <: ~,
      A <: ~
    ](
      val innerDot: Ɛ.DOT[U],
      val ↔ : U ↔ A   
    ) extends MaskableDotFacade[A] { dot =>

       override def `>Uncached`[
         T <: ~
       ](
         that: MaskableDotFacade[T]
       ): EXPONENTIAL[A,T] = 
         ???
         
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
             source.innerDot(innerDot) { t =>
               ↔ \ f(source.↔ / t)
             }
           new MaskableArrow[T, U, Z, A](
             newArrow,
             source.↔,
             ↔
           )
         }
         
       override val globals: Traversable[
         MaskableArrowFacade[UNIT, A]
       ] = 
         innerDot.globals map { 
           cachedArrow(_, equivalence.identity[UNIT], ↔)
         }
         
       override def sanityTest() = 
         ???
         
       override lazy val toI: MaskableArrowFacade[A, UNIT] = 
         cachedArrow(
           innerDot.toI,
           ↔,
           equivalence.identity[UNIT]
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
           left: MaskableDot[T, Z]
       ): BIPRODUCT[Z, A] = {
//         val innerProductDot = left.innerDot x innerDot
//         val tepee = 
//           new ↔[T x U, Z x A] (
//              _ match { case t ⊕ u => (left.↔ / t) ⊕ (↔ / u) },
//              ???
//            )
//
//        new MaskableDot[T x U, Z x A](
//            innerProductDot,
//            new ↔[T x U, Z x A] {
//              _ match { case t ⊕ u => (left.↔ / t) ⊕ (↔ / u) },
//              ???
//            }
//        ) with BiproductDot[
//          Z,
//          A,
//          Z x A
//        ]
         ???
       }
    }

    class MaskableArrow[
      A <: ~,
      B <: ~,
      S <: ~,
      T <: ~
    ] (
      innerArrow: Ɛ.>[A, B],
      `↔1` : A ↔ S,   
      `↔2` : B ↔ T   
    ) extends MaskableArrowFacade[S, T] { arrow =>
      override lazy val source = 
        cachedDot(
          innerArrow.source, 
          `↔1`
        )
      override lazy val target = 
        cachedDot(
          innerArrow.target, 
          `↔2`
        )
        
      override def \[U <: ~](
        monic: MaskableArrowFacade[U,T]
      ): MaskableArrowFacade[S,U] = ???
      
      override def ?=(
        that: MaskableArrowFacade[S, T]
      ): EQUALIZER[S] = 
        ???
        
      override def apply(
        s: S
      ): T = 
        ???
        
      override lazy val chi: MaskableArrowFacade[T, TRUTH] = 
        cachedArrow(
          innerArrow.chi,
          `↔2`,
          equivalence.identity[TRUTH]
        )
        
      override def o[R <: ~](
        that: MaskableArrowFacade[R, S]
      ): MaskableArrowFacade[R,T] = 
        ???
        
      override def sanityTest() = 
        ???
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
        preArrow.`↔1`,
        preArrow.`↔2`
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
      predot: Ɛ.MaskablePreDot[U, T]
    ) =
      new MaskableDot[U, T](
        predot.innerDot,
        predot.↔
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
    innerDot: Ɛ.DOT[T],
    ↔ : T ↔ U  
  ): DOT[U] =
    makeDot(
         Ɛ.MaskablePreDot(
       innerDot, 
       ↔
      )
    )

  private def cachedDot[
    T <: ~
  ](
    innerDot: Ɛ.DOT[T] 
  ) =
    makeDot(
          Ɛ.MaskablePreDot(
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
    `↔1` : S ↔ U,  
    `↔2` : T ↔ V  
  ): U > V =
    makeArrow(
        Ɛ.MaskablePreArrow(
      innerArrow,
      `↔1`,
      `↔2`
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
