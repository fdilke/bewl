package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import Wrappings.NO_WRAPPER

trait ConstructToposOfMaskables extends
  BaseTopos with
  ToposEnrichments {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>
    
  class ToposOfMaskables extends Topos[~] 
  /* with Wrappings[
    ~,
    ~,
//      Ɛ.DOT,
//      Ɛ.>,
    ({type λ[X <: ~] = Ɛ.DOT[X]}) # λ,
    ({type λ[X <: ~, Y <: ~] = Ɛ.>[X, Y]}) # λ,
    ({type λ[T <: ~] = T}) # λ
  ] */{
    override type DOT[A <: ~] = MaskableDotFacade[A]
    override type >[A <: ~, B <: ~] = MaskableArrowFacade[A, B]
    override type UNIT = Ɛ.UNIT
    override type TRUTH = Ɛ.TRUTH

    override val I = ???
    
    override val omega = ???
    
    override val truth = ???
    
    trait MaskableDotFacade[
      A <: ~
    ] extends Dot[A]

    trait MaskableArrowFacade[
      S <: ~,
      T <: ~
    ] extends Arrow[S, T] 
    
    class MaskableDot[
      U <: ~,
      A <: ~
    ](
      innerDot: Ɛ.DOT[U],
      to: U =:= A,   
      from: A =:= U   
    ) extends MaskableDotFacade[A] { dot =>

       override def `>Uncached`[
         T <: ~
       ](
           that: MaskableDotFacade[T]
       ): EXPONENTIAL[A,T] = 
         ???
         
       override def apply[
         T <: ~
       ](
         target: MaskableDotFacade[T]
       )(
         f: A => T
       ): MaskableArrowFacade[A,T] = 
         ???
         
       override val globals: Traversable[
         MaskableArrowFacade[UNIT, A]
       ] = 
         ???
         
       override def sanityTest() = 
         ???
         
       override val toI: MaskableArrowFacade[A, UNIT] = 
         ???
         
       override def xUncached[
         T <: ~
       ](
         that: MaskableDotFacade[T]
       ): BIPRODUCT[A,T] = 
         ???
  
      class MaskableArrow[
        T <: ~
      ] (
        override val target: MaskableDotFacade[T]
      ) extends MaskableArrowFacade[A, T] { arrow =>
        override val source = dot
        
        override def \[U <: ~](
          monic: MaskableArrowFacade[U,T]
        ): MaskableArrowFacade[A,U] = ???
        
        override def ?=(
          that: MaskableArrowFacade[A,T]
        ): EQUALIZER[A] = 
          ???
          
        override def apply(
          s: A
        ): T = 
          ???
          
        override val chi: MaskableArrowFacade[T, TRUTH] = 
          ???
          
        override def o[R <: ~](
          that: MaskableArrowFacade[R,A]
        ): MaskableArrowFacade[R,T] = 
          ???
          
        override def sanityTest() = 
          ???
      }
    }
 
//      override def bifunctionAsBiArrow[
//        L <: ~,
//        R <: ~,
//        T <: ~
//      ] (
//        left: Ɛ.DOT[L],
//        right: Ɛ.DOT[R],
//        target: Ɛ.DOT[T]
//      ) (
//        bifunc: (L, R) => T
//      ): BiArrow[L, R, T] =
//        ???
//
//      override def functionAsArrow[
//        S <: ~,
//        T <: ~
//      ](
//        source: Ɛ.DOT[S],
//        target: Ɛ.DOT[T],
//        f: S => T
//      ): S > T
//        
//      override def makeArrow[
//        S <: ~, 
//        T <: ~
//      ](
//        prearrow: S > T
//      ): MaskableArrowFacade[S,T] = 
//        ???
//        
//      override def makeDot[
//        T <: ~
//      ](
//        predot: MaskableDotFacade[T]
//      ): MaskableDotFacade[T] = 
//        ???
  }
}