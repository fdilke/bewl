package com.fdilke.bewl2.topos

import com.fdilke.bewl.helper.Memoize

trait Topos[DOT[_]] { topos =>
  val name: String = getClass.getSimpleName

  def sanityTest[S: DOT]: Unit
  def sanityTest[S: DOT, T:DOT](arrow: S => T): Unit

  def compareFunctions[S:DOT, T:DOT](func: S=> T, func2: S => T): Boolean
  def functionAsString[S: DOT, T: DOT](arrow: S => T): String
  def productUncached[A : DOT, B : DOT]: DOT[(A, B)]

  trait Equalizer[S, R] {
    def include(r: R):S
    def restrict[Q: DOT](
      arrow: Q => S
    ): Q => R
  }

  abstract class EqualizerReceiver[S: DOT, X] {
    def apply[R <: S : DOT](
      equalizer: Equalizer[S, R]
    ): X
  }

  def equalize[S:DOT, T:DOT, X](
    func1: S => T,
    func2: S => T
  ): EqualizerReceiver[S, X] => X

  // TODO: put this helper code in a separate class

  @inline implicit class RichFunction[S: DOT, T:DOT] (
    function: S => T
  ) {
    @inline def =?=(function2: S => T): Boolean =
      compareFunctions(function, function2)

    @inline def ?=[X](function2: S => T): EqualizerReceiver[S, X] => X =
      equalize(function, function2)

    @inline def o[R: DOT](function2: R => S): R => T =
      function compose function2

    @inline def x[U: DOT](function2: S => U): S => (T, U) =
      s => (function(s), function2(s))

    @inline def sanityTest =
      topos.sanityTest(function)
  }

  // anticipate these will not be used very much...
  // as it's all baked into the types and thoughtcrime is impossible. remove?
  @inline final def dot[S:DOT]: DOT[S] =
    implicitly[DOT[S]]

  final def source[S:DOT, T:DOT](arrow: S => T): DOT[S] =
    dot[S]
  final def target[S:DOT, T:DOT](arrow: S => T): DOT[T] =
    dot[T]
  final def id[S: DOT]: S => S =
    identity

  final private class DotExtras[A: DOT] {
    final private def makeProduct[B](
      dot: DOT[B]
    ): DOT[(A, B)] = {
      implicit val theDot: DOT[B] = dot
      productUncached[A, B]
    }

    final private val memoizedProduct =
      Memoize.generic[
        DOT,
        ({ type λ[B] = DOT[(A, B)]}) # λ,
      ] (makeProduct)

    def productDot[B : DOT]: DOT[(A, B)] =
      memoizedProduct(dot[B])
  }

  final private def makeDotExtras[A](
    dot: DOT[A]
  ): DotExtras[A] = {
    implicit val theDot: DOT[A] = dot
    new DotExtras[A]
  }

  final private val memoizedExtras =
    Memoize.generic[
      DOT,
      DotExtras
    ] (makeDotExtras)

  final private def extras[A: DOT]: DotExtras[A] =
    memoizedExtras(dot[A])

  implicit def productDot[A: DOT, B: DOT]: DOT[(A, B)] =
    extras[A].productDot[B]

  implicit def multiFunction2function[A: DOT, B: DOT, C: DOT](
    mf: (A, B) => C
  ): ((A, B)) => C =
    Function.tupled(mf)
}
