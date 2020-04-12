package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSetsUtilities.allMaps
import com.fdilke.bewl.fsets.monoid_actions.{
  ActionSplitter,
  FindGenerators,
  FindPresentation,
  FiniteSetsMonoidAssistant
}
import com.fdilke.bewl.helper.{⊕, Memoize}
import com.fdilke.bewl.helper.⊕._
import com.fdilke.bewl.topos.{Topos, Wrappings}

object FiniteSets
  extends BaseFiniteSets
  with FindGenerators
  with FindPresentation
  with ActionSplitter
  with FiniteSetsMonoidAssistant
  with FiniteSetsImageFinder

class BaseFiniteSets
  extends Topos[Any]
  with Wrappings[
    Any,
    Any,
    Iterable,
    FiniteSetsPreArrow,
    Wrappings.NO_WRAPPER
  ] {
  override val name = "FiniteSets"
  override type DOT[S] = FiniteSetsDot[S]
  override type >[S, T] = FiniteSetsArrow[S, T]
  override type UNIT = Unit
  override type TRUTH = Boolean
  override type →[T <: ~, U <: ~] = Map[T, U] with ~

  override lazy val I = makeDot(Iterable(()))
  override lazy val omega = makeDot(Iterable(true, false))
  override lazy val truth = I(omega)(_ => true)
  override lazy val optionalGenerator = Some(I)

  class FiniteSetsDot[S](
    protected[fsets] val elements: Iterable[S]
  ) extends Dot[S] { outerDot =>
    override lazy val toI = this(I)(_ => ())

    override lazy val globals: Iterable[UNIT > S] =
      elements.map(s => new FiniteSetsArrow(I, this, (_: UNIT) => s))

    override def xUncached[T](that: DOT[T]) =
      new FiniteSetsDot[S x T](
        for {
          s <- this.elements
          t <- that.elements
        } yield s ⊕ t
      ) with BiproductDot[S, T] {
        override val left: DOT[S] = outerDot
        override val right: DOT[T] = that
        override def pair(l: S, r: T): x[S, T] =
          l ⊕ r
      }

    override def `>Uncached`[T](that: FiniteSetsDot[T]) =
      new FiniteSetsDot[S → T](
        allMaps(outerDot.elements, that.elements)
      ) with ExponentialDot[S, T] { exponentialDot =>
        override val source: DOT[S] = outerDot
        override val target: DOT[T] = that

        override def transpose[R](biArrow: BiArrow[R, S, T]) =
          biArrow.product.left(exponentialDot) { r =>
            outerDot.elements.map(s => s -> biArrow(r, s)).toMap
          }

        override def evaluate(
          function: S → T,
          arg: S
        ): T =
          function(arg)
      }

    override def apply[T](target: DOT[T])(f: S => T) =
      new FiniteSetsArrow(this, target, f)

    override def toString =
      s"FiniteSetsDot[$elements]"

    override def sanityTest =
      assert(elements.toSet.size == elements.size)
  }

  class FiniteSetsArrow[S, T](
    val source: FiniteSetsDot[S],
    val target: FiniteSetsDot[T],
    protected[fsets] val function: S => T
  ) extends Arrow[S, T] { self =>
    override def \[U](monic: U > T) =
      source(monic.source) { s =>
        val quarry: T = function(s)
        monic.source.elements.find(u => monic(u) == quarry).getOrElse {
          throw new IllegalArgumentException(s"Cannot backdivide $self by monic $monic")
        }
      }
    override def sanityTest: Unit = {
      source.sanityTest
      target.sanityTest
      for { targetElement <- source.elements.map(function) } if (!target.elements.exists(
                                                                   _ == targetElement
                                                                 )) {
        throw new IllegalArgumentException(
          "Map value " + targetElement + " not in target " + target.elements
        )
      }
    }
    override def ?=(that: S > T) =
      new FiniteSetsDot[S](
        source.elements.filter(s => function(s) == that.function(s))
      ) with EqualizingDot[S] { equalizer =>
        override val equalizerTarget = source
        override def restrict[R](subdot: R > S) =
          subdot.source(this)(subdot(_))
      }

    override def apply(s: S) =
      function(s)

    override def o[R](that: R > S) =
      that.source(target)(function.compose(that.function))

    override lazy val chi =
      target(omega) { t =>
        source.elements.exists {
          this(_) == t
        }
      }

    override def equals(other: Any): Boolean = other match {
      case that: FiniteSetsArrow[S, T] =>
        (source eq that.source) &&
          (target eq that.target) &&
          source.elements.forall(x => function(x) == that.function(x))
      case _ => false
    }
    override def hashCode = 0

    override def toString =
      s"FiniteSetsArrow[$source -> $target : ${source.elements.map(this(_))}]"
  }

  private val memoizedDotWrapper = {
    def wrap[T](elements: Iterable[T]) =
      new FiniteSetsDot(elements)
    Memoize.generic(wrap)
  }

  override def functionAsArrow[S, T](source: DOT[S], target: DOT[T], f: S => T) =
    source(target)(f)

  override def makeArrow[S, T](prearrow: FiniteSetsPreArrow[S, T]) =
    makeDot(prearrow.source)(
      makeDot(prearrow.target)
    )(
      prearrow.function
    )

  override def makeDot[T](predot: Iterable[T]): FiniteSetsDot[T] =
    memoizedDotWrapper(predot)

  // unusually simple generic definition for this topos because WRAPPER is trivial
  override def bifunctionAsBiArrow[L, R, T](
    left: DOT[L],
    right: DOT[R],
    target: DOT[T]
  )(
    bifunc: (L, R) => T
  ): BiArrow[L, R, T] =
    left.x(right).biArrow(target) {
      bifunc
    }
}

case class FiniteSetsPreArrow[S, T](
  source: Iterable[S],
  target: Iterable[T],
  function: S => T
)
