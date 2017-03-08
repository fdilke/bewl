package com.fdilke.bewl.fsets

import com.fdilke.bewl.helper.{Memoize, ⊕}
import com.fdilke.bewl.topos.{Topos, Wrappings}
import ⊕._
import FiniteSetsUtilities.allMaps

object FiniteSets extends Topos[Any] with Wrappings[
  Any, Any, Traversable, FiniteSetsPreArrow, Wrappings.NO_WRAPPER
] {
  override type DOT[S] = FiniteSetsDot[S]
  override type >[S, T] = FiniteSetsArrow[S, T]
  override type UNIT = Unit
  override type TRUTH = Boolean
  override type →[T <: ~, U <: ~] = (T => U) with ~
  
  override lazy val I = makeDot(Traversable(()))
  override lazy val omega = makeDot(Traversable(true, false))
  override lazy val truth = I(omega) { _ => true }

  class FiniteSetsDot[S](private[FiniteSets] val elements: Traversable[S])
    extends Dot[S] { self =>
    override lazy val toI = this(I) { _ => () }

    override lazy val globals: Traversable[UNIT > S] =
      elements map { s =>
        new FiniteSetsArrow(I, this, (_: UNIT) => s)
      }

    override def xUncached[T](that: DOT[T]) =
      new FiniteSetsDot[S x T](
        for {
          s <- this.elements
          t <- that.elements
        }
          yield s ⊕ t
      ) with BiproductDot[S, T, S x T] {
        override val left: DOT[S] = self
        override val right: DOT[T] = that
        override def pair(l: S, r: T): x[S, T] =
          l ⊕ r
      }

    override def `>Uncached`[T](that: FiniteSetsDot[T]) = {
//      println(s"exponentiating: ${that.elements.size} ^ ${this.elements.size}")
      case class FunctionElement(function: S => T) extends (S => T) {
        override def equals(
          that: scala.Any
        ): Boolean =
          that match {
            case that: FunctionElement => elements.forall {
              s => function(s) == that.function(s)
            }
          }
        override def hashCode = 0
        override def apply(s: S): T =
          function(s)
      }
      val hh: DOT[S → T] with ExponentialDot[S, T, S → T] = ???
      val kk: EXPONENTIAL[S, T] = hh
      ???
      
      new FiniteSetsDot[S → T](
        allMaps(self.elements, that.elements) map FunctionElement
      ) with ExponentialDot[S, T, S → T] { exponentialDot =>
        override val source: DOT[S] = self
        override val target: DOT[T] = that

        override def transpose[R](biArrow: BiArrow[R, S, T]) =
          biArrow.product.left(exponentialDot) {
            r => FunctionElement {
              s => biArrow(r, s)
            }
        }
            
        override def evaluate(
            function: S → T, 
            arg: S
        ): T =
          function(arg)
      }
    }

    override def apply[T](target: DOT[T])(f: S => T) =
      new FiniteSetsArrow(this, target, f)

    override def toString =
      s"FiniteSetsDot[$elements]"

    override def sanityTest =
      for {
        x <- elements
        y <- elements
      }
        x == y
  }

  class FiniteSetsArrow[S, T](
    val source: FiniteSetsDot[S],
    val target: FiniteSetsDot[T],
    private[FiniteSets] val function: S => T
  ) extends Arrow[S, T] { self =>
    override def \[U](monic: U > T) =
      source(monic.source) { s =>
        val quarry: T = function(s)
        monic.source.elements.find { u =>
          monic(u) == quarry
        } getOrElse {
          throw new IllegalArgumentException(s"Cannot backdivide $self by monic $monic")
      }}
    override def sanityTest =
      if (!source.elements.map(function).forall(x => target.elements.exists(_ == x))) {
        throw new IllegalArgumentException("Map values not in target")
      }
    override def ?=(that: S > T) =
      new FiniteSetsDot[S] (
        source.elements.filter { s => function(s) == that.function(s) }
      ) with EqualizingDot[S] { equalizer =>
        override val equalizerTarget = source
        override def restrict[R](subdot: R > S) =
          subdot.source(this) { subdot(_) }
      }

    override def apply(s: S) =
      function(s)

    override def o[R](that: R > S) =
      that.source(target)(function compose that.function)

    override lazy val chi =
      target(omega) { t =>
        source.elements.exists {
          this(_) == t
      }}

    override def equals(other: Any): Boolean = other match {
      case that: FiniteSetsArrow[S, T] =>
        (source eq that.source) &&
          (target eq that.target) &&
          source.elements.forall { x => 
            function(x) == that.function(x)
          }
      case _ => false
    }
    override def hashCode = 0

    override def toString =
      s"FiniteSetsArrow[$source -> $target : ${
        source.elements map { this(_) }
      }]"
  }

  private val memoizedDotWrapper = {
    def wrap[T](elements: Traversable[T]) =
      new FiniteSetsDot(elements)
    Memoize generic wrap
  }

  override def functionAsArrow[S, T](source: DOT[S], target: DOT[T], f: S => T) =
    source(target)(f)

  override def makeArrow[S, T](prearrow: FiniteSetsPreArrow[S, T]) =
    makeDot(prearrow.source)(makeDot(prearrow.target))(prearrow.function)

  override def makeDot[T](predot: Traversable[T]) =
    memoizedDotWrapper(predot)

  // unusually simple generic definition for this topos because WRAPPER is trivial
  override def bifunctionAsBiArrow[L, R, T](
    left: DOT[L],
    right: DOT[R],
    target: DOT[T]
  ) (
    bifunc: (L, R) => T
  ): BiArrow[L, R, T] =
    (left x right).biArrow(target) { bifunc }
}

case class FiniteSetsPreArrow[S, T](
  source: Traversable[S],
  target: Traversable[T],
  function: S => T
)

object FiniteSetsUtilities {
  import FiniteSets._

  def dot[T](elements: T*) =
    makeDot(elements)

  def arrow[S, T](
    source: DOT[S],
    target: DOT[T]
  ) (
    map: (S, T)*
  ) =
    functionAsArrow(
      source,
      target,
      Map(map: _*)
    )

  def biArrow[L, R, T](
   left: DOT[L],
   right: DOT[R],
   target: DOT[T]
  )(
   mappings: ((L, R), T)*
 ) =
    bifunctionAsBiArrow[L, R, T](
      left,
      right,
      target
    )(
      Function untupled Map(
        mappings:_*
      )
    )

  def elementsOf[X](
    dot: DOT[X]
  ): Traversable[X] =
    dot.globals map {
      _(())
    }

  def asElement[X, Y](
    arrow: X > Y
  ): X → Y = {
    val xx: UNIT > (X → Y) = arrow.name
    arrow.name(())
  }

  def makeNullaryOperator[X](
    carrier: DOT[X],
    value: X
  ) =
    functionAsArrow(
      I,
      carrier,
      (_: UNIT) => value
    )

  def makeUnaryOperator[X](
    carrier: DOT[X],
    mappings: (X, X)*
  ) =
    functionAsArrow(
      carrier,
      carrier,
      Map(mappings:_*)
    )

  def makeBinaryOperator[X](
    carrier: DOT[X],
    mappings: ((X, X), X)*
  ) =
    bifunctionAsBiArrow[X](
      carrier
    ) (
      Function untupled Map(
        mappings:_*
      )
    )

  def doubleCharacteristic[T](
    set: DOT[T]
  )(
    subsets: Set[T]*
  ): (T → TRUTH) > TRUTH = {
    val incl =
      bifunctionAsBiArrow(
        dot(subsets :_*),
        set,
        omega
      ) {
        _ contains _
      }
    (set.power transpose incl).chi
  }

  private def intSqrt(square: Int) =
    (1 to square).find { n =>
      n * n == square
    }.getOrElse {
      throw new IllegalArgumentException("Not a valid monoid multiplication table: size " + square)
    }

  def monoidFromTable[M](table: M*): Monoid[M] = {
    val carrierSize = intSqrt(table.size)
    val carrierAsList = table.take(carrierSize)
    val carrier = dot(carrierAsList :_*)
    val mappings =
      for {
        i <- 0 until carrierSize
        j <- 0 until carrierSize
      } yield (
        carrierAsList(i),
        carrierAsList(j)
      ) -> table(
        i * carrierSize + j
      )
    val product =
      makeBinaryOperator(
        carrier,
        mappings:_ *
      )
    new Monoid[M](
      carrier,
      makeNullaryOperator(
        carrier,
        carrierAsList.head
      ),
      product
    )
  }

  def allMaps[A, B](
    source: Traversable[A],
    target: Traversable[B]
  ) : Traversable[A => B] =
    new Traversable[A => B] {
      override def foreach[U](
        enumerate: (A => B) => U
      ) =
        if (source.isEmpty)
          enumerate { _ => ??? }
        else
          for {
            f <- allMaps(source.tail, target)
            choice <- target
          } {
            enumerate { x =>
              if (x == source.head)
                choice
              else
                f(x)
            }
          }
    }
}