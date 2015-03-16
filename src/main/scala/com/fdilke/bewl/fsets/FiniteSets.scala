package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSetsUtilities._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.{Wrappings, Topos}

object FiniteSets extends Topos with Wrappings[Any, Traversable, FiniteSetsPreQuiver] {
  override type ~ = Any
  override type DOT[S <: ~] = FiniteSetsDot[S]
  override type ARROW[S <: ~, T <: ~] = FiniteSetsArrow[S, T]
  override type UNIT = Unit
  override type TRUTH = Boolean
  override lazy val I = star(Traversable(()))
  override lazy val omega = star(Traversable(true, false))
  override lazy val truth = I(omega) { _ => true }
  override type >[T <: ~, U <: ~] = (T => U) with ~
  override type x[T <: ~, U <: ~] = (T, U) with ~

  class FiniteSetsDot[S](private[FiniteSets] val elements: Traversable[S])
    extends Dot[S] { self =>
    override lazy val toI = this(I) { _ => () }

    override lazy val globals: Traversable[ARROW[UNIT, S]] =
      elements map { s =>
        new FiniteSetsArrow(I, this, (_: UNIT) => s)
      }

    override def `>Uncached`[T <: ~](that: FiniteSetsDot[T]) = {
      // println(s"exponentiating: ${that.elements.size} ^ ${this.elements.size}")
      case class FunctionElement(function: S => T) extends (S => T) {
        override def equals(that: scala.Any): Boolean = that match {
          case that: FunctionElement => elements.forall {
            s => function(s) == that.function(s)
          }}
        override def hashCode = 0
        def apply(s: S): T = function(s)
      }
      new FiniteSetsDot[S > T](
        allMaps(self.elements, that.elements).map { FunctionElement } // TODO: coalesce
      ) with ExponentialDot[S, T, S > T] { exponentialStar =>
        override val source: DOT[S] = self
        override val target: DOT[T] = that

        override def transpose[R <: ~](biQuiver: BiQuiver[R, S, T]) =
          biQuiver.product.left(exponentialStar) {
            r => FunctionElement {
              s => biQuiver(r, s)
            }}}}

    override def xUncached[T <: ~](that: DOT[T]) =
      new FiniteSetsDot[S x T](
        for(s <- this.elements ; t <- that.elements)
        yield (s, t)
      ) with BiproductDot[S, T, S x T] {
        override val left: DOT[S] = self
        override val right: DOT[T] = that
        override def pair(l: S, r: T): x[S, T] = (l, r)
      }

    override def apply[T <: ~](target: DOT[T])(f: S => T) =
      new FiniteSetsArrow(this, target, f)

    override def toString =
      s"FiniteSetsDot[$elements]"

    override def sanityTest =
      for (x <- elements ; y <- elements)
        x == y
  }

  class FiniteSetsArrow[S, T](
    val source: FiniteSetsDot[S],
    val target: FiniteSetsDot[T],
    private[FiniteSets] val function: S => T
  ) extends Arrow[S, T] { self =>
    override def \[U <: ~](monic: ARROW[U, T]) =
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
    override def ?=(that: ARROW[S, T]) =
      new FiniteSetsDot[S] (
        source.elements.filter { s => function(s) == that.function(s) }
      ) with EqualizingDot[S] { equalizer =>
        override val equalizerTarget = source
        override def restrict[R](substar: ARROW[R, S]) =
          substar.source(this) { substar(_) }
        override val inclusion: ARROW[S, S] = equalizer(source) { s => s }
      }

    override def apply(s: S) =
      function(s)

    override def o[R <: ~](that: ARROW[R, S]) =
      that.source(target)(function compose that.function)

    override lazy val chi =
      target(omega) { t =>
        source.elements.exists { s =>
          this(s) == t
      }}

    override def equals(other: Any): Boolean = other match {
      case that: FiniteSetsArrow[S, T] =>
        source == that.source && target == that.target &&
          source.elements.forall(x => function(x) == that.function(x))
      case _ => false
    }
    override def hashCode = 0

    override def toString =
      s"FiniteSetsArrow[$source -> $target : ${
        for(s <- source.elements)
          yield s -> this(s)
      }]"
  }

  private val memoizedStarWrapper = {
    def wrap[T](elements: Traversable[T]) =
      new FiniteSetsDot(elements)
    Memoize.generic(wrap)
  }

  // wrapping API: TODO build this comment into the structure

  override type WRAPPER[T] = T

  override def functionAsQuiver[S, T](source: DOT[S], target: DOT[T], f: S => T) =
    source(target)(f)

  override def quiver[S, T](prequiver: FiniteSetsPreQuiver[S, T]) =
    star(prequiver.source)(star(prequiver.target))(prequiver.function)

  override def star[T](input: Traversable[T]) =
    memoizedStarWrapper(input)

  // unusually simple generic definition for this topos because WRAPPER is trivial
  override def bifunctionAsBiQuiver[L, R, T](
    left: DOT[L],
    right: DOT[R],
    target: DOT[T]
  ) (
    bifunc: (L, R) => T
  ): BiQuiver[L, R, T] =
    (left x right).biQuiver(target) { bifunc }
}

case class FiniteSetsPreQuiver[S, T](
  source: Traversable[S],
  target: Traversable[T],
  function: S => T
)

object FiniteSetsUtilities {
  import FiniteSets._

  def makeStar[T](elements: T*) = star(elements)

  def makeQuiver[S, T](source: DOT[S], target: DOT[T], map: (S, T)*) =
    functionAsQuiver(source, target, Map(map: _*))

  def makeBiQuiver[L, R, T](
                             left: DOT[L],
                             right: DOT[R],
                             target: DOT[T],
                             mappings: ((L, R), T)*
                             ) =
    bifunctionAsBiQuiver[L, R, T](left, right, target) { (l, r) => Map(mappings:_*)((l, r)) }

  def makeNullaryOperator[X](carrier: DOT[X], value: X) =
    functionAsQuiver(I, carrier, (_: UNIT) => value)

  def makeBinaryOperator[X](
    carrier: DOT[X],
    mappings: ((X, X), X)*
  ) =
    bifunctionAsBiQuiver[X](carrier) { (x, y) => Map(mappings:_*)((x, y)) }

  private def intSqrt(square: Int) =
    (1 until square).find { n =>
      n * n == square
    }.getOrElse {
      throw new IllegalArgumentException("Not a valid monoid multiplication table: size " + square)
    }

  def monoidFromTable[M](table: M*): NaiveMonoid[M] = {
    val carrierSize = intSqrt(table.size)
    val carrierAsList = table.take(carrierSize)
    val carrier = makeStar(carrierAsList :_*)
    val mappings = for (i <- 0 until carrierSize ; j <- 0 until carrierSize)
      yield (carrierAsList(i), carrierAsList(j)) -> table(i * carrierSize + j)
    val product = makeBinaryOperator(carrier, mappings:_ *)
    NaiveMonoid[M](
      carrier,
      makeNullaryOperator(carrier, carrierAsList.head),
      product
    )
  }
}