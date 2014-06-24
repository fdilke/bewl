package com.fdilke.bewl.fsets

import com.fdilke.bewl._
import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities.allMaps
import Function.{tupled, const}

object FiniteSets extends Topos {
  type DOT[X] = FiniteSetsDot[X]
  type ARROW[S, T] = FiniteSetsArrow[S, T]
  type BIPRODUCT[L, R] = FiniteSetsBiproduct[L, R]
  type EXPONENTIAL[S, T] = FiniteSetsExponential[S, T]
  type EQUALIZER[M, T] = FiniteSetsEqualizer[M, T]
  type EQUALIZER_SOURCE[M, T] = M
  type TERMINAL = Unit
  type OMEGA = Boolean

  override val I = FiniteSetsDot[Unit](Traversable(()))
  override val omega = FiniteSetsDot[Boolean](Traversable(true, false))
  override val truth = FiniteSetsArrow[Unit, Boolean](I, omega, const (true) _)

  case class FiniteSetsDot[X](elements: Traversable[X]) extends Dot[X] with Traversable[X] {
    override def toString = elements.toString

    override def foreach[U](f: (X) => U) { elements.foreach(f) }

    override def identity: FiniteSetsArrow[X, X] = FiniteSetsArrow(this, this, x => x)

    override def multiply[Y](that: FiniteSetsDot[Y]) = new FiniteSetsBiproduct[X, Y](this, that)

    override def exponential[Y](that: FiniteSetsDot[Y]) = new FiniteSetsExponential[Y, X](that, this)

    override def toI = FiniteSetsArrow[X, TERMINAL](this, FiniteSets.I, const () _)
  }

  case class FiniteSetsArrow[X, Y](
    source: FiniteSetsDot[X],
    target: FiniteSetsDot[Y],
    function: X => Y
  ) extends Arrow[X, Y] {
    override def toString = s"""FiniteSetsArrow(${source}, ${target},
      |${Map(source.map(x => (x, function(x))).toList: _*)})""".stripMargin

    override def apply[W](arrow: FiniteSetsArrow[W, X]) =
      if (arrow.target == source) {
        FiniteSetsArrow(arrow.source, target,  function.compose(arrow.function))
      } else {
        throw new IllegalArgumentException("Target does not match source")
      }

    override def ?=(that: FiniteSetsArrow[X, Y]): EQUALIZER[X, Y] =
      new FiniteSetsEqualizer(this, that)

    override lazy val chi = new Characteristic[X, Y] {
      val arrow = FiniteSetsArrow[Y, Boolean](target, omega,
        (y: Y) => source.exists(function(_) == y)
      )
      def restrict[W](arrow: ARROW[W, Y]) = FiniteSetsArrow[W, X](
        arrow.source, source, (w: W) => {
          val y = arrow.function(w)
          source.find(function(_) == y).getOrElse(???)
      })
    }

    // TODO: clean up unnecessary type paramaters?

    override def equals(other: Any): Boolean = other match {
      case that: FiniteSetsArrow[X, Y] =>
        source == that.source && target == that.target &&
          source.forall(x => function(x) == that.function(x))
      case _ => false
    }

    override def hashCode(): Int = 0 // don't use these as keys

    def sanityTest =
      if (source.map(function).exists(x => target.forall(_ != x))) {
        throw new IllegalArgumentException("Map values not in target")
      }
  }

  class FiniteSetsBiproduct[L, R](left: FiniteSetsDot[L], right: FiniteSetsDot[R]
                                   ) extends Biproduct[L, R] {
    override val product = FiniteSetsDot[(L, R)](for (x <- left; y <- right) yield (x, y))

    override val leftProjection = FiniteSetsArrow[(L, R), L](product, left, tupled { (x, y)  => x} )

    override val rightProjection = FiniteSetsArrow[(L, R), R](product, right, tupled { (x, y) => y})

    override def multiply[W](leftArrow: FiniteSetsArrow[W, L], rightArrow: FiniteSetsArrow[W, R]) =
      FiniteSetsArrow(leftArrow.source, product, { x => (leftArrow.function(x), rightArrow.function(x))} )
  } // TODO: ^ can make arrow act on X directly?

  class FiniteSetsExponential[S, T](source: FiniteSetsDot[S], target: FiniteSetsDot[T])
    extends Exponential[S, T] {

    // the 'function equality' semantics are needed even if these are all maps, because
    // we'll be comparing against things that aren't
    val theAllMaps: Traversable[S => T] = allMaps(source, target).
      map(FunctionWithEquality(source, _))
    val exponentDot: FiniteSetsDot[S => T] = FiniteSetsDot[S => T](theAllMaps)

    override val evaluation = new BiArrow[S => T, S, T](exponentDot, source,
      FiniteSetsArrow[(S => T, S), T](exponentDot x source, target, tupled {
        (f:S => T, s:S) => f(s)
      }))

    override def transpose[W](multiArrow: BiArrow[W, S, T]) =
      FiniteSetsArrow[W, S => T](multiArrow.left, exponentDot,
        (w: W) => FunctionWithEquality[S, T](multiArrow.right, { s => multiArrow.arrow.function((w, s)) })
      )
  }

  class FiniteSetsEqualizer[M, T](arrow: FiniteSetsArrow[M, T], arrow2: FiniteSetsArrow[M, T])
    extends Equalizer[M, T] {
      import arrow._

      override val equalizerSource = FiniteSetsDot[EQUALIZER_SOURCE[M, T]](
        source.filter(s => arrow.function(s) == arrow2.function(s))
      )

    override val equalizer = FiniteSetsArrow[EQUALIZER_SOURCE[M, T], M](
      equalizerSource, source, identity
    )

    override def restrict[S](equalizingArrow: FiniteSetsArrow[S, M]) = FiniteSetsArrow[S, M](
      equalizingArrow.source, equalizerSource, equalizingArrow.function
    )
  }

  object FiniteSetsBiArrow {
    def apply[L, R, T](left: FiniteSetsDot[L],
                       right: FiniteSetsDot[R],
                       target: FiniteSetsDot[T],
              map: ((L, R), T)*) : BiArrow[L, R, T] =
      BiArrow[L, R, T](left, right,
        FiniteSetsArrow[(L, R), T](left x right, target, Map(map:_*)))
  }

  object FiniteSetsUtilities {
    def arrow[S, T](source: FiniteSetsDot[S], target: FiniteSetsDot[T], map: (S, T)*) =
      FiniteSetsArrow[S, T](source, target, Map(map:_*))

    def dot[T](elements: T*) = FiniteSetsDot(elements)

    def cartesian[A](factors: Seq[Seq[A]]): Traversable[Seq[A]] = factors match {
        case Nil => Traversable(Seq())
        case head :: tail =>
          for (h <- head; sequence <- cartesian[A](tail))
                yield(h +: sequence)
      }

      def allMaps[A, B](source: Traversable[A], target: Traversable[B]): Traversable[A=>B] =
      new Traversable[A=>B] {
        override def foreach[U](func: (A => B) => U): Unit =
          if (source.isEmpty)
            func(_ => ???)
          else
            for(f <- allMaps(source.tail, target);
                choice <- target) {
                func { x => if (x == source.head) choice else f(x) }
              }
    }
  }
}
