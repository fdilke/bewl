package com.fdilke.bewl.fsets

import com.fdilke.bewl.diagrammatic.DiagrammaticTopos
import com.fdilke.bewl.fsets.DiagrammaticFiniteSetsUtilities.allMaps
import com.fdilke.bewl.helper.FunctionWithEquality
import com.fdilke.bewl.topos.StarsAndQuiversAdapter
import scala.Function.{const, tupled}

object DiagrammaticFiniteSets extends DiagrammaticTopos {
  case class DiagrammaticFiniteSetsDot[X](elements: Traversable[X]) extends Dot[X] with Traversable[X] {
    override def toString = elements.toString

    override def foreach[U](f: (X) => U) { elements.foreach(f) }

    override def identity: DiagrammaticFiniteSetsArrow[X, X] = DiagrammaticFiniteSetsArrow(this, this, x => x)

    override def multiply[Y](that: DiagrammaticFiniteSetsDot[Y]) =
      new Biproduct[X, Y] {
        override val product = DiagrammaticFiniteSetsDot[(X, Y)](for (x <- DiagrammaticFiniteSetsDot.this; y <- that) yield (x, y))

        override val leftProjection = DiagrammaticFiniteSetsArrow[(X, Y), X](product, DiagrammaticFiniteSetsDot.this, tupled { (x, y)  => x} )

        override val rightProjection = DiagrammaticFiniteSetsArrow[(X, Y), Y](product, that, tupled { (x, y) => y})

        override def multiply[W](leftArrow: DiagrammaticFiniteSetsArrow[W, X], rightArrow: DiagrammaticFiniteSetsArrow[W, Y]) =
          DiagrammaticFiniteSetsArrow(leftArrow.source, product, x => (leftArrow.function(x), rightArrow.function(x)) )
    }

    override def exponential[Y](that: DiagrammaticFiniteSetsDot[Y]) =
      new Exponential[Y, X] {
        // the 'function equality' semantics are needed even if these are all maps, because
        // we'll be comparing against things that aren't
        val theAllMaps: Traversable[Y => X] = allMaps(that, DiagrammaticFiniteSetsDot.this).
          map(FunctionWithEquality(that, _))
        override val exponentDot: DiagrammaticFiniteSetsDot[Y => X] = DiagrammaticFiniteSetsDot[Y => X](theAllMaps)

        override val evaluation = new BiArrow[Y => X, Y, X](exponentDot, that,
          DiagrammaticFiniteSetsArrow[(Y => X, Y), X](exponentDot x that, DiagrammaticFiniteSetsDot.this, tupled {
            (f:Y => X, y:Y) => f(y)
          }))

        override def transpose[W](multiArrow: BiArrow[W, Y, X]) =
          DiagrammaticFiniteSetsArrow[W, Y => X](multiArrow.left, exponentDot,
            (w: W) => FunctionWithEquality[Y, X](multiArrow.right, { s => multiArrow.arrow.function((w, s)) })
          )
      }


    override def toI = DiagrammaticFiniteSetsArrow[X, TERMINAL](this, I, const () _)

    override def sanityTest =
      for (x <- elements ; y <- elements)
        x == y
  }

  case class DiagrammaticFiniteSetsArrow[X, Y](
    source: DiagrammaticFiniteSetsDot[X],
    target: DiagrammaticFiniteSetsDot[Y],
    function: X => Y
  ) extends Arrow[X, Y] {
    override def toString = s"""FiniteSetsArrow(${source}, ${target},
      |${Map(source.map(x => (x, function(x))).toList: _*)})""".stripMargin

    override def apply[W](arrow: DiagrammaticFiniteSetsArrow[W, X]) =
      if (arrow.target == source) {
        DiagrammaticFiniteSetsArrow(arrow.source, target,  function.compose(arrow.function))
      } else {
        throw new IllegalArgumentException("Target does not match source")
      }

    override def ?=(that: DiagrammaticFiniteSetsArrow[X, Y]) =
      new Equalizer[X] {

      override val equalizerSource = DiagrammaticFiniteSetsDot[EQUALIZER_SOURCE[X]](
        source.filter(s => function(s) == that.function(s))
      )

      override val equalizer = DiagrammaticFiniteSetsArrow[EQUALIZER_SOURCE[X], X](
        equalizerSource, source, identity
      )

      override def restrict[S](equalizingArrow: DiagrammaticFiniteSetsArrow[S, X]) = DiagrammaticFiniteSetsArrow(
        equalizingArrow.source, equalizerSource, equalizingArrow.function
      )
    }

    override lazy val chi = new Characteristic[X, Y] {
      val arrow = DiagrammaticFiniteSetsArrow[Y, Boolean](target, omega,
        (y: Y) => source.exists(function(_) == y)
      )
      def restrict[W](arrow: ARROW[W, Y]) = DiagrammaticFiniteSetsArrow[W, X](
        arrow.source, source, (w: W) => {
          val y = arrow.function(w)
          source.find(function(_) == y).getOrElse(???)
        })
    }

    override def equals(other: Any): Boolean = other match {
      case that: DiagrammaticFiniteSetsArrow[X, Y] =>
        source == that.source && target == that.target &&
          source.forall(x => function(x) == that.function(x))
      case _ => false
    }

    override def hashCode(): Int = 0 // don't use these as keys

    def sanityTest =
      if (!source.map(function).forall(x => target.exists(_ == x))) {
        throw new IllegalArgumentException("Map values not in target")
      }
  }

  object FiniteSetsBiArrow {
    def apply[L, R, T](left: DiagrammaticFiniteSetsDot[L],
                       right: DiagrammaticFiniteSetsDot[R],
                       target: DiagrammaticFiniteSetsDot[T],
                       map: ((L, R), T)*) : BiArrow[L, R, T] =
      BiArrow[L, R, T](left, right,
        DiagrammaticFiniteSetsArrow[(L, R), T](left x right, target, Map(map:_*)))
  }

  override type DOT[X] = DiagrammaticFiniteSetsDot[X]
  override type ARROW[S, T] = DiagrammaticFiniteSetsArrow[S, T]
  override type EQUALIZER_SOURCE[M] = M
  override type TERMINAL = Unit
  override type OMEGA = Boolean

  override val I = DiagrammaticFiniteSetsDot[Unit](Traversable(()))
  override val omega = DiagrammaticFiniteSetsDot[Boolean](Traversable(true, false))
  override val truth = DiagrammaticFiniteSetsArrow[Unit, Boolean](I, omega, const (true) _)

  override def buildArrow[S, T](source: DiagrammaticFiniteSetsDot[S], target: DiagrammaticFiniteSetsDot[T], f: S => T) =
    DiagrammaticFiniteSetsArrow(source, target, f)
}

object LayeredFiniteSets extends StarsAndQuiversAdapter(DiagrammaticFiniteSets)