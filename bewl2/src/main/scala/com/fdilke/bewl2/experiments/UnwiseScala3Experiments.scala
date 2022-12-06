package com.fdilke.bewl2.experiments

object UnwiseScala3Experiments {

  trait Crappable[A, CTXT[_] /* <: Mappable[A, CTXT]] */ ] { self: CTXT[A] =>
    def map[B](f: A => B): CTXT[B]
  }

  def f[
    CTXT[A] <: Crappable[A, CTXT]
  ]: Int =
    2

  //    monad.eta
  //    val f: X => CTXT[X] =
  //      x => monad.eta(x)
  //    val g: X ~> X = f
  //    g

  // initial abortive experiments with type lambdas
  //  type M = [X, Y] =>> Map[Y, X]
  //  ({ type λ[T] = triadicMonoid.Action[T] })#λ
  //  type FTYPE = [H, X] =>> ( type { H[Y <: H[Y]] })#Y
  //  H[X <: H[X]]

  object PolymorphicPiffle {
    // A polymorphic method:
    def foo[A](xs: List[A]): List[A] = xs.reverse

    // A polymorphic function value:
    val bar: [A] => List[A] => List[A]
    // ^^^^^^^^^^^^^^^^^^^^^^^^^
    // a polymorphic function type
    = [A] => (xs: List[A]) => foo[A](xs)

  }

  object MorePolymorphicPiffle {
    // A polymorphic method:
    def foo[A, B](xs: List[A]): List[B] = List.empty

    // A polymorphic function value:
    val bar: [A, B] => List[A] => List[B]
      = [A, B] => (xs: List[A]) => foo[A, B](xs)

  }

  object StillMorePolymorphicPiffle {
    // A polymorphic method:
    def foo[A, B](xs: List[A], ys: List[B]): List[B] = List.empty

    // A polymorphic function value:
    val bar: [A, B] => (List[A], List[B]) => List[B]
      = [A, B] => (xs: List[A], ys: List[B]) => foo[A, B](xs, ys)

  }

  object YetStillMorePolymorphicPiffle {
    // A polymorphic method:
    def foo[A, B](xs: (List[A], List[B])): List[B] = List.empty

    // A polymorphic function value:
    val bar: [A, B] => ((List[A], List[B])) => List[B]
      = [A, B] => (xsys: (List[A], List[B])) => foo[A, B](xsys)

  }

  trait HasConstructionArgs(n: Int) {}
  object FancyWidget extends HasConstructionArgs(3)

  sealed trait Node[X]
  class LeafNode[X](leaf: X) extends Node[X]
  class BranchNode[X](left: => Node[X], right: => Node[X]) extends Node[X]

  object SelfRefNode extends BranchNode[Unit](left = SelfRefNode, right = SelfRefNode)

  def variableThing(name: String): Node[String] =
    lazy val theVar: Node[String] = BranchNode[String](theVar, theVar)
    theVar

  abstract class Widget(n: Int) {
    val parent: Widget
  }

  val selfParentWidget = new Widget(3) {
    val parent: Widget = this
  }

//  def variableThing2(name: String): Node[String] =
//    new BranchNode[String](BranchNode.this, BranchNode.this)
}

// It would be nice if this could be made to work:
// wrap functions taking an untupled biarrow and pass them to something expecting a tupled one
// instead, for now, we have convenience versions of ∀[X, Y] which take the untupled
//
//  inline def untupledBiArrow[X: Dot, Y: Dot, Z: Dot](
//    block: (CTXT[X], CTXT[Y]) => CTXT[Z]
//  ): BiArrow[X, Y, Z] = {
//    case x ⊕ y => block(x, y)
//  }

//  implicit class FunnyExtension[X: Dot, Y: Dot, Z: Dot](
//    typedQuantifier: ((X, Y) ~> Z) => (X ~> Z)
//  ):
//    def apply(
//      untupledBlock: (CTXT[X], CTXT[Y]) => CTXT[Z]
//    ): X ~> Z =
//      typedQuantifier {
//        case (x: CTXT[X]) ⊕ (y: CTXT[Y]) => untupledBlock(x, y)
//      }

//  extension[RESULT[_, _, _]](
//    block: [X, Y, Z] => Dot[X] ?=> Dot[Y] ?=> Dot[Z] ?=> ((CTXT[X], CTXT[Y]) => CTXT[Z]) => RESULT[X, Y, Z]
//  ): RESULT = {
//    case x ⊕ y => block(x, y)
//  }

object Wiggis:
  enum Arity:
    case Nullary, Unary

object EnumFrenzy extends App:
  import Wiggis._
  println("arities: " + Arity.values)

object MultipleTraits:
  trait Theory:
    trait Algebra
  val widgets: Theory = new Theory {}
  val doodads: Theory = new Theory {}
  // but then can't do this, 'trait Algebra is extended twice'
  // trait HyperSquiddle extends widgets.Algebra with doodads.Algebra

