package com.fdilke.bewl2.experiments

import com.fdilke.bewl2.sets.Sets
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

/* This served its purpose but now causes weird internal Metals errors in the log  ... actually it's ok */
object CurriedDependentImplicits:
  trait FauxGroup[A]:
    trait Action[B]
  trait FauxDot[A]
  def noddy(
    block: FauxDot[Seq[Int]] ?=> FauxGroup[Seq[Int]] ?=> Unit
  ): Unit = ()
  noddy { (dot: FauxDot[Seq[Int]]) ?=> (group: FauxGroup[Seq[Int]]) ?=>
    ()
  }
  def noddy2(
    block: FauxDot[Seq[Int]] ?=> FauxDot[Int] ?=> FauxGroup[Seq[Int]] ?=> Unit
  ): Unit = ()
  noddy2 { (dot: FauxDot[Seq[Int]]) ?=> (dotInt: FauxDot[Int]) ?=> (group: FauxGroup[Seq[Int]]) ?=>
    ()
  }
  // def noddy3(
  //   block: FauxDot[Seq[Int]] ?=> (group: FauxGroup[Seq[Int]]) ?=> (action: group.Action[Int]) ?=> Unit
  // ): Unit = ()
  // noddy3 { (dot: FauxDot[Seq[Int]]) ?=> (group: FauxGroup[Seq[Int]]) ?=> (action: group.Action[Int]) ?=> 
  //   ()
  // }
  def noddy4(
    block: [E] => Sets.Dot[E] ?=> (monoid: Sets.Monoid[E]) ?=> monoid.Action[Int] ?=> Unit
  ): Unit = ()
  noddy4 {
    [E] => (_ : Sets.Dot[E]) ?=> (monoid: Sets.Monoid[E]) ?=> (action: monoid.Action[Int]) ?=>
      ()
  }
  def noddy5(
    block: [E] => Sets.Dot[E] ?=> (monoid: Sets.Monoid[E]) ?=> monoid.Action[Int] ?=> Unit
  ): Unit = ()
  noddy5 {
    [E] => (dot : Sets.Dot[E]) ?=> (monoid: Sets.Monoid[E]) ?=> (action: monoid.Action[Int]) ?=>
    // [E] => (_ : Sets.Dot[E]) ?=> (monoid: Monoid[E]) ?=> monoid.Action[X] ?=>
      ()
  }
  def noddy6(
    block: (group: Sets.Group[Seq[Int]]) ?=> group.Action[Int] => Unit
  ): Unit = ()
  noddy6 {
    (group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) =>
      ()
  }
  def noddy7(
    block: Sets.Dot[Seq[Int]] ?=> (group: Sets.Group[Seq[Int]]) ?=> group.Action[Int] => Unit
  ): Unit = ()
  noddy7 {
    (dot : Sets.Dot[Seq[Int]]) ?=> (group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) =>
      ()
  }
  def noddy8(
    block: Sets.Dot[Seq[Int]] ?=> Sets.Dot[Int] ?=> (group: Sets.Group[Seq[Int]]) ?=> group.Action[Int] => Unit
  ): Unit = ()
  noddy8 {
    (dot : Sets.Dot[Seq[Int]]) ?=> (ints: Sets.Dot[Int]) ?=> (group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) =>
      ()
  }
  // def noddy4(
  //   block: FauxDot[Seq[Int]] ?=> (dotInt: FauxDot[Int]) ?=> (group: FauxGroup[Seq[Int]]) ?=> (action: group.Action[Int]) ?=> Unit
  // ): Unit = ()
  // noddy4 { (dot: FauxDot[Seq[Int]]) ?=> (group: FauxGroup[Seq[Int]]) ?=> (action: group.Action[Int]) ?=> 
  //   ()
  // }

object OverridingInnerClasses:
  class Twiglet {
    class Piglet(x: Int)
    class Val
  }
  class Sniglet extends Twiglet { // can't
  // class Piglet(x: Int) extends super.Piglet
    // class Val extends Twiglet#Val
  }
  abstract class Topos[DOT[_]] {
    abstract class DefaultDot[X](dot: DOT[X]) { me: Dot[X] =>
    }
    type Dot[X] <: DefaultDot[X]
    def Dot[X](dot: DOT[X]): Dot[X]
  }

  class FancyTopos[DOT[_]] extends Topos[DOT] {
    class LocalDot[X](dot: DOT[X]) extends DefaultDot[X](dot){
    }
    override type Dot[X] = LocalDot[X]
    override def Dot[X](dot: DOT[X]): Dot[X] =
      new LocalDot(dot)
  }

// object OverridingTypes:
//   class Higgidy:
//     type X = List[Boolean]
//   class Piggidy extends Higgidy:
//     override type X = Seq[Boolean]

// object OverridingInnerClasses2:
//   class Topos[DOT[_]]:
//     class DefaultDot[X](dot: DOT[X])
//     class DotFactory:
//       type Dot[X] >: DefaultDot[X]
//       def Dot[X](dot: DOT[X]): Dot[X]
//     val dotFactory: DotFactory =
//       new DotFactory:
//         type Dot[X] = DefaultDot[X]
//         override def Dot[X](dot: DOT[X]): Dot[X] =
//           DefaultDot[X](dot)

// object OverridingInnerClasses3:
//   class Topos[DOT[_]]:
//     class ToolkitBuilder:
//       type TOOLKIT[_]
//       def buildToolkit[X : DOT]: TOOLKIT[X]
//     class DotFactory:
//       type Dot[X] >: DefaultDot[X]
//       def Dot[X](dot: DOT[X]): Dot[X]
//     val dotFactory: DotFactory =
//       new DotFactory:
//         type Dot[X] = DefaultDot[X]
//         override def Dot[X](dot: DOT[X]): Dot[X] =
//           DefaultDot[X](dot)

//   class PiggyTopos[DOT[_]] extends Topos[DOT]:
//     class InnerPig extends InnerHig

trait TestFType[A, F[AA] <: TestFType[AA, F]]
class SampleTestFType[A] extends TestFType[A, SampleTestFType]

object Pigg:
  trait ActionAnalysis[A, ACTION_ANALYSIS[AA] <: ActionAnalysis[AA, ACTION_ANALYSIS]]
  class DefaultActionAnalysis[A] extends ActionAnalysis[A, DefaultActionAnalysis]

object FurtherFTypes:
  trait ActionAnalysis[
    A,
    ACTION[_],
    INTERNAL_MAP[_, _],
    ACTION_ANALYSIS[AA] <: ActionAnalysis[AA, ACTION, INTERNAL_MAP, ACTION_ANALYSIS]
  ]
  class SetsActionAnalysis[A] extends ActionAnalysis[A, List, Map, SetsActionAnalysis]

object TrackPifflyError:
  trait Monoid[M]:
    type InternalMap[A, B] = (M, A) => B
    trait Action[A]:
      def x  = 0
  trait ActionAnalysis[
    A,
    ACTION[_],
    INTERNAL_MAP[_, _],
    ACTION_ANALYSIS[AA] <: ActionAnalysis[AA, ACTION, INTERNAL_MAP, ACTION_ANALYSIS]
  ]:
    def makeExponential[B](
      analysisB: ACTION_ANALYSIS[B]
    ): ACTION[INTERNAL_MAP[A, B]]
    def enumerateMorphisms[B](
      analysisB: ACTION_ANALYSIS[B]
    ): Iterable[A => B]

  def twiggle[M](monoid: Monoid[M]): Unit =
    class BaseDefaultActionAnalysis[A](action: monoid.Action[A]) extends 
      ActionAnalysis[A, monoid.Action, monoid.InternalMap, BaseDefaultActionAnalysis]:
      override def makeExponential[B](
        analysisB: BaseDefaultActionAnalysis[B]
      ): monoid.Action[monoid.InternalMap[A, B]] =
        ???
      override def enumerateMorphisms[B](
        analysisB: BaseDefaultActionAnalysis[B]
      ): Iterable[A => B] =
        ???
    ()
