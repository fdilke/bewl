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
}
