package com.fdilke.bewl.helper

import org.scalatest.{Matchers, FunSpec}
import Matchers._

class MemoizeTest extends FunSpec {

  private case class Input[T](seq: Seq[T])
  private case class Output[T](seq: Seq[T])

  private class Counterize[I[T], O[T], T](func: I[_] => O[_]) {
    private var count = 0

    def check(verify: => Unit) {
      count shouldBe 0
      verify
      count shouldBe 1
      verify
      count shouldBe 1
    }

    def apply[U](i: I[U]): O[U] = {
      count += 1
      func(i.asInstanceOf[I[T]]).asInstanceOf[O[U]]
    }
  }

  describe("A memoized function") {

    it("caches its results") {
      var numCalls = 0

      def testFunc(input: Input[_]): Output[_] =
        new Output(input.seq ++ input.seq)

      val counter = new Counterize[Input, Output, Nothing](testFunc)

      def instrumentedFunc[T](input: Input[T]): Output[T] =
        counter(input)

      val memoizedFunc = Memoize[Input, Output, Any](instrumentedFunc)

      counter.check {
        memoizedFunc[Int](
          new Input[Int](Seq(2))
        ).seq shouldBe Seq(2,2)
      }
    }

    it("still works with structural types") {
      type Widget[T] = Seq[Option[T]]

      def testFunc[T](input: Seq[_]): Widget[_] =
        Seq(input.headOption, None)

      val counter = new Counterize[Seq, Widget, Nothing](testFunc)

      def instrumentedFunc[T](input: Seq[T]): Widget[T] =
        counter(input)

      val memoizedFunc = Memoize[Seq, Widget, Any](instrumentedFunc)

      counter.check {
        memoizedFunc(
          Seq(2)
        ) shouldBe Seq(Some(2), None)
      }
    }

    /* Note may need to automate this kind of fancy-pants code:
  private object standardWrappedDot {
    private case class WrappedStar[T](star: STAR[WrappedArrow[T]])

    private def wrapDot[T](dot: DOT[T]) = WrappedStar[T](
      new WrappedDot(dot)
    )

    private val memoized = Memoize(wrapDot)

    def apply[T](dot: DOT[T]): STAR[WrappedArrow[T]] =
      memoized(dot).star
  }
     */
  }
}


