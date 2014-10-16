package com.fdilke.bewl.helper

import org.scalatest.{Matchers, FunSpec}
import Matchers._

class MemoizeTest extends FunSpec {

  private case class Input[T](seq: Seq[T])
  private case class Output[T](seq: Seq[T])

  private class Counterize[I[T], O[T]](func: I[_] => O[_]) {
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
      func(i.asInstanceOf[I[_]]).asInstanceOf[O[U]]
    }
  }

  private class CounterizeWithBase[BASE, I[T <: BASE], O[T <: BASE]](func: I[BASE] => O[BASE]) {
    private var count = 0

    def check(verify: => Unit) {
      count shouldBe 0
      verify
      count shouldBe 1
      verify
      count shouldBe 1
    }

    def apply[U <: BASE](i: I[U]): O[U] = {
      count += 1
      func(i.asInstanceOf[I[BASE]]).asInstanceOf[O[U]]
    }
  }

  describe("A memoized function") {

    it("caches its results") {
      var numCalls = 0

      def testFunc(input: Input[_]): Output[_] =
        new Output(input.seq ++ input.seq)

      val counter = new Counterize[Input, Output](testFunc)

      def instrumentedFunc[T](input: Input[T]): Output[T] =
        counter(input)

      val memoizedFunc = Memoize(instrumentedFunc)

      counter.check {
        memoizedFunc(
          new Input[Int](Seq(2))
        ).seq shouldBe Seq(2,2)
      }
    }

    it("still works with structural types") {
      type Widget[T] = Seq[Option[T]]

      def testFunc[T](input: Seq[_]): Widget[_] =
        Seq(input.headOption, None)

      val counter = new Counterize[Seq, Widget](testFunc)

      def instrumentedFunc[T](input: Seq[T]): Widget[T] =
        counter(input)

      val memoizedFunc = Memoize(instrumentedFunc)

      counter.check {
        memoizedFunc(
          Seq(2)
        ) shouldBe Seq(Some(2), None)
      }
    }

    it("still works with structural types and a lower bound") {
      type Widget[T <: Number] = Seq[Option[T]]

      def testFunc[T <: Number](input: Seq[T]): Widget[T] =
        Seq(input.headOption, None)

      val counter = new CounterizeWithBase[Number, Seq, Widget](testFunc)

      def instrumentedFunc[T <: Number](input: Seq[T]): Widget[T] =
        counter(input)

      val memoizedFunc = Memoize.withLowerBound[Seq, Widget, Number](instrumentedFunc)

      counter.check {
        memoizedFunc[java.lang.Double](
          Seq(2.0)
        ) shouldBe Seq(Some(2.0), None)
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


