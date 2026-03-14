package com.fdilke.bewl.helper

import org.scalatest.funspec._
import org.scalatest.matchers.should.Matchers._

class MemoizeTest extends AnyFunSpec {

  private case class Input[T](seq: Seq[T])
  private case class Output[T](seq: Seq[T])

  private class Counterize[I, O](func: I => O) {
    private var count = 0

    def check(verify: => Unit): Unit = {
      count shouldBe 0
      verify
      count shouldBe 1
      verify
      count shouldBe 1
    }

    def apply[U](i: I): O = {
      count += 1
      func(i)
    }
  }

  private class CounterizeGeneric[I[T], O[T]](func: I[_] => O[_]) {
    private var count = 0

    def check(verify: => Unit): Unit = {
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

  private class CounterizeGenericWithBase[BASE, I[T <: BASE], O[T <: BASE]](
    func: I[BASE] => O[BASE]
  ) {
    private var count = 0

    def check(verify: => Unit): Unit = {
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

    it("caches results for a vanilla function") {
      def testFunc(n: Int): String =
        "Hello " + n

      val counter = new Counterize[Int, String](testFunc)

      def instrumentedFunc(input: Int): String =
        counter(input)

      val memoizedFunc = Memoize(instrumentedFunc)

      counter.check {
        memoizedFunc(2) shouldBe "Hello 2"
      }
    }

    it("caches its results, with a generic parameter") {
      def testFunc(input: Input[_]): Output[_] =
        new Output(input.seq ++ input.seq)

      val counter = new CounterizeGeneric[Input, Output](testFunc)

      def instrumentedFunc[T](input: Input[T]): Output[T] =
        counter(input)

      val memoizedFunc = Memoize.generic(instrumentedFunc)

      counter.check {
        memoizedFunc(
          new Input[Int](Seq(2))
        ).seq shouldBe Seq(2, 2)
      }
    }

    it("still works with structural types") {
      type Widget[T] = Seq[Option[T]]

      def testFunc[T](input: Seq[_]): Widget[_] =
        Seq(input.headOption, None)

      val counter = new CounterizeGeneric[Seq, Widget](testFunc)

      def instrumentedFunc[T](input: Seq[T]): Widget[T] =
        counter(input)

      val memoizedFunc = Memoize.generic(instrumentedFunc)

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

      val counter = new CounterizeGenericWithBase[Number, Seq, Widget](testFunc)

      def instrumentedFunc[T <: Number](input: Seq[T]): Widget[T] =
        counter(input)

      val memoizedFunc = Memoize.generic.withLowerBound[Seq, Widget, Number](instrumentedFunc)

      counter.check {
        memoizedFunc[java.lang.Double](
          Seq(2.0)
        ) shouldBe Seq(Some(2.0), None)
      }
    }
  }
}
