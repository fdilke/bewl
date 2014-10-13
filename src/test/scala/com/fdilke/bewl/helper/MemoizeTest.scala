package com.fdilke.bewl.helper

import org.scalatest.{Matchers, FunSpec}
import Matchers._

class MemoizeTest extends FunSpec {

  private case class Input[T](seq: Seq[T])
  private case class Output[T](seq: Seq[T])

  describe("A memoized function") {

    it("caches its results") {
      var numCalls = 0

      def testFunc[T](input: Input[T]): Output[T] = {
        numCalls += 1
        new Output(input.seq ++ input.seq)
      }

      val memoizedFunc = Memoize(testFunc)

      def checkIt =
        memoizedFunc(
          new Input[Int](Seq(2))
        ).seq shouldBe Seq(2,2)

      numCalls shouldBe 0
      checkIt
      numCalls shouldBe 1
      checkIt
      numCalls shouldBe 1
    }
  }
}
