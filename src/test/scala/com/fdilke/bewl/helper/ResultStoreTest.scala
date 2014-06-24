package com.fdilke.bewl.helper

import org.scalatest.{Matchers, FunSpec, FunSuite}
import Matchers._

class ResultStoreTest extends FunSpec {

  describe("A ResultStore") {
    it("should cache values") {
      var numCalls = 0
      def isEven(n: Int) = {
        numCalls += 1
        (n % 2) == 0
      }
      val store = new ResultStore[Int, Boolean](isEven)

      numCalls shouldBe 0
      store(2) shouldBe true
      numCalls shouldBe 1
      store(2) shouldBe true
      numCalls shouldBe 1
    }
  }
}
