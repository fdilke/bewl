package com.fdilke.utility

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import scala.collection.mutable.ListBuffer

class IterateToFixedSpec extends FunSuite:
  test("Iterating to a fixed point works"):
    val argumentsSeen = new ListBuffer[Int]
    def collatz(n: Int) =
      argumentsSeen += n
      if (n == 1)
        1
      else if (n % 2 == 0)
        n / 2
      else
        3 * n + 1
    
    IterateToFixed(6)(collatz) is 1
    argumentsSeen.toList is Seq(
      6, 3, 10, 5, 16, 8, 4, 2, 1
    )
  
