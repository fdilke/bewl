package com.fdilke.utility;

import junit.framework.TestCase
import Mask.*
import com.fdilke.bewl2.utility.RichFunSuite

class MaskSpec extends RichFunSuite:
  test("Can mask a type") {
    mask[Int, Set, Boolean](
      Set[Int](1, 2, 3)
    ){ [I] => ( input : Set[I]) => (_: I =:= Int) ?=> (_: Int =:= I) ?=>
      input.size > 2
    } is true
  }

  test("Can mask a type and calculate values parameterized on it") {
    mask[Int, List, Set[Int]](
      List[Int](1, 2, 3)
    ){ [I] => ( input : List[I]) => (i2int: I =:= Int) ?=> (_: Int =:= I) ?=>
      i2int.substituteCo[Set](input.toSet)
    } is Set[Int](1, 2, 3)
  }

  // test("Can pass a parameter of a type parameterized by a masked type") {
  //   mask[Int, List, Set[Int]](
  //     List[Int](1, 2, 3)
  //   ){ [I] => ( input : List[I]) => (i2int: I =:= Int) ?=> (_: Int =:= I) ?=>
  //     i2int.substituteCo[Set](input.toSet)
  //   } is Set[Int](1, 2, 3)
  // }

