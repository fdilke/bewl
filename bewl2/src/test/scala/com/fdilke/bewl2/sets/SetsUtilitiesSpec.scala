package com.fdilke.bewl2.sets;

import junit.framework.TestCase
import munit.FunSuite;
import SetsUtilities._

class SetsUtilitiesSpec extends FunSuite:
  test("enumerates all maps between two sets") {
    assertEquals(
        allMaps(Set(1, 2), Set("a", "b", "c")).map { f =>
            Seq(f(1), f(2))
        },
        Iterable(
           Seq("a", "a"),
           Seq("b", "a"),
           Seq("c", "a"),
           Seq("a", "b"),
           Seq("b", "b"),
           Seq("c", "b"),
           Seq("a", "c"),
           Seq("b", "c"),
           Seq("c", "c")
        )
      )
  }

  test("gives sensible results even when the source is empty") {
    assertEquals( allMaps(Set[String](), Set(0)).size, 1)
  }

  test("gives sensible results even when the target is empty") {
    assert( allMaps(Set(0), Set()).isEmpty )
  }

  test("gives sensible results even when both source and target are empty") {
    assertEquals( allMaps(Set(), Set()).size, 1 )
  }

  test("enumerates n-ary operations: degenerate case of binaries on 1") {
    assertEquals(
      allNaryOps(arity = 2, order = 1) map { f =>
        f(0, 0)
      },
      Iterable(0)
    )
  }

  test("enumerates n-ary operations: degenerate case of unaries on 2") {
    assertEquals(
      allNaryOps(arity = 1, order = 2) map { f =>
        f(0) -> f(1)
      },
      Iterable(
        0 -> 0,
        1 -> 0,
        0 -> 1,
        1 -> 1
      )
    )
  }

  test("enumerates n-ary operations: even more degenerate case of nullaries on 0") {
    assertEquals(
      allNaryOps(arity = 0, order = 0),
      Iterable.empty
    )
  }