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
          Set(
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

