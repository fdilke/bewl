package com.fdilke.bewl2.categories

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.helper.BuildEquivalence
import scala.language.{existentials, reflectiveCalls}

class FiniteCategorySpec extends FunSuite:
  test("Can create the trivial category 1"):
    val category =
      FiniteCategory[String]("1")()
    category.sanityTest()

  test("Can create the empty category 1"):
    val category =
      FiniteCategory[String]()()
    category.sanityTest()
  