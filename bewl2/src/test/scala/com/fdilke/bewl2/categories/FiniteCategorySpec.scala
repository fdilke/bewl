package com.fdilke.bewl2.categories

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.helper.BuildEquivalence
import scala.language.{existentials, reflectiveCalls}

class FiniteCategorySpec extends FunSuite:
  test("Can create the empty category 1"):
    FiniteCategory[String]()()()

  test("Can create the trivial category 1"):
    FiniteCategory[Int](1)()()
  
  test("requires listing all sources/targets"):
    intercept[IllegalArgumentException]:
      FiniteCategory[Int]()(
        FiniteCategory.SimplePreArrow[Int]("arrow", 1, 2)
      )()
  intercept[IllegalArgumentException]:
    FiniteCategory[Int](1)(
      FiniteCategory.SimplePreArrow[Int]("arrow", 1, 2)
    )()
  intercept[IllegalArgumentException]:
    FiniteCategory[Int](2)(
      FiniteCategory.SimplePreArrow[Int]("arrow", 1, 2)
    )()
  FiniteCategory[Int](1, 2)(
    FiniteCategory.SimplePreArrow[Int]("arrow", 1, 2)
  )()
  
  test("can model the retraction category"):
    val retraction: PreArrow[Boolean] =
      FiniteCategory.SimplePreArrow[Boolean]("retraction", true, false)
    val section: PreArrow[Boolean] =
      FiniteCategory.SimplePreArrow[Boolean]("section", false, true)
    FiniteCategory[Boolean](true, false)(
      retraction, section
    )(
      retraction o section := CompositionLaw.ONE
    )

