package com.fdilke.bewl2.topos

import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*

import scala.Function.tupled
import scala.language.postfixOps
import com.fdilke.bewl2.utility.RichFunSuite

import scala.collection.immutable.Set

class LogicalOperationsSpec extends RichFunSuite:

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos._

  test("The truth object has the correct binary operations for logical operations") {
//    import logicalOperations._
//
//    and isArrow { _ & _ }
//    implies isArrow { !_ | _ }
//    or isArrow { _ | _ }
//    falsity isArrow { _ => false }
  }

//  test("The truth object has the correct binary operations for logical operations as enrichments") {
//    { (_: Ω) ∧ (_: Ω) } isArrow { _ & _ }
//    { (_: Ω) → (_: Ω) } isArrow { !_ | _ }
//    { (_: Ω) ∨ (_: Ω) } isArrow { _ | _ }
//  }

