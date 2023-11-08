package com.fdilke.bewl2.topos

import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*

import scala.Function.tupled
import scala.language.postfixOps
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._

import scala.collection.immutable.Set
import com.fdilke.bewl2.sets.Sets
import Sets._

class LogicalOperationsSpec extends FunSuite:

  test("The truth object has the correct binary operations for logical operations"):
    omega.sanityTest

    val logOps: LogicalOperations = logicalOperations
    import logOps._

    and isArrow { _ & _ }
    implies isArrow { !_ | _ }
    or isArrow { _ | _ }
    falsity isArrow { _ => false }

  test("The truth object has the correct binary operations for logical operations as enrichments"):
    tupled { (_: Boolean) ∧ (_: Boolean) } isArrow { _ & _ }
    tupled { (_: Boolean) → (_: Boolean) } isArrow { !_ | _ }
    tupled { (_: Boolean) ∨ (_: Boolean) } isArrow { _ | _ }

