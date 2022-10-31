package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.Topos
import com.fdilke.utility.Shortcuts.*

import scala.annotation.targetName
import scala.language.{dynamics, postfixOps}
import Mappable.*
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.sets.SetsUtilities._
import Function.tupled
import scala.annotation.targetName

trait AlgebraicConstructions[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  →[_, _]
] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, →] =>

}

// Constructions specific to Sets (and maybe other topoi`) live here
object AlgebraicConstructions:

  def withCyclicGroup[R](
    order: Int
  )(
    block: [I] => Set[I] ?=> I =:= Int ?=> Int =:= I ?=> Sets.Group[I] => R
  ): R =
    maskSetDot[Int, R](
      dot = 0 until order toSet
    ) {
      [I] => (_: Set[I]) ?=> (_: I =:= Int) ?=> (_: Int =:= I) ?=>
        block[I](
          new Sets.Group[I](
            makeNullaryOperator[I](0),
            tupled { (x, y) => (x + y) % order },
            { (i: I) => (order - i) % order }
          )
        )
    }

