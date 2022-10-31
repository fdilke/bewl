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

  private def intSqrt(square: Int) =
    (1 to square).find(n => n * n == square).getOrElse {
      throw new IllegalArgumentException("Not a valid monoid multiplication table: size " + square)
    }

  def monoidFromTable[M: Set](table: M*): Sets.Monoid[M] = {
    val carrierSize = intSqrt(table.size)
//    val carrierAsList = table.take(carrierSize)
//    val carrier = dot(carrierAsList: _*)
    val carrier = summon[Set[M]]
//    val carrierSize = carrier.size
    val carrierAsList = table.take(carrier.size)
    
    val mappings: Seq[((M, M), M)] =
      for {
        i <- 0 until carrier.size
        j <- 0 until carrier.size
      } yield (
        carrierAsList(i),
        carrierAsList(j)
      ) -> table(
        i * carrier.size + j
      )
    new Sets.Monoid[M](
      makeNullaryOperator[M](
        table.head
      ),
      makeBinaryOperator[M](
        mappings: _*
      )
    )
  }
