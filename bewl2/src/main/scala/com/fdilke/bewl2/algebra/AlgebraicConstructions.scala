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

// Constructions specific to Sets (and maybe other topoi) live here
object AlgebraicConstructions:

  def withCyclicGroup[R](
    order: Int
  )(
    block: [I] => Set[I] ?=> I =:= Int ?=> Int =:= I ?=> Sets.Group[I] ?=> R
  ): R =
    maskSetDot[Int, R](
      dot = 0 until order toSet
    ) {
      [I] => (_: Set[I]) ?=> (_: I =:= Int) ?=> (_: Int =:= I) ?=>
        implicit val _: Sets.Group[I] =
          new Sets.Group[I](
            makeNullaryOperator[I](0),
            tupled { (x, y) => (x + y) % order },
            { (i: I) => (order - i) % order }
          )
        block[I]
    }

  private def intSqrt(square: Int) =
    (1 to square).find(n => n * n == square).getOrElse {
      throw new IllegalArgumentException("Not a valid monoid multiplication table: size " + square)
    }

  def withMonoidFromTable[M, RESULT](
    table: M*
  )(
    block: Set[M] ?=> Sets.Monoid[M] ?=> RESULT
  ): RESULT =
    val carrierSize = intSqrt(table.size)
    val carrierAsList = table.take(carrierSize)

    val mappings: Seq[((M, M), M)] =
      for {
        i <- 0 until carrierSize
        j <- 0 until carrierSize
      } yield (
        carrierAsList(i),
        carrierAsList(j)
      ) -> table(
        i * carrierSize + j
      )

    implicit val _: Set[M] = carrierAsList.toSet
    implicit val _: Sets.Monoid[M] =
      Sets.Monoid[M](
        makeNullaryOperator[M](
          table.head
        ),
        makeBinaryOperator[M](
          mappings: _*
        )
      )

    block

  def withGroupFromTable[G, RESULT](
    table: G*
  )(
    block: Set[G] ?=> Sets.Group[G] ?=> RESULT
  ): RESULT =
    val carrierSize = intSqrt(table.size)
    val carrierAsList = table.take(carrierSize)

    val mappings: Seq[((G, G), G)] =
      for {
        i <- 0 until carrierSize
        j <- 0 until carrierSize
      } yield (
        carrierAsList(i),
        carrierAsList(j)
      ) -> table(
        i * carrierSize + j
      )

    implicit val _: Set[G] = carrierAsList.toSet
    val binOp = makeBinaryOperator[G](mappings: _*)
    val theUnit: G = table.head
    implicit val _: Sets.Group[G] =
      Sets.Group[G](
        unit = makeNullaryOperator[G](theUnit),
        multiply = binOp,
        inverse = { (g: G) =>
          carrierAsList.find { h =>
            binOp((g, h)) == theUnit
          }.get
        }
      )

    block

  def withSymmetricGroup[RESULT](
    degree: Int
  )(
    block: Set[Seq[Int]] ?=> Sets.Group[Seq[Int]] ?=> RESULT
  ): RESULT =
    val symbols: Seq[Int] = (0 until degree)
    implicit val permutations: Set[Seq[Int]] =
      symbols.permutations.toSet[Seq[Int]]

    implicit val _: Sets.Group[Seq[Int]] =
      Sets.Group[Seq[Int]](
        unit = makeNullaryOperator[Seq[Int]](symbols),
        multiply = { (p1: Seq[Int], p2: Seq[Int]) =>
          symbols map { s => p2(p1(s))}
        },
        inverse = { (p: Seq[Int]) =>
          val array: Array[Int] = new Array[Int](degree)
          symbols.foreach { s =>
            array(p(s)) = s
          }
          array.toSeq
        }
      )

    block