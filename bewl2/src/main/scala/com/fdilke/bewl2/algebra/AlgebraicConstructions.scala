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
  >[_, _]
] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

//  trait Twizzler[X : DOT] { // TODO: tidy up
//    def apply[RESULT](
//      block: [E] => DOT[E] ?=> Monoid[E] ?=> RESULT
//    ): RESULT
//  }
  trait EndomorphismMonoid[E: DOT, X : DOT] extends Monoid[E] with Actions[E] {
    val standardAction: Action[X]
  }

  def withEndomorphismMonoid[X : DOT, RESULT](
    block: [E] => DOT[E] ?=> EndomorphismMonoid[E, X] ?=> RESULT
  )(
    implicit exponential: DOT[X > X]
  ): RESULT =
    type E = X > X
    val nameOfId = id[X].name
    val reval: CTXT[(X > X, X)] => CTXT[X] = evaluation[X, X]
    def spiffyMul(c_fgx: CTXT[((E, E), X)]): CTXT[X] =
      val sniffle: CTXT[(E, X)] =
        c_fgx.map {
          case ((f: E, g: E), x: X) =>
            (f, x)
        }
      val c_fx: CTXT[X] = reval(sniffle)
      val c_g_fx: CTXT[(E, X)] =
        productMagic[E, X](
          c_fgx.map {
            case ((f: E, g: E), x: X) =>
              g
          },
          c_fx
        )
      val hiff: CTXT[(E, X)] =
        c_g_fx.map {
          case (g: E, fx: X) =>
            (g, fx)
        }
      val c_gfx: CTXT[X] = reval(hiff)
      c_gfx

    //    val myMul: CTXT[(E, E)] => CTXT[E] = { c =>
////      val f: CTXT[E] = c.map { _._1 }
////      val g: CTXT[E] = c.map { _._2 }
////  evaluation is a (X > Y, X) ~> Y
////  i.e. here a CTXT[(X > Y, X)] => CTXT[X]
//      c.map {
//        case (f: E, g: E) => f // not the answer
//      }
//    }
//    val myMul2: (E, E) ~> E = myMul
    val myMul2 = transpose(spiffyMul)
    implicit val _: EndomorphismMonoid[E, X] =
      new Monoid[E](
        unit = nameOfId,
        multiply = myMul2
      ) with EndomorphismMonoid[E, X] {
        override val standardAction: Action[X] =
          action[X] { (x_e: CTXT[(X, E)]) =>
            val e_x: CTXT[(E, X)] =
              x_e.map {
                case (x, e) => (e, x)
              }
            reval(e_x)
          }
      }
    block[X > X]
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