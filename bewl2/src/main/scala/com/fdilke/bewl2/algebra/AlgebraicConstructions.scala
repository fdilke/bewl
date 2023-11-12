package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.topos.ProductMappable
import com.fdilke.bewl2.topos.Topos
import com.fdilke.utility.Shortcuts.*
import com.fdilke.bewl2.sets.{Sets, BaseSets}

import scala.language.{dynamics, postfixOps}
import ProductMappable.*
import Sets.{ makeNullaryOperator, makeBinaryOperator }
import com.fdilke.bewl2.sets.SetsUtilities._
import Function.tupled

trait AlgebraicConstructions[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
]:
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  def withEndomorphismMonoid[X : Dot, RESULT](
    block: [E] => Dot[E] ?=> (monoid: Monoid[E]) ?=> monoid.Action[X] ?=> RESULT
  ): RESULT =
    type E = X > X

    def applyPair(c_fgx: CTXT[((E, E), X)]): CTXT[X] =
      val c_fx: CTXT[X] = applicate(c_fgx) {
          case ((f: E, g: E), x: X) =>
            (f, x)
        }
      val c_g_fx: CTXT[(E, X)] =
        c_fgx.map {
          case ((f: E, g: E), x: X) =>
            g
        } ⊕ c_fx
      applicate(c_g_fx) {
        case (g: E, fx: X) =>
          (g, fx)
      }

    implicit val monoid: Monoid[E] =
      new Monoid[E](
        unit = id[X].name,
        multiply = transpose(applyPair)
      )
    implicit val action: monoid.Action[X] =
      monoid.Action[X] { (x_e: CTXT[(X, E)]) =>
        applicate(x_e) {
          case (x, e) => (e, x)
        }
      }
    block[E]

  def withGroupOfUnits[M : Dot, RESULT](
    block: [G] => Dot[G] ?=> Group[G] ?=> (embed: G ~> M) => RESULT
  )(
    implicit monoid: Monoid[M]
  ): RESULT =
    val doubleProduct: (M, M) ~> (M, M) =
      c_mm =>
        monoid.multiply(c_mm) ⊕
        monoid.multiply(c_mm.map{ case (m, n) => (n, m) })
    val oneOne: (M, M) ~> (M, M) =
      val mm_to_1: (M, M) ~> M =
        monoid.unit o toUnit[(M, M)]
      (mm_to_1 x mm_to_1)
    doubleProduct.?=(oneOne) {
      [G] => (_: Dot[G]) ?=> (equalizer: Equalizer[G, (M, M)]) =>
        given Group[G] =
          new Group(
            unit = equalizer.restrict[UNIT](
              monoid.unit x monoid.unit
            ),
            multiply = equalizer.restrict[G, G] { (g, h) =>
              val c_m_m__n_n_ : CTXT[((M, M), (M, M))] =
                equalizer.inclusion(g) ⊕
                equalizer.inclusion(h)
                
              monoid.multiply(c_m_m__n_n_.map {
                case ((m, m_), (n, n_)) => (m, n)
              }) ⊕
              monoid.multiply(c_m_m__n_n_.map {
                case ((m, m_), (n, n_)) => (n_, m_)
              })
            },
            inverse = equalizer.restrict[G] { (g: CTXT[G]) =>
              equalizer.inclusion(g).map {
                case (m, n) => (n, m)
              }
            }
          )
        val embed: G ~> M =
          π0[M, M] o equalizer.inclusion
        block[G](embed)
    }

  final inline def withAutomorphismGroup[X : Dot, RESULT](
    block: [A] => Dot[A] ?=> (group: Group[A]) ?=> group.Action[X] ?=> RESULT
  ): RESULT =
    autoFinder.withAutomorphismGroup[X, RESULT](block)

  lazy val autoFinder: AutomorphismFinder =
    new DefaultAutomorphismFinder

  trait AutomorphismFinder:
    def withAutomorphismGroup[X : Dot, RESULT](
      block: [A] => Dot[A] ?=> (group: Group[A]) ?=> group.Action[X] ?=> RESULT
    ): RESULT

  class DefaultAutomorphismFinder extends AutomorphismFinder:
    override def withAutomorphismGroup[X : Dot, RESULT](
      block: [A] => Dot[A] ?=> (group: Group[A]) ?=> group.Action[X] ?=> RESULT
    ): RESULT =
      withEndomorphismMonoid[X, RESULT] {
        [E] => (_: Dot[E]) ?=> (monoid: Monoid[E]) ?=> (action: monoid.Action[X]) ?=>
        withGroupOfUnits[E, RESULT] {
          [A] => (_: Dot[A]) ?=> (group: Group[A]) ?=> (embed: A ~> E) =>
            // ??? : RESULT
            given group.Action[X] = group.Action[X] { case x ⊕ a =>
              action.actionMultiply(x, embed(a))
            }
            block[A]
          }
        }

// Constructions specific to Sets (and maybe other topoi) live here
object AlgebraicConstructions:

  def withCyclicGroup[R](
    order: Int
  )(
    block: [I] => Sets.Dot[I] ?=> I =:= Int ?=> Int =:= I ?=> Sets.Group[I] ?=> R
  ): R =
    Sets.withDotMask[Int, R](
      dot = 0 until order toSet
    ) {
      [I] => (_: Sets.Dot[I]) ?=> (_: I =:= Int) ?=> (_: Int =:= I) ?=>
        given Sets.Group[I] =
          new Sets.Group[I](
            makeNullaryOperator[I](0),
            tupled { (x, y) => (x + y) % order },
            { (i: I) => (order - i) % order }
          )
        block[I]
    }

  def withGroupFromTable[G, RESULT](
    table: G*
  )(
    block: Sets.Dot[G] ?=> Sets.Group[G] ?=> RESULT
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

    Sets.withDot(carrierAsList.toSet) {
      val binOp = makeBinaryOperator[G](mappings: _*)
      val theUnit: G = table.head
      given Sets.Group[G] =
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
    }

  def withSymmetricGroupNoAction[RESULT](
    degree: Int
  )(
    block: (seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> RESULT
  ): RESULT =
    val symbols: Seq[Int] = (0 until degree)
    Sets.withDot(
      symbols.permutations.toSet[Seq[Int]]
    ) {
      given Sets.Group[Seq[Int]] =
        Sets.Group[Seq[Int]](
          unit = makeNullaryOperator[Seq[Int]](symbols),
          multiply = { (p1: Seq[Int], p2: Seq[Int]) =>
            symbols map { s => p2(p1(s)) }
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
    }

  def withSymmetricGroup[RESULT](
    degree: Int
  )(
    block: (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => RESULT
  ): RESULT =
    withSymmetricGroupNoAction[RESULT](degree) { (_ : Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=>
      Sets.withDot(
        (0 until degree).toSet[Int]
      ) {
        block(
          group.Action[Int] { (i, seq) =>
            seq(i)
          }          
        )
      }
    }