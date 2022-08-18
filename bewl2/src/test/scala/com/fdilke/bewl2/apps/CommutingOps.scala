package com.fdilke.bewl2.apps

import com.fdilke.algo.Backtrack
import com.fdilke.algo.Backtrack.*
import com.fdilke.bewl2.apps.NQueens.{coordinates, lastOneAttacks, queensNode}
import com.fdilke.bewl2.sets.SetsUtilities.*

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.View

object CommutingOpsBruteForce extends App:
  private val order = 4
  private val quads: Seq[(Int, Int, Int, Int)] =
    allMaps(
      source = 0 until 4,
      target = 0 until order
    ).toSeq.map { (m: Map[Int, Int]) =>
      (m(0), m(1), m(2), m(3))
    }

  def selfCommutes(op: VarArgFunc[Int, Int]): Boolean =
    quads.forall { case (a, b, c, d) =>
      op(op(a, b), op(c, d)) ==
        op(op(a, c), op(b, d))
    }

  val ops: Iterable[VarArgFunc[Int, Int]] =
    allNaryOps(arity = 2, order = order)
  val count = ops.count(selfCommutes)

  println("count = " + count + " of " + ops.size + " ("
    + Math.round(100.0 * count / ops.size) + "%)"
  )

object CommutingOpsSubtle extends App:
  private val order = 2

  private val elements: Set[Int] = (0 until order).toSet
  private val pairs: Set[(Int, Int)] =
    for {
      i <- elements
      j <- elements
    } yield { (i, j) }

  def showTable(
     counter: AtomicInteger
  )(
     map: Map[(Int, Int), Int]
  ): Unit =
    counter.incrementAndGet()
    println("--+" + List.fill(order*2)('-').mkString)
    print("  |")
    for { i <- elements } {
      print(s"$i ")
    }
    println()
    println("-----------------")
    for { i <- elements } {
      print(s"$i |")
      for { j <- elements } {
        print(s"${map((i, j))} ")
      }
      println()
    }

  def bumpQuad(quad: ((Int, Int), (Int, Int))): Option[((Int, Int), (Int, Int))] =
    val ((a, b), (c, d)) = quad
    if (a < order - 1)
      Some(((a+1, b), (c, d)))
    else if (b < order - 1)
      Some(((0, b+1), (c, d)))
    else if (c < order - 1)
      Some(((0, 0), (c+1, d)))
    else if (d < order - 1)
      Some(((0, 0), (0, d+1)))
    else
      None

  /*@tailrec*/ def theNext(
    m0: Map[(Int, Int), Int],
    quad: ((Int, Int), (Int, Int))
  ): NextStep[(Int, Int), Int] =
//    println(s"quad: $quad")
    val ((a, b), (c, d)) = quad
    assuming(m0, (a, b)) { (ab, m1) =>
      assuming(m1, (c, d)) { (cd, m2) =>
        assuming(m2, (a, c)) { (ac, m3) =>
          assuming(m3, (b, d)) { (bd, m4) =>
            assuming(m4, (ab, cd)) { (ab_cd, m5) =>
              assuming(m5, (ac, bd)) { (ac_bd, m6) =>
                if (ab_cd != ac_bd)
                  MapInvalid.asInstanceOf[NextStep[(Int, Int), Int]]
                else bumpQuad(quad).match {
                  case None =>
                    MapComplete.asInstanceOf[NextStep[(Int, Int), Int]]
                  case Some(quad2) =>
                    theNext(m6, quad2)
                }
              }
            }
          }
        }
      }
    }

  val initialQuad = ((0, 0), (0, 0))
  val commutesNode: DecisionNode[(Int, Int), Int] =
    map => theNext(map, initialQuad)

  val counter = AtomicInteger(0)
  Backtrack.solve[(Int, Int), Int](
    elements,
    commutesNode
  ) foreach showTable(counter)
  println("total: " + counter.get)
