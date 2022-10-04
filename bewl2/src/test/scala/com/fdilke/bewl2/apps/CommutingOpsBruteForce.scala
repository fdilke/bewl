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

