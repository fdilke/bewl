package com.fdilke.bewl2.apps

import com.fdilke.algo.Backtrack
import com.fdilke.algo.Backtrack.{DecisionNode, MapComplete, MapInvalid, NextStep, assuming}

import java.util.concurrent.atomic.AtomicInteger

object CommutingOpsSubtle extends App:
  private val order = 5

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

  /*@tailrec*/
  def theNext(
    m0: Map[(Int, Int), Int],
    quad: ((Int, Int), (Int, Int))
  ): NextStep[(Int, Int), Int] =
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

