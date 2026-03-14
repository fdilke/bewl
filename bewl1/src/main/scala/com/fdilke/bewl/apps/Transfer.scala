package com.fdilke.bewl.apps

import com.fdilke.bewl.apps.permutations.Permutations
import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._

import scala.collection.mutable
import scala.language.postfixOps

object Transfer {
  def apply[P](
    group: Group[P],
    subgroup: DOT[P]
  ): P > P = {
    val elements = elementsOf(group.carrier)
    val subgroupElements = elementsOf(subgroup) toSeq

    val representatives = new mutable.ArrayBuffer[P]
    def findRepresentative(p: P): Option[P] =
      representatives.find { x =>
        subgroupElements.contains(
          group.multiply(
            p,
            group.inverse(x)
          )
        )
      }

    elements.foreach { p =>
      if (findRepresentative(p).isEmpty)
        representatives += p
    }
//    println("# representatives: " + representatives.size)
    group.carrier(subgroup) { p =>
      val coefficients = // Seq[P]()
        representatives.map { rep =>
          val product = group.multiply(rep, p)
          group.multiply(
            product,
            group.inverse(
              findRepresentative(product).get
            )
          )
        }
      coefficients.fold(
        group.unit(())
      )(
        group.multiply
      )
    }
  }
}

object UseTransfer extends App {
  val n = 6
  val symbols = dot(1 to n: _*)
  type PERMUTATION = Int → Int

  val group: Group[PERMUTATION] =
    Permutations.of(n)

  val the1 = group.unit(())

  def isInvolution(p: PERMUTATION): Boolean =
    (p != the1) && (group.multiply(p, p) == the1)

  def prettyPrint(map: Map[Int, Int]): String =
    (1 to n)
      .filter(i => map(i) != i)
      .map(i => i -> map(i))
      .map {
        _.toString
      }
      .mkString("")

  for {
    i <- elementsOf(group.carrier).filter(isInvolution)
  } {
    print("> " + prettyPrint(i))
    val subgroup = dot(the1, i)
    val transfer = Transfer(group, subgroup)
    println("\t => " + transfer.image.size)
  }
}
