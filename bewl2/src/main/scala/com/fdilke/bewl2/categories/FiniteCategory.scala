package com.fdilke.bewl2.categories

import scala.language.implicitConversions

trait PreArrow[+DOT]:
  val name: String
  val source: DOT
  val target: DOT

type Chain[DOT] = Seq[PreArrow[DOT]]

implicit def toChain[DOT](preArrow: PreArrow[DOT]): Chain[DOT] =
  Seq(preArrow)

extension[DOT](chain: Chain[DOT])
  infix def o(preArrow: PreArrow[DOT]): Chain[DOT] =
    chain :+ preArrow
  def :=(rhs: Chain[DOT]): CompositionLaw[DOT] =
    CompositionLaw(lhs = chain, rhs = rhs)

case class CompositionLaw[DOT](
  lhs: Chain[DOT],
  rhs: Chain[DOT]
)

object CompositionLaw:
  val ONE: Seq[PreArrow[Nothing]] = Seq.empty

class FiniteCategory[DOT](
  dots: DOT*
)(
  arrows: PreArrow[DOT]*
)(
  laws: CompositionLaw[DOT]*
):
  def checkChain(chain: Chain[DOT]): Unit =
    for { i <- chain.indices}
      if (!arrows.contains(chain(i)))
        throw new IllegalArgumentException("unknown arrow in chain")
//      if (i > 0 && chain(i - 1).source != chain(i).target)
//        throw new IllegalArgumentException("bad chain")
  for { arrow <- arrows }
    if (!((dots.contains(arrow.source)) && (dots.contains(arrow.target)))) {
      throw new IllegalArgumentException("source/target not listed")
    }
  for { law <- laws }
    { checkChain(law.lhs) ; checkChain(law.rhs) }

object FiniteCategory:
  case class SimplePreArrow[DOT](
    name: String,
    source: DOT,
    target: DOT                            
  ) extends PreArrow[DOT]
