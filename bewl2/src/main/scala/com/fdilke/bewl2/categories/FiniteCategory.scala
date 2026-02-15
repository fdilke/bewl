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
  for { arrow <- arrows }
    if (!((dots.contains(arrow.source)) && (dots.contains(arrow.target)))) {
      throw new IllegalArgumentException("source/target not listed")
    }

object FiniteCategory:
  case class SimplePreArrow[DOT](
    name: String,
    source: DOT,
    target: DOT                            
  ) extends PreArrow[DOT]
