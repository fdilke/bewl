package com.fdilke.bewl2.categories

trait PreArrow[DOT]:
  val name: String
  val source: DOT
  val target: DOT

class FiniteCategory[DOT](
  dots: DOT*
)(
  arrows: PreArrow[DOT]*
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
