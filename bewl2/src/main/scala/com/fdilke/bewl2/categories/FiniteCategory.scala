package com.fdilke.bewl2.categories

trait Arrow[DOT]
class FiniteCategory[DOT](
  dots: DOT*
)(
  arrows: Arrow[DOT]*
):
  def sanityTest(): Unit =
    ()  
