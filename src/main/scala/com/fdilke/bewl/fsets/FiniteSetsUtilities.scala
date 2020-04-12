package com.fdilke.bewl.fsets

object FiniteSetsUtilities {
  import FiniteSets._

  def dot[T](elements: T*) =
    makeDot(elements)

  def arrow[S, T](
    source: DOT[S],
    target: DOT[T]
  )(
    map: (S, T)*
  ) =
    functionAsArrow(
      source,
      target,
      Map(map: _*)
    )

  def biArrow[L, R, T](
    left: DOT[L],
    right: DOT[R],
    target: DOT[T]
  )(
    mappings: ((L, R), T)*
  ) =
    bifunctionAsBiArrow[L, R, T](
      left,
      right,
      target
    )(
      Function.untupled(
        Map(
          mappings: _*
        )
      )
    )

  def elementsOf[X](
    dot: DOT[X]
  ): Iterable[X] =
    dot.globals.map {
      _(())
    }

  def asElement[X, Y](
    arrow: X > Y
  ): X → Y = {
    val xx: UNIT > (X → Y) = arrow.name
    arrow.name(())
  }

  def makeNullaryOperator[X](
    carrier: DOT[X],
    value: X
  ) =
    functionAsArrow(
      I,
      carrier,
      (_: UNIT) => value
    )

  def makeUnaryOperator[X](
    carrier: DOT[X],
    mappings: (X, X)*
  ) =
    functionAsArrow(
      carrier,
      carrier,
      Map(mappings: _*)
    )

  def makeBinaryOperator[X](
    carrier: DOT[X],
    mappings: ((X, X), X)*
  ) =
    bifunctionAsBiArrow[X](
      carrier
    )(
      Function.untupled(
        Map(
          mappings: _*
        )
      )
    )

  def doubleCharacteristic[T](
    set: DOT[T]
  )(
    subsets: Set[T]*
  ): (T → TRUTH) > TRUTH = {
    val incl =
      bifunctionAsBiArrow(
        dot(subsets: _*),
        set,
        omega
      ) {
        _ contains _
      }
    set.power.transpose(incl).chi
  }

  private def intSqrt(square: Int) =
    (1 to square).find(n => n * n == square).getOrElse {
      throw new IllegalArgumentException("Not a valid monoid multiplication table: size " + square)
    }

  def monoidFromTable[M](table: M*): Monoid[M] = {
    val carrierSize = intSqrt(table.size)
    val carrierAsList = table.take(carrierSize)
    val carrier = dot(carrierAsList: _*)
    val mappings =
      for {
        i <- 0 until carrierSize
        j <- 0 until carrierSize
      } yield (
        carrierAsList(i),
        carrierAsList(j)
      ) -> table(
        i * carrierSize + j
      )
    val product =
      makeBinaryOperator(
        carrier,
        mappings: _*
      )
    new Monoid[M](
      carrier,
      makeNullaryOperator(
        carrier,
        carrierAsList.head
      ),
      product
    )
  }

  def allMaps[A, B](
    source: Iterable[A],
    target: Iterable[B]
  ): Iterable[Map[A, B]] =
    if (source.isEmpty)
      Iterable(Map.empty)
    else
      for {
        f <- allMaps(source.tail, target)
        choice <- target
      } yield {
        f + (source.head -> choice)
      }

  def relationFrom[X, Y](
    source: DOT[X],
    target: DOT[Y],
    identifications: (X, Y)*
  ): FiniteSets.Relation[X, Y] =
    (source.x(target)).relation((p, q) => identifications contains p -> q)

  def relationFrom[X](
    carrier: DOT[X],
    identifications: (X, X)*
  ): FiniteSets.Relation[X, X] =
    relationFrom(
      carrier,
      carrier,
      identifications: _*
    )
}
