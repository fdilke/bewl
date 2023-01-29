package com.fdilke.bewl2.sets

import scala.language.postfixOps
import com.fdilke.utility.Shortcuts.*
import Sets.*
import com.fdilke.utility.EnumValues

import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple}

object SetsUtilities:
  def allMaps[A, B](
     source: Iterable[A],
     target: Iterable[B]
  ): Iterable[Map[A, B]] =
    if (source.isEmpty)
      Iterable(Map.empty)
    else
      (for {
        partialMap <- allMaps(source.tail, target)
        choice <- target
      } yield {
        partialMap + (source.head -> choice)
      }).view

  trait VarArgFunc[-A, +B]:
    def apply(is: A*): B

  private def seqToMap[T](
    a: Seq[T]
  ): Map[Int, T] =
    Map(
      a.indices map { i =>
        i -> a(i)
      } :_*
    )

  def sequencesOfLength[H](
    letters: Seq[H],
    length: Int
  ): Iterable[Seq[H]] =
    if (letters.isEmpty || length == 0) then
      Iterable(Seq.empty)
    else for {
      seq <- sequencesOfLength(letters, length - 1)
      letter <- letters
    } yield {
      letter +: seq
    }

  def allNaryOps(
    arity: Int,
    order: Int
  ): Iterable[VarArgFunc[Int, Int]] =
    val toOrder: Seq[Int] = (0 until order)
    val toArity: Seq[Int] = (0 until arity)

    allMaps(
      allMaps(toArity, toOrder),
      toOrder
    ) map { (m: Map[Int, Int] => Int) =>
      (a: Seq[Int]) =>
        m(seqToMap(a))
    }

  def makeNullaryOperator[X: Sets.Dot](
    value: X
  ): Unit => X =
    _ => value

  def makeUnaryOperator[X: Sets.Dot](
    values: (X, X)*
  ): X => X =
    val map: Map[X, X] = Map[X, X](values: _*)
    if Sets.dot[X] != map.keySet then
      bail("incomplete or excessive unary operator definition")
    map

  def makeBinaryOperator[X: Sets.Dot](
    values: ((X, X), X)*
  ): ((X, X)) => X =
    val map: Map[(X, X), X] = Map[(X, X), X](values: _*)
    map

  inline def withEnum[ENUM]( // refactor when I have named type arguments
    block: Dot[ENUM] ?=> Unit
  ): Unit =
    withEnum[ENUM, Unit](block)

  inline def withEnum[ENUM, RESULT](
    block: Dot[ENUM] ?=> RESULT
  ): RESULT =
    withDot[ENUM, RESULT](
      EnumValues[ENUM].toSet
    ) {
      block
    }
