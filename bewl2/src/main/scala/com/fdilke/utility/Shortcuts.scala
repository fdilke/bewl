package com.fdilke.utility

object Shortcuts:
  def bail(message: String) =
    throw new IllegalArgumentException(message)

  inline def transmute[P, Q](p: P)(implicit
    pToQ: P =:= Q
  ): Q =
    pToQ.substituteCo[[X] =>> X](p)

  inline def transmute[P, Q, R](p: P)(using
    P =:= Q,
    Q =:= R
  ): R =
    transmute[Q, R](transmute[P, Q](p))

  extension(letters: Seq[Char])
    inline def string: String =
      new String(letters.toArray)

  def intSqrt(square: Int) =
    (1 to square).find(n => n * n == square).getOrElse {
      throw new IllegalArgumentException("Not a valid monoid multiplication table: size " + square)
    }
