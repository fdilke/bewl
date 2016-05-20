package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.topos.{ToposStructures, BaseTopos}

trait ContinuationMonad {
  Ɛ: BaseTopos with ToposStructures =>

  def continuationMonad[
    S <: ~
  ] (
    dot: DOT[S]
  ) =
    new StrongMonad[
      ({type λ[X <: ~] = X → S → S}) # λ
      ] {
      override def atUncached[X <: ~](
        dash: DOT[X]
      ): At[X] =
        new At[X] {
          private lazy val doubleExp: EXPONENTIAL[X → S, S] =
            dash > dot > dot

          override lazy val free: DOT[X → S → S] =
            doubleExp

          override lazy val eta =
            doubleExp.transpose(dash) {
              (x, f) => f(x)
            }

          override lazy val mu =
            (dash > dot > dot).transpose(
              dash > dot > dot > dot > dot
            ) {
              (ffff, f) => ffff(
                (dash > dot > dot > dot).transpose(
                  dash > dot
                ) {
                  (x, f) => f(x)
                }(
                  f
                )
              )
            }

          override def tensorialStrength[
          Y <: ~
          ](
            daa: DOT[Y]
          ) = (
            (dash x daa) > dot > dot
            ).transpose {
            dash x (daa > dot > dot)
          } {
            case (
              (x, yss),
              xys
              ) =>
              yss(
                (dot > daa(dash x daa) {
                    y => dash x daa pair(x, y)
                  }) (
                    xys
                  )
              )
          }
        }

      override def map[
      X <: ~,
      Y <: ~
      ](
        arrow: X > Y
      ) =
        dot > (dot > arrow)

      lazy val home: Algebra[S] = {
        val ddd: EXPONENTIAL[S → S, S] = dot > dot > dot
        val structure: (S → S → S) > S =
          ddd(dot) { f =>
            f(
              dot.identity.name(
                ddd.toI(f)
              )
            )
          }
        new Algebra[S](
          structure
        )
      }
    }
}
