package com.fdilke.bewl2.topos

trait Monad[CTXT[_]] {
  def eta[A](a : A) : CTXT[A]
  def mu[A](elem : CTXT[CTXT[A]]): CTXT[A]
  def map[A, B](f: A => B): CTXT[A] => CTXT[B]
}

object Monad {
  type IDENTITY = [X] =>> X

  implicit object IdentityMonad extends Monad[IDENTITY] {
    override inline def eta[A](
      a: A
    ): IDENTITY[A] = a

    override inline def mu[A](
      a: IDENTITY[IDENTITY[A]]
    ): A = a

    override inline def map[A, B](
      f: A => B
    ): IDENTITY[A] => IDENTITY[B] =
      f
  }
}