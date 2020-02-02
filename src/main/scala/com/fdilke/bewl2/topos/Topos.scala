package com.fdilke.bewl2.topos

trait Topos[DOT[_]] {
  def sanityTest[S: DOT]: Unit
  def sanityTest[S: DOT, T:DOT](arrow: S => T): Unit

  // anticipate these will not be used very much...
  // as it's all baked into the types and thoughtcrime is impossible. remove?
  def source[S:DOT, T:DOT](arrow: S => T): DOT[S] =
    implicitly[DOT[S]]
  def target[S:DOT, T:DOT](arrow: S => T): DOT[T] =
    implicitly[DOT[T]]
}
