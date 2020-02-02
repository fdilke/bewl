package com.fdilke.bewl2.topos

trait Topos[DOT[_]] {
  val name: String = getClass.getSimpleName

  def sanityTest[S: DOT]: Unit
  def sanityTest[S: DOT, T:DOT](arrow: S => T): Unit

  def compareFunctions[S:DOT, T:DOT](func: S=> T, func2: S => T): Boolean

  /// TODO: put these helper functions in a separate class

  // anticipate these will not be used very much...
  // as it's all baked into the types and thoughtcrime is impossible. remove?
  @inline final def dot[S:DOT]: DOT[S] =
    implicitly[DOT[S]]

  final def source[S:DOT, T:DOT](arrow: S => T): DOT[S] =
    dot[S]
  final def target[S:DOT, T:DOT](arrow: S => T): DOT[T] =
    dot[T]
  final def id[S: DOT]: S => S =
    identity
}
