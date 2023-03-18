package com.fdilke.bewl2.utility

trait Opacity[X]:
  opaque type theType = X
  inline def blur[T[_]](tx: T[X]):T[theType] = tx
  inline def focus[T[_]](tx: T[theType]):T[X] = tx

