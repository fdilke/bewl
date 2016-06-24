package com.fdilke.bewl.helper

object Shortcuts {
  def bail(message: String) =
    throw new IllegalArgumentException(
      message
    )
}
