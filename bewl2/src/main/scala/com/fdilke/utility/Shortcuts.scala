package com.fdilke.utility

object Shortcuts:
  def bail(message: String) =
    throw new IllegalArgumentException(
      message
    )
