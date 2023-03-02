package com.fdilke.utility

object Mask:

  inline def mask[X, INPUT[_], OUTPUT](
    input: INPUT[X]
  )(
    block: [X_] => INPUT[X_] => (X_ =:= X) ?=> (X =:= X_) ?=> OUTPUT
  ): OUTPUT =
    block[X](input)
    
object MaskScratchpad:
  
  def mask2[X, RESULT[_]](
    block: [X_] => (X_ =:= X) ?=> (X =:= X_) ?=> RESULT[X_]
  ): RESULT[X] =
    block[X]

  def mask[X, RESULT](
    block: [X_] => (X_ =:= X) ?=> (X =:= X_) ?=> RESULT
  ): RESULT =
    block[X]


  def sillyMaskExperimental[X, INPUT[_], OUTPUT[_]](
    input: INPUT[X]
  )(
    block: [X_] => INPUT[X_] => (X_ =:= X) ?=> (X =:= X_) ?=> OUTPUT[X_]
  ): OUTPUT[X] =
    block[X](input)

  def sillyMaskSetDot[X, RESULT](
    dot: Set[X]
  )(
    block: [X_] => Set[X_] ?=> (X_ =:= X) ?=> (X =:= X_) ?=> RESULT
  ): RESULT =
    given Set[X] = dot
    block[X]
