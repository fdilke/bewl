package com.fdilke.bewl2.cantorians

trait JonssonTarski[J] {
  def join(l: J, r: J): J
  def left(join: J): J
  def right(join: J): J
}

object JonssonTarski {
  @inline
  def apply[J](
    implicit tude: JonssonTarski[J]
  ): JonssonTarski[J] = tude

  @inline
  def join[J: JonssonTarski](l: J, r: J): J =
    JonssonTarski[J].join(l, r)

  @inline
  def left[J: JonssonTarski](join: J): J =
    JonssonTarski[J].left(join)

  @inline
  def right[J: JonssonTarski](join: J): J =
    JonssonTarski[J].right(join)
}
