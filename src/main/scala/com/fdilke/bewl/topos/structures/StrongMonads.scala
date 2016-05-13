package com.fdilke.bewl.topos.structures

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.{ToposStructures, BaseTopos}
import org.scalatest.Matchers._

import scala.language.{higherKinds, reflectiveCalls}

trait StrongMonads {
  Ɛ: BaseTopos with ToposStructures =>

  trait StrongMonad[
    M[X <: ~] <: ~
  ] extends Monad[M]
}
