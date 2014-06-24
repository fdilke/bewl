package com.fdilke.bewl.fsets

// User: Felix Date: 07/06/2014 Time: 11:01

case class FunctionWithEquality[S, T](domain: Traversable[S], function: S => T)
  extends Function[S, T] {

  // Note we don't compare the domains. Equality semantics are guaranteed only for
  // objects of the same type i.e. same domain
  override def equals(that: scala.Any): Boolean = that match {
    case that: FunctionWithEquality[S, T] => domain.forall(s => function(s) == that.function(s))
    case _ => false
  }

  // This keeps the semantics right, and is efficient. No one should use these values as keys
  override def hashCode(): Int = 0

  // Could check membership of the domain, but we don't
  def apply(s: S): T = function(s)
}
