package com.fdilke.bewl.topos

trait AlgebraicConstructions { topos: BaseTopos with AlgebraicMachinery with AlgebraicStructures  =>
  def endomorphismMonoid[T <: ~](dot: DOT[T]) = {
    val endos = dot > dot
    Monoid(endos, dot.identity.name,
      BiArrow(endos x endos, endos.transpose(endos x endos) {
        case ((f, g), x) => g(f(x))
      })
    )
  }

  def groupOfUnits[T <: ~](monoid: Monoid[T]) = {
    val carrier = monoid.carrier
    val pairs = carrier.squared
    val unit = monoid.unit
    val invertiblePairs = pairs(omega) {
      case (x, y) =>
        def i = monoid.unit(carrier.toI(x))
        carrier.=?=(i, monoid.multiply(x, y)) ^
          carrier.=?=(i, monoid.multiply(y, x))
    }.whereTrue
    Group(
      invertiblePairs,
      invertiblePairs.restrict(unit x unit),
      BiArrow(invertiblePairs x invertiblePairs, invertiblePairs.restrict(
        (invertiblePairs x invertiblePairs)(pairs) {
          case ((p, q), (r, s)) => pairs.pair(monoid.multiply(p, r), monoid.multiply(s, q))
        }
      )),
      invertiblePairs.restrict(
        invertiblePairs(pairs) {
          case (p, q) => pairs.pair(q, p)
        }
      )
    )
  }
}
