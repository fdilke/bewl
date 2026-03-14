package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.BaseTopos
import ⊕._

trait AlgebraicConstructions {
  Ɛ: BaseTopos with AlgebraicMachinery with AlgebraicStructures =>

  def endomorphismMonoid[T <: ~](
      dot: DOT[T]
  ) =
    new EndomorphismMonoid[T](dot)

  class EndomorphismMonoid[T <: ~](
      dot: DOT[T]
  ) {
    private implicit val endos =
      dot > dot

    val monoid =
      new Monoid[T → T](
        endos,
        dot.identity.name,
        BiArrow(endos x endos, endos.transpose(endos x endos) {
          case (f ⊕ g, x) => g(f(x))
        })
      )

    lazy val homeAction =
      monoid.action(
        dot
      ) { (a, m) => m(a) }
  }

  def groupOfUnits[T <: ~](
      monoid: Monoid[T]
  ): (Group[T], T > T) = {
    val carrier = monoid.carrier
    val pairs = carrier.squared
    val unit = monoid.unit
    val invertiblePairs = pairs where {
      case x ⊕ y =>
        def i = monoid.unit(carrier.toI(x))
        carrier.=?=(i, monoid.multiply(x, y)) ∧
          carrier.=?=(i, monoid.multiply(y, x))
    }
    val ip2carrier = pairs.π0 o invertiblePairs.inclusion
    val units = ip2carrier.chi.whereTrue
    val ip2units = units.restrict(ip2carrier)
    val units2ip = ip2units.inverse
    val mulUnits =
      BiArrow(
        units x units,
        units.restrict(
          (units x units)(units) {
            case u ⊕ v =>
              monoid.multiply(
                units.inclusion(u),
                units.inclusion(v)
              )
          }
        )
      )
    val inversion = units.restrict(pairs.π1 o invertiblePairs.inclusion o units2ip)
    (
      new Group[T](
        units,
        units.restrict(unit),
        mulUnits,
        inversion
      ),
      units.inclusion
    )
  }
}
