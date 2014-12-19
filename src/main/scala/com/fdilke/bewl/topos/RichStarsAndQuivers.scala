package com.fdilke.bewl.topos

trait RichStarsAndQuivers { topos: BaseTopos =>

  class RichQuiver[X <: ELEMENT, Y <: ELEMENT](quiver: QUIVER[X, Y]) {
    import quiver._

    def name = 
      (source > target).transpose(
        (I x source).biQuiver(target) {
          case (i, x) => quiver(x)
        })
    def x[Z <: ELEMENT](that: QUIVER[X, Z]): QUIVER[X, Y x Z] = {
      val product = target x that.target
      source(product) {
        s => product.pair(quiver(s), that(s))
      }}
  }

  implicit def enrich[X <: ELEMENT, Y <: ELEMENT](quiver: QUIVER[X, Y]) = new RichQuiver(quiver)

  // TODO: make this a trait always included, ∀ and ∃ lazy val
  class RichStar[X <: ELEMENT](star: STAR[X]) {
    import star._

    def ∀ = (truth o toI).name.chi

    def map(f: X => X): QUIVER[X, X] = star(star)(f)
    def flatMap(f2: X => QUIVER[X, X]): BiQuiver[X, X, X] =
      (star x star).biQuiver(star) {
          case (x, y) => f2(x)(y)
      }

    def ∃ = RichStarsAndQuivers.this.∃(star)
  }

  implicit def enrich[X <: ELEMENT](star: STAR[X]) = new RichStar(star)

//  private def ∃[X <: ELEMENT](star: STAR[X]) =
//    (star > omega)(omega) { f  =>
//      omega.∀ o (
//        omega(omega) { w =>
//          star.∀ o (
//            star(omega) { y =>
//              TruthObject.implies(f(y), w)
//            }
//          )
//        }
//      )
//    }

  private def ∃[X <: ELEMENT](star: STAR[X]) = {
    val p = star > omega
    val lhs = p.evaluation(leftProjection(p, omega, star), rightProjection(p, omega, star))
    val rhs = midProjection(p, omega, star)
    val fx_implies_w = TruthObject.implies(lhs, rhs)
    val b = BiQuiver(p x omega x star, fx_implies_w)
    val forall_x_fx_implies_w  = star.∀ o p.transpose(b)
    val bigImplies = TruthObject.implies(forall_x_fx_implies_w, (p x omega).π1)
    omega.∀ o (omega > omega).transpose(BiQuiver(p x omega, bigImplies))
  }

  // TODO: refactor using bindings
    // intention is this:
    //    (omega ^ x).lambda { f =>
    //      all(omega)(omega.lambda { w =>
    //        all(x)(x.lambda { y =>
    //        implies(omega.all(implies(f(y), w)), w)
    //      })
    //    })
    //      val p = omega ^ x
    //      val pox: DOT[((X => OMEGA, OMEGA), X)] = p x omega x x
    //
    //      val lhs: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = evaluation(x, omega)(leftProjection(p, omega, x), rightProjection(p, omega, x))
    //      val rhs: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = midProjection(p, omega, x)
    //      val fx_implies_w: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = implies(lhs x rhs)
    //      val forall_x_fx_implies_w : ARROW[(X => OMEGA, OMEGA), OMEGA] = all(x)(transpose(x, omega, BiArrow(p x omega, x, fx_implies_w)))
    //      val bigImplies: ARROW[(X => OMEGA, OMEGA), OMEGA] = implies(forall_x_fx_implies_w x rightProjection(p, omega))
    //      all(omega)(transpose(omega, omega, BiArrow(p, omega, bigImplies)))

  object TruthObject { // TODO: This should eventually express omega as a complete Heyting algebra
    lazy val omegaSquared = omega x omega
    lazy val and = BiQuiver(omegaSquared, (truth x truth).chi)
    private lazy val omegaId = omega.identity
    private lazy val diagonal = (omegaId x omegaId).chi     // TODO: factor out
    lazy val implies = BiQuiver(omegaSquared, diagonal o (and.quiver x ((omega x omega).π0)))
  }
}
