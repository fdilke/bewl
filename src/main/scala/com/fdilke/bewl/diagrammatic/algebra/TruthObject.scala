package com.fdilke.bewl.diagrammatic.algebra

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos

trait TruthObject { topos: BaseDiagrammaticTopos with Algebra with AlgebraicLaws with AlgebraicStructures =>

  // TODO: do thee need to be lazy? why?
  private lazy val and: ARROW[(OMEGA, OMEGA), OMEGA] = (truth x truth).chi.arrow
  private lazy val omegaId = omega.identity
  private lazy val diagonal = (omegaId x omegaId).chi.arrow     // TODO: factor out
  private lazy val po20: ARROW[(OMEGA, OMEGA), OMEGA] = projection(omega, 2, 0).asInstanceOf[ARROW[(OMEGA, OMEGA), OMEGA]]
  private lazy val implies = diagonal(and x po20)

  // TODO: a way to have these as methods on DOT?
  def all[X](x: DOT[X]): ARROW[X => OMEGA, OMEGA] = truth(x.toI).name.chi.arrow
  def exists[X](x: DOT[X]): ARROW[X => OMEGA, OMEGA] = {
    // TODO: refactor using bindings
    // intention is this:
//    (omega ^ x).lambda { f =>
//      all(omega)(omega.lambda { w =>
//        all(x)(x.lambda { y =>
//        implies(omega.all(implies(f(y), w)), w)
//      })
//    })
    val p = omega ^ x
    val pox: DOT[((X => OMEGA, OMEGA), X)] = p x omega x x

    val lhs: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = evaluation(x, omega)(leftProjection(p, omega, x), rightProjection(p, omega, x))
    val rhs: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = midProjection(p, omega, x)
    val fx_implies_w: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = implies(lhs x rhs)
    val forall_x_fx_implies_w : ARROW[(X => OMEGA, OMEGA), OMEGA] = all(x)(transpose(x, omega, BiArrow(p x omega, x, fx_implies_w)))
    val bigImplies: ARROW[(X => OMEGA, OMEGA), OMEGA] = implies(forall_x_fx_implies_w x rightProjection(p, omega))
    all(omega)(transpose(omega, omega, BiArrow(p, omega, bigImplies)))
  }

  /*
  // TODO move somewhere else, test separately. Note IntegerPower.multiply() might as well use this

  def doBigProduct[S](source: DOT[S], arrows: ARROW[S, _]*): ARROW[S, _] =
    (arrows size match {
      case 0 => source.toI
      case 1 => arrows(0)
      case _ => arrows.head x doBigProduct(source, arrows.tail: _*)
    })

  case class MixedProduct[P](product: DOT[P], projection: Seq[ARROW[P, _]])

//  object Weasel {
//    def unapply[X](seqx: Seq[DOT[X]])
//  }

  object MixedProduct {
    def of(components: Seq[DOT[_]]): MixedProduct[_] =
      components match {
        case Seq() => MixedProduct[TERMINAL](I, Seq.empty)
        case Seq(x: DOT[_]) => MixedProduct(x, Seq(x.identity))
        case _ =>
          computeMixedProduct(components.head, MixedProduct.of(components.tail))
      }

    private def computeMixedProduct[H, T](head: DOT[H], tailProduct: MixedProduct[T]) = {
      val biproduct: BIPRODUCT[H, T] = head * tailProduct.product

      MixedProduct(biproduct.product,
        biproduct.leftProjection +: tailProduct.projection.map(_(biproduct.rightProjection))
      )
    }
  }

  def bind(domains: DOT[_]*)(ops: Operator[_]*)(f: (Seq[BoundAlgebraicOperator[_, _]], Seq[ARROW[_, _]]) => ARROW[_,_]) = {
    def doBind[P](product: MixedProduct[P]) = {
      val boundOps: Seq[BoundAlgebraicOperator[P, _]] = ops map { op => BoundAlgebraicOperator(product.product, op)}
      f(boundOps, product.projection)
    }

    doBind(MixedProduct.of(domains))
  }

  def quantifyA[S](freeVariables: DOT[_]*)(boundVariables: DOT[_]*)(ops: Operator[_]*)(f: (Seq[BoundAlgebraicOperator[S, _]], Seq[ARROW[S, _]]) => ARROW[S,_]) = {
    val freeXboundToOmega = bind((freeVariables ++ boundVariables):_*)(ops :_*)(f)

    val productFree = MixedProduct.of(freeVariables)
    val productBound = MixedProduct.of(boundVariables)
    val productAll = MixedProduct.of(freeVariables ++ boundVariables)
    val valLeftProjection  = doBigProduct(productAll.product, productAll.projection.take(freeVariables.size):_*)
    val valRightProjection = doBigProduct(productAll.product, productAll.projection.drop(freeVariables.size):_*)

    val productFreeXBound = productFree.product * productBound.product
    val identifyProducts = productFree.projection.map(_(productFreeXBound.leftProjection)) ++ productBound.projection.map(_(productFreeXBound.rightProjection))

    val freeToBoundToOmega = transpose(productBound.product, omega, BiArrow(productFree.product, productBound.product, freeXboundToOmega(identifyProducts)))

    return forAll(productBound.product)(freeToBoundToOmega)
  }

  def omegaHeyting = {
    
    // TODO rewrite in terms of bound operators?
    // TODO: use 'tupled' somewhere here

    val or = quantifyA(omega, omega)(omega)(and, implies) { // TODO: use infix here
      case (Seq(and, implies), Seq(a, b, w)) => implies(and(implies(a,w), implies(b, w)), w)
    }
    val falsity = quantifyA()(omega)() {
      case (Seq(), Seq(w)) => w
    }

    HeytingAlgebra(omega,
      falsity,
      truth,
      and,
      or,
      implies)
  }
*/
  def omegaHeyting = null
}
