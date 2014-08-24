package com.fdilke.bewl.algebra

import com.fdilke.bewl.BaseTopos

trait TruthObject { topos: BaseTopos with Algebra with AlgebraicLaws with AlgebraicStructures =>
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

  def forAll[X](dot: DOT[X]): ARROW[X => OMEGA, OMEGA] = {
    truth(dot.toI).name.chi.arrow
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
    
    val theTrue = truth
    val and = (truth x truth).chi.arrow
    val omegaId = omega.identity
    val diagonal = (omegaId x omegaId).chi.arrow
    val implies = diagonal(and x projection(omega, 2, 0))
    // TODO rewrite in terms of bound operators?
    // TODO: use 'tupled' somewhere here

    val or = quantifyA(omega, omega)(omega)(and, implies) { // TODO: use infix here
      case (Seq(and, implies), Seq(a, b, w)) => implies(and(implies(a,w), implies(b, w)), w)
    }
    val theFalse = quantifyA()(omega)() {
      case (Seq(), Seq(w)) => w
    }

    HeytingAlgebra(omega,
      theFalse,
      theTrue,
      and,
      or,
      implies)
  }
*/
  def omegaHeyting = null
}
