package com.fdilke.bewl.topos

import com.fdilke.bewl.topos.algebra.{
  AlgebraicConstructions,
  AlgebraicMachinery,
  AlgebraicStructures,
  RelationalAlgebra
}
import com.fdilke.bewl.topos.constructions.{
  ConstructToposOfAutomorphisms,
  ConstructToposOfGroupActions,
  ConstructToposOfMonoidActions
}
import com.fdilke.bewl.topos.drivers.ImageFinder
import com.fdilke.bewl.topos.enrichment.{ElementEnrichments, LogicalOperations, MonadicPlumbing}
import com.fdilke.bewl.topos.monads.{ContinuationMonad, MonadOfMonoidActions}
import com.fdilke.bewl.topos.structures.{MonadCaching, Monads, StrongMonads}

trait ToposPrerequisites
  extends BaseToposDrivers
  with ToposAlgebra
  with ToposEnrichments
  with ToposStructures

trait ToposAlgebra
  extends AlgebraicMachinery
  with AlgebraicConstructions
  with AlgebraicStructures
  with RelationalAlgebra { Ɛ: ToposPrerequisites => }

trait ToposConstructions
  extends BaseTopos
  with ConstructToposOfMonoidActions
  with ConstructToposOfGroupActions
  with ConstructToposOfAutomorphisms { Ɛ: ToposPrerequisites => }

trait BaseToposDrivers extends BaseTopos with ImageFinder { Ɛ: ToposPrerequisites => }

trait ToposStructures extends Monads with StrongMonads with MonadCaching {
  Ɛ: BaseTopos with ToposEnrichments =>
}

trait ToposEnrichments extends LogicalOperations with ElementEnrichments with MonadicPlumbing {
  Ɛ: BaseTopos with ToposStructures with ToposAlgebra =>
}

trait MonadConstructions extends ContinuationMonad with MonadOfMonoidActions {
  Ɛ: BaseTopos with ToposStructures with ToposAlgebra =>
}

trait Topos[BASE]
  extends BaseTopos
  with Monads
  with MonadConstructions
  with ToposPrerequisites
  with ToposConstructions {
  override type ~ = BASE
}
