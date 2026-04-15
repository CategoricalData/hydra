package hydra.languages

import hydra.coders.*

import hydra.core.*

lazy val hydraLanguage: hydra.coders.Language = {
  lazy val eliminationVariants: scala.collection.immutable.Set[hydra.variants.EliminationVariant] = hydra.lib.sets.fromList[hydra.variants.EliminationVariant](hydra.reflect.eliminationVariants)
  lazy val literalVariants: scala.collection.immutable.Set[hydra.variants.LiteralVariant] = hydra.lib.sets.fromList[hydra.variants.LiteralVariant](hydra.reflect.literalVariants)
  lazy val floatTypes: scala.collection.immutable.Set[hydra.core.FloatType] = hydra.lib.sets.fromList[hydra.core.FloatType](hydra.reflect.floatTypes)
  lazy val functionVariants: scala.collection.immutable.Set[hydra.variants.FunctionVariant] = hydra.lib.sets.fromList[hydra.variants.FunctionVariant](hydra.reflect.functionVariants)
  lazy val integerTypes: scala.collection.immutable.Set[hydra.core.IntegerType] = hydra.lib.sets.fromList[hydra.core.IntegerType](hydra.reflect.integerTypes)
  lazy val termVariants: scala.collection.immutable.Set[hydra.variants.TermVariant] = hydra.lib.sets.fromList[hydra.variants.TermVariant](hydra.reflect.termVariants)
  lazy val typeVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.reflect.typeVariants)
  def types(t: hydra.core.Type): Boolean =
    t match
    case _ => true
  hydra.coders.Language("hydra.core", hydra.coders.LanguageConstraints(eliminationVariants,
     literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants,
     types))
}
