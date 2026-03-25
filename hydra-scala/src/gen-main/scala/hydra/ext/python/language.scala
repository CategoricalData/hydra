package hydra.ext.python.language

import hydra.coders.*

import hydra.core.*

import hydra.variants.*

import hydra.lib.lists

import hydra.lib.sets

lazy val pythonLanguage: hydra.coders.Language = {
  lazy val eliminationVariants: scala.collection.immutable.Set[hydra.variants.EliminationVariant] = hydra.lib.sets.fromList[hydra.variants.EliminationVariant](Seq(hydra.variants.EliminationVariant.record,
     hydra.variants.EliminationVariant.union, hydra.variants.EliminationVariant.wrap))
  lazy val literalVariants: scala.collection.immutable.Set[hydra.variants.LiteralVariant] = hydra.lib.sets.fromList[hydra.variants.LiteralVariant](Seq(hydra.variants.LiteralVariant.binary,
     hydra.variants.LiteralVariant.boolean, hydra.variants.LiteralVariant.float, hydra.variants.LiteralVariant.integer,
     hydra.variants.LiteralVariant.string))
  lazy val floatTypes: scala.collection.immutable.Set[hydra.core.FloatType] = hydra.lib.sets.fromList[hydra.core.FloatType](Seq(hydra.core.FloatType.bigfloat,
     hydra.core.FloatType.float64))
  lazy val functionVariants: scala.collection.immutable.Set[hydra.variants.FunctionVariant] = hydra.lib.sets.fromList[hydra.variants.FunctionVariant](Seq(hydra.variants.FunctionVariant.elimination,
     hydra.variants.FunctionVariant.lambda, hydra.variants.FunctionVariant.primitive))
  lazy val integerTypes: scala.collection.immutable.Set[hydra.core.IntegerType] = hydra.lib.sets.fromList[hydra.core.IntegerType](Seq(hydra.core.IntegerType.bigint))
  lazy val termVariants: scala.collection.immutable.Set[hydra.variants.TermVariant] = hydra.lib.sets.fromList[hydra.variants.TermVariant](Seq(hydra.variants.TermVariant.annotated,
     hydra.variants.TermVariant.application, hydra.variants.TermVariant.either, hydra.variants.TermVariant.function,
     hydra.variants.TermVariant.let, hydra.variants.TermVariant.list, hydra.variants.TermVariant.literal,
     hydra.variants.TermVariant.map, hydra.variants.TermVariant.maybe, hydra.variants.TermVariant.pair,
     hydra.variants.TermVariant.record, hydra.variants.TermVariant.set, hydra.variants.TermVariant.typeApplication,
     hydra.variants.TermVariant.typeLambda, hydra.variants.TermVariant.union, hydra.variants.TermVariant.unit,
     hydra.variants.TermVariant.variable, hydra.variants.TermVariant.wrap))
  lazy val typeVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](Seq(hydra.variants.TypeVariant.annotated,
     hydra.variants.TypeVariant.application, hydra.variants.TypeVariant.either, hydra.variants.TypeVariant.function,
     hydra.variants.TypeVariant.forall, hydra.variants.TypeVariant.list, hydra.variants.TypeVariant.literal,
     hydra.variants.TypeVariant.map, hydra.variants.TypeVariant.maybe, hydra.variants.TypeVariant.pair,
     hydra.variants.TypeVariant.record, hydra.variants.TypeVariant.set, hydra.variants.TypeVariant.union,
     hydra.variants.TypeVariant.unit, hydra.variants.TypeVariant.variable, hydra.variants.TypeVariant.void,
     hydra.variants.TypeVariant.wrap))
  def typePredicate[T0](_x: T0): Boolean = true
  hydra.coders.Language("hydra.ext.python", hydra.coders.LanguageConstraints(eliminationVariants, literalVariants,
     floatTypes, functionVariants, integerTypes, termVariants, typeVariants, typePredicate))
}

lazy val pythonReservedWords: scala.collection.immutable.Set[scala.Predef.String] = {
  lazy val pythonKeywords: Seq[scala.Predef.String] = Seq("False", "None", "True", "and", "as", "assert",
     "async", "await", "break", "class", "continue", "def", "del", "elif", "else", "except", "finally",
     "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise",
     "return", "try", "while", "with", "yield")
  lazy val pythonBuiltInFunctions: Seq[scala.Predef.String] = Seq("range")
  lazy val hydraPythonKeywords: Seq[scala.Predef.String] = Seq("Node", "FrozenDict")
  hydra.lib.sets.fromList[scala.Predef.String](hydra.lib.lists.concat[scala.Predef.String](Seq(pythonKeywords, pythonBuiltInFunctions, hydraPythonKeywords)))
}
