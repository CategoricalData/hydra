package hydra.haskell.language

import hydra.coders.*

import hydra.core.*

import hydra.variants.*

lazy val haskellLanguage: hydra.coders.Language = {
  lazy val eliminationVariants: scala.collection.immutable.Set[hydra.variants.EliminationVariant] = hydra.lib.sets.fromList[hydra.variants.EliminationVariant](Seq(hydra.variants.EliminationVariant.record,
     hydra.variants.EliminationVariant.union, hydra.variants.EliminationVariant.wrap))
  lazy val literalVariants: scala.collection.immutable.Set[hydra.variants.LiteralVariant] = hydra.lib.sets.fromList[hydra.variants.LiteralVariant](Seq(hydra.variants.LiteralVariant.binary,
     hydra.variants.LiteralVariant.boolean, hydra.variants.LiteralVariant.decimal,
     hydra.variants.LiteralVariant.float, hydra.variants.LiteralVariant.integer, hydra.variants.LiteralVariant.string))
  lazy val floatTypes: scala.collection.immutable.Set[hydra.core.FloatType] = hydra.lib.sets.fromList[hydra.core.FloatType](Seq(hydra.core.FloatType.float32,
     hydra.core.FloatType.float64))
  lazy val functionVariants: scala.collection.immutable.Set[hydra.variants.FunctionVariant] = hydra.lib.sets.fromList[hydra.variants.FunctionVariant](Seq(hydra.variants.FunctionVariant.elimination,
     hydra.variants.FunctionVariant.lambda))
  lazy val integerTypes: scala.collection.immutable.Set[hydra.core.IntegerType] = hydra.lib.sets.fromList[hydra.core.IntegerType](Seq(hydra.core.IntegerType.bigint,
     hydra.core.IntegerType.int8, hydra.core.IntegerType.int16, hydra.core.IntegerType.int32,
     hydra.core.IntegerType.int64))
  lazy val termVariants: scala.collection.immutable.Set[hydra.variants.TermVariant] = hydra.lib.sets.fromList[hydra.variants.TermVariant](Seq(hydra.variants.TermVariant.annotated,
     hydra.variants.TermVariant.application, hydra.variants.TermVariant.cases, hydra.variants.TermVariant.either,
     hydra.variants.TermVariant.lambda, hydra.variants.TermVariant.let, hydra.variants.TermVariant.list,
     hydra.variants.TermVariant.literal, hydra.variants.TermVariant.map, hydra.variants.TermVariant.maybe,
     hydra.variants.TermVariant.pair, hydra.variants.TermVariant.project, hydra.variants.TermVariant.record,
     hydra.variants.TermVariant.set, hydra.variants.TermVariant.typeApplication, hydra.variants.TermVariant.typeLambda,
     hydra.variants.TermVariant.inject, hydra.variants.TermVariant.unit, hydra.variants.TermVariant.unwrap,
     hydra.variants.TermVariant.variable, hydra.variants.TermVariant.wrap))
  lazy val typeVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](Seq(hydra.variants.TypeVariant.annotated,
     hydra.variants.TypeVariant.application, hydra.variants.TypeVariant.either, hydra.variants.TypeVariant.function,
     hydra.variants.TypeVariant.forall, hydra.variants.TypeVariant.list, hydra.variants.TypeVariant.literal,
     hydra.variants.TypeVariant.map, hydra.variants.TypeVariant.maybe, hydra.variants.TypeVariant.pair,
     hydra.variants.TypeVariant.record, hydra.variants.TypeVariant.set, hydra.variants.TypeVariant.union,
     hydra.variants.TypeVariant.unit, hydra.variants.TypeVariant.variable, hydra.variants.TypeVariant.void,
     hydra.variants.TypeVariant.wrap))
  def typePredicate[T0](_x: T0): Boolean = true
  hydra.coders.Language("hydra.haskell", hydra.coders.LanguageConstraints(eliminationVariants,
     literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants,
     typePredicate))
}

lazy val reservedWords: scala.collection.immutable.Set[scala.Predef.String] = {
  lazy val keywordSymbols: Seq[scala.Predef.String] = Seq("case", "class", "data",
     "default", "deriving", "do", "else", "forall", "foreign", "if", "import", "in",
     "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then",
     "type", "where")
  lazy val reservedSymbols: Seq[scala.Predef.String] = Seq("Bool", "Double", "False",
     "Float", "Int", "Integer", "Just", "Maybe", "Nothing", "Ord", "Show", "String",
     "True")
  hydra.lib.sets.fromList[scala.Predef.String](hydra.lib.lists.concat2[scala.Predef.String](keywordSymbols)(reservedSymbols))
}
