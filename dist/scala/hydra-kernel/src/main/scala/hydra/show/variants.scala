package hydra.show.variants

import hydra.variants.*

def termVariant(v1: hydra.variants.TermVariant): scala.Predef.String =
  v1 match
  case hydra.variants.TermVariant.annotated => "annotated"
  case hydra.variants.TermVariant.application => "application"
  case hydra.variants.TermVariant.cases => "cases"
  case hydra.variants.TermVariant.either => "either"
  case hydra.variants.TermVariant.lambda => "lambda"
  case hydra.variants.TermVariant.let => "let"
  case hydra.variants.TermVariant.list => "list"
  case hydra.variants.TermVariant.literal => "literal"
  case hydra.variants.TermVariant.map => "map"
  case hydra.variants.TermVariant.maybe => "maybe"
  case hydra.variants.TermVariant.pair => "pair"
  case hydra.variants.TermVariant.project => "project"
  case hydra.variants.TermVariant.record => "record"
  case hydra.variants.TermVariant.set => "set"
  case hydra.variants.TermVariant.typeLambda => "typeLambda"
  case hydra.variants.TermVariant.typeApplication => "typeApplication"
  case hydra.variants.TermVariant.union => "union"
  case hydra.variants.TermVariant.unit => "unit"
  case hydra.variants.TermVariant.unwrap => "unwrap"
  case hydra.variants.TermVariant.variable => "variable"
  case hydra.variants.TermVariant.wrap => "wrap"

def typeVariant(v1: hydra.variants.TypeVariant): scala.Predef.String =
  v1 match
  case hydra.variants.TypeVariant.annotated => "annotated"
  case hydra.variants.TypeVariant.application => "application"
  case hydra.variants.TypeVariant.either => "either"
  case hydra.variants.TypeVariant.forall => "forall"
  case hydra.variants.TypeVariant.function => "function"
  case hydra.variants.TypeVariant.list => "list"
  case hydra.variants.TypeVariant.literal => "literal"
  case hydra.variants.TypeVariant.map => "map"
  case hydra.variants.TypeVariant.maybe => "maybe"
  case hydra.variants.TypeVariant.pair => "pair"
  case hydra.variants.TypeVariant.record => "record"
  case hydra.variants.TypeVariant.set => "set"
  case hydra.variants.TypeVariant.union => "union"
  case hydra.variants.TypeVariant.unit => "unit"
  case hydra.variants.TypeVariant.variable => "variable"
  case hydra.variants.TypeVariant.void => "void"
  case hydra.variants.TypeVariant.wrap => "wrap"
