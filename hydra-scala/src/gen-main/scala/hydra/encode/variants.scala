package hydra.encode.variants

import hydra.core.*

import hydra.variants.*

def eliminationVariant(v1: hydra.variants.EliminationVariant): hydra.core.Term =
  v1 match
  case hydra.variants.EliminationVariant.record => hydra.core.Term.union(hydra.core.Injection("hydra.variants.EliminationVariant",
     hydra.core.Field("record", hydra.core.Term.unit)))
  case hydra.variants.EliminationVariant.union => hydra.core.Term.union(hydra.core.Injection("hydra.variants.EliminationVariant",
     hydra.core.Field("union", hydra.core.Term.unit)))
  case hydra.variants.EliminationVariant.wrap => hydra.core.Term.union(hydra.core.Injection("hydra.variants.EliminationVariant",
     hydra.core.Field("wrap", hydra.core.Term.unit)))

def functionVariant(v1: hydra.variants.FunctionVariant): hydra.core.Term =
  v1 match
  case hydra.variants.FunctionVariant.elimination => hydra.core.Term.union(hydra.core.Injection("hydra.variants.FunctionVariant",
     hydra.core.Field("elimination", hydra.core.Term.unit)))
  case hydra.variants.FunctionVariant.lambda => hydra.core.Term.union(hydra.core.Injection("hydra.variants.FunctionVariant",
     hydra.core.Field("lambda", hydra.core.Term.unit)))
  case hydra.variants.FunctionVariant.primitive => hydra.core.Term.union(hydra.core.Injection("hydra.variants.FunctionVariant",
     hydra.core.Field("primitive", hydra.core.Term.unit)))

def literalVariant(v1: hydra.variants.LiteralVariant): hydra.core.Term =
  v1 match
  case hydra.variants.LiteralVariant.binary => hydra.core.Term.union(hydra.core.Injection("hydra.variants.LiteralVariant",
     hydra.core.Field("binary", hydra.core.Term.unit)))
  case hydra.variants.LiteralVariant.boolean => hydra.core.Term.union(hydra.core.Injection("hydra.variants.LiteralVariant",
     hydra.core.Field("boolean", hydra.core.Term.unit)))
  case hydra.variants.LiteralVariant.float => hydra.core.Term.union(hydra.core.Injection("hydra.variants.LiteralVariant",
     hydra.core.Field("float", hydra.core.Term.unit)))
  case hydra.variants.LiteralVariant.integer => hydra.core.Term.union(hydra.core.Injection("hydra.variants.LiteralVariant",
     hydra.core.Field("integer", hydra.core.Term.unit)))
  case hydra.variants.LiteralVariant.string => hydra.core.Term.union(hydra.core.Injection("hydra.variants.LiteralVariant",
     hydra.core.Field("string", hydra.core.Term.unit)))

def termVariant(v1: hydra.variants.TermVariant): hydra.core.Term =
  v1 match
  case hydra.variants.TermVariant.annotated => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("annotated", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.application => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("application", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.either => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("either", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.function => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("function", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.let => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("let", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.list => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("list", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.literal => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("literal", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.map => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("map", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.maybe => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("maybe", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.pair => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("pair", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.record => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("record", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.set => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("set", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.typeApplication => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("typeApplication", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.typeLambda => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("typeLambda", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.union => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("union", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.unit => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("unit", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.variable => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("variable", hydra.core.Term.unit)))
  case hydra.variants.TermVariant.wrap => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TermVariant",
     hydra.core.Field("wrap", hydra.core.Term.unit)))

def typeVariant(v1: hydra.variants.TypeVariant): hydra.core.Term =
  v1 match
  case hydra.variants.TypeVariant.annotated => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("annotated", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.application => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("application", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.either => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("either", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.forall => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("forall", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.function => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("function", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.list => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("list", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.literal => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("literal", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.map => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("map", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.maybe => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("maybe", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.pair => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("pair", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.record => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("record", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.set => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("set", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.union => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("union", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.unit => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("unit", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.variable => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("variable", hydra.core.Term.unit)))
  case hydra.variants.TypeVariant.wrap => hydra.core.Term.union(hydra.core.Injection("hydra.variants.TypeVariant",
     hydra.core.Field("wrap", hydra.core.Term.unit)))
