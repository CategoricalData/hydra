module Hydra.Sources.Kernel.Types.Variants where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (eliminationVariant, functionVariant, literalVariant, termVariant, typeVariant)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.variants"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Core.ns] [Core.ns] $
    Just "Variant types which describe the structure of Hydra core types and terms."
  where
    definitions = [
      eliminationVariant,
      functionVariant,
      literalVariant,
      termVariant,
      typeVariant]

-- Note: kept around for backward compatibility with the Hydra.Coders LanguageConstraints schema,
-- even though hydra.core.Function and hydra.core.Elimination no longer exist as #332 of 2026-04.
eliminationVariant :: Binding
eliminationVariant = define "EliminationVariant" $
  doc "The identifier of an elimination constructor (legacy)" $
  T.enum [
    "record",
    "union",
    "wrap"]

-- Note: kept around for backward compatibility with the Hydra.Coders LanguageConstraints schema.
functionVariant :: Binding
functionVariant = define "FunctionVariant" $
  doc "The identifier of a function constructor (legacy)" $
  T.enum [
    "elimination",
    "lambda"]

literalVariant :: Binding
literalVariant = define "LiteralVariant" $
  doc "The identifier of a literal constructor" $
  T.enum [
    "binary",
    "boolean",
    "float",
    "integer",
    "string"]

termVariant :: Binding
termVariant = define "TermVariant" $
  doc "The identifier of a term expression constructor" $
  T.enum [
    "annotated",
    "application",
    "cases",
    "either",
    "inject",
    "lambda",
    "let",
    "list",
    "literal",
    "map",
    "maybe",
    "pair",
    "project",
    "record",
    "set",
    "typeApplication",
    "typeLambda",
    "unit",
    "unwrap",
    "variable",
    "wrap"]

typeVariant :: Binding
typeVariant = define "TypeVariant" $
  doc "The identifier of a type constructor" $
  T.enum [
    "annotated",
    "application",
    "either",
    "forall",
    "function",
    "list",
    "literal",
    "map",
    "maybe",
    "pair",
    "record",
    "set",
    "union",
    "unit",
    "variable",
    "void",
    "wrap"]
