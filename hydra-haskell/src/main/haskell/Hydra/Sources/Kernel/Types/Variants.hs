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
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just "Variant types which describe the structure of Hydra core types and terms."
  where
    elements = [
      eliminationVariant,
      functionVariant,
      literalVariant,
      termVariant,
      typeVariant]

eliminationVariant :: Binding
eliminationVariant = define "EliminationVariant" $
  doc "The identifier of an elimination constructor" $
  T.enum [
    "record",
    "union",
    "wrap"]

functionVariant :: Binding
functionVariant = define "FunctionVariant" $
  doc "The identifier of a function constructor" $
  T.enum [
    "elimination",
    "lambda",
    "primitive"]

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
    "either",
    "function",
    "let",
    "list",
    "literal",
    "map",
    "maybe",
    "pair",
    "record",
    "set",
    "typeApplication",
    "typeLambda",
    "union",
    "unit",
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
    "wrap"]
