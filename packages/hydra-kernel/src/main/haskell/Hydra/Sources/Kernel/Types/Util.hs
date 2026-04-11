module Hydra.Sources.Kernel.Types.Util where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.util"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Core.ns] [Core.ns] $
    Just "General-purpose utility types used across Hydra."
  where
    -- Note: either_ and pair are NOT included here because they correspond to built-in
    -- type constructors (TypeEither, TypePair) which are handled natively by all target languages.
    -- Including them would cause conflicts with Haskell's Prelude.Either.
    -- Note: adapter, bicoder, and coder have been moved to hydra.coders.
    definitions = [
      caseConvention,
      comparison,
      precision]

caseConvention :: Binding
caseConvention = define "CaseConvention" $
  doc "A naming convention for symbols, such as camelCase or snake_case" $
  T.enum ["camel", "pascal", "lowerSnake", "upperSnake"]

comparison :: Binding
comparison = define "Comparison" $
  doc "An equality judgement: less than, equal to, or greater than" $
  T.enum [
    "lessThan",
    "equalTo",
    "greaterThan"]

either_ :: Binding
either_ = define "Either" $
  doc "A named union type equivalent to the built-in Either type constructor, for use in languages that lack anonymous sum types" $
  T.forAlls ["a", "b"] $ T.union [
    "left">:
      doc "The left alternative"
      (T.var "a"),
    "right">:
      doc "The right alternative"
      (T.var "b")]

pair :: Binding
pair = define "Pair" $
  doc "A named record type equivalent to the built-in Pair type constructor, for use in languages that lack anonymous product types" $
  T.forAlls ["a", "b"] $ T.record [
    "first">:
      doc "The first component"
      (T.var "a"),
    "second">:
      doc "The second component"
      (T.var "b")]

precision :: Binding
precision = define "Precision" $
  doc "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
  T.union [
    "arbitrary">:
      doc "Arbitrary precision" $
      T.unit,
    "bits">:
      doc "Precision to a specified number of bits" $
      T.int32]
