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
module_ = Module ns elements [] [Core.module_] $
    Just "General-purpose utility types used across Hydra."
  where
    elements = [
      caseConvention,
      comparison,
      decodingError,
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

decodingError :: Binding
decodingError = define "DecodingError" $
  doc "An error that occurred during decoding of a term" $
  T.wrap T.string

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
