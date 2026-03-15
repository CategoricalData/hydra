module Hydra.Sources.Kernel.Types.Util where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Context as Context
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Error as Error


ns :: Namespace
ns = Namespace "hydra.util"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Context.ns, Core.ns, Error.ns] [Context.ns, Core.ns, Error.ns] $
    Just "General-purpose utility types used across Hydra."
  where
    elements = [
      adapter,
      bicoder,
      caseConvention,
      coder,
      comparison,
      precision]

adapter :: Binding
adapter = define "Adapter" $
  doc "A two-level bidirectional encoder which adapts types to types and terms to terms" $
  T.forAlls ["t1", "t2", "v1", "v2"] $ T.record [
    "isLossy">:
      doc "Whether information may be lost in the course of this adaptation"
      T.boolean,
    "source">:
      doc "The source type"
      "t1",
    "target">:
      doc "The target type"
      "t2",
    "coder">:
      doc "The coder for transforming instances of the source type to instances of the target type" $
      coder @@ "v1" @@ "v2"]

bicoder :: Binding
bicoder = define "Bicoder" $
  doc "A two-level encoder and decoder, operating both at a type level and an instance (data) level" $
  T.forAlls ["t1", "t2", "v1", "v2"] $ T.record [
    "encode">:
      doc "A function from source types to adapters" $
      "t1" ~> adapter @@ "t1" @@ "t2" @@ "v1" @@ "v2",
    "decode">:
      doc "A function from target types to adapters" $
      "t2" ~> adapter @@ "t2" @@ "t1" @@ "v2" @@ "v1"]

coder :: Binding
coder = define "Coder" $
  doc "An encoder and decoder; a bidirectional transformation between two types" $
  T.forAlls ["v1", "v2"] $ T.record [
    "encode">:
      doc "A function which encodes source values as target values in a given context" $
      Context.context ~> "v1" ~> T.either_ (Context.inContext @@ Error.error_) "v2",
    "decode">:
      doc "A function which decodes target values as source values in a given context" $
      Context.context ~> "v2" ~> T.either_ (Context.inContext @@ Error.error_) "v1"]

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
