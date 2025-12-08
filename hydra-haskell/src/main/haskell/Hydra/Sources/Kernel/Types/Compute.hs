module Hydra.Sources.Kernel.Types.Compute where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.compute"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "Abstractions for single- and bidirectional transformations"
  where
    elements = [
      adapter,
      bicoder,
      coder,
      flow,
      flowState,
      trace]

adapter :: Binding
adapter = define "Adapter" $
  doc "A two-level bidirectional encoder which adapts types to types and terms to terms" $
  T.forAlls ["s1", "s2", "t1", "t2", "v1", "v2"] $ T.record [
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
      coder @@ "s1" @@ "s2" @@ "v1" @@ "v2"]

bicoder :: Binding
bicoder = define "Bicoder" $
  doc "A two-level encoder and decoder, operating both at a type level and an instance (data) level" $
  T.forAlls ["s1", "s2", "t1", "t2", "v1", "v2"] $ T.record [
    "encode">:
      doc "A function from source types to adapters" $
      "t1" ~> adapter @@ "s1" @@ "s2" @@ "t1" @@ "t2" @@ "v1" @@ "v2",
    "decode">:
      doc "A function from target types to adapters" $
      "t2" ~> adapter @@ "s2" @@ "s1" @@ "t2" @@ "t1" @@ "v2" @@ "v1"]

coder :: Binding
coder = define "Coder" $
  doc "An encoder and decoder; a bidirectional flow between two types" $
  T.forAlls ["s1", "s2", "v1", "v2"] $ T.record [
    "encode">:
      doc "A function from source values to a flow of target values" $
      "v1" ~> flow @@ "s1" @@ "v2",
    "decode">:
      doc "A function from target values to a flow of source values" $
      "v2" ~> flow @@ "s2" @@ "v1"]

flow :: Binding
flow = define "Flow" $
  doc "A variant of the State monad with built-in logging and error handling" $
  T.forAlls ["s", "v"] $ T.wrap $
  "s" ~> trace ~> flowState @@ "s" @@ "v"

flowState :: Binding
flowState = define "FlowState" $
  doc "The result of evaluating a Flow" $
  T.forAlls ["s", "v"] $ T.record [
    "value">:
      doc "The resulting value, or nothing in the case of failure" $
      T.optional "v",
    "state">:
      doc "The final state"
      "s",
    "trace">:
      doc "The trace (log) produced during evaluation"
      trace]

trace :: Binding
trace = define "Trace" $
  doc "A container for logging and error information" $
  T.record [
    "stack">:
      doc "A stack of context labels" $
      T.list T.string,
    "messages">:
      doc "A log of informational messages" $
      T.list T.string,
    "other">:
      doc "A map of string keys to arbitrary terms as values, for application-specific use" $
      T.map Core.name Core.term]
