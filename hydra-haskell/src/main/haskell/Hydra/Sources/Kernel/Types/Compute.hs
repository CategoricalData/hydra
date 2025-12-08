{-# LANGUAGE OverloadedStrings #-}

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
      doc "Whether information may be lost in the course of this adaptation" $
      T.boolean,
    "source">:
      doc "The source type" $
      T.var "t1",
    "target">:
      doc "The target type" $
      T.var "t2",
    "coder">:
      doc "The coder for transforming instances of the source type to instances of the target type" $
      use coder @@ T.var "s1" @@ T.var "s2" @@ T.var "v1" @@ T.var "v2"]

bicoder :: Binding
bicoder = define "Bicoder" $
  doc "A two-level encoder and decoder, operating both at a type level and an instance (data) level" $
  T.forAlls ["s1", "s2", "t1", "t2", "v1", "v2"] $ T.record [
    "encode">:
      doc "A function from source types to adapters" $
      T.var "t1" ~> (use adapter @@ T.var "s1" @@ T.var "s2" @@ T.var "t1" @@ T.var "t2" @@ T.var "v1" @@ T.var "v2"),
    "decode">:
      doc "A function from target types to adapters" $
      T.var "t2" ~> (use adapter @@ T.var "s2" @@ T.var "s1" @@ T.var "t2" @@ T.var "t1" @@ T.var "v2" @@ T.var "v1")]

coder :: Binding
coder = define "Coder" $
  doc "An encoder and decoder; a bidirectional flow between two types" $
  T.forAlls ["s1", "s2", "v1", "v2"] $ T.record [
    "encode">:
      doc "A function from source values to a flow of target values" $
      T.var "v1" ~> (use flow @@ T.var "s1" @@ T.var "v2"),
    "decode">:
      doc "A function from target values to a flow of source values" $
      T.var "v2" ~> (use flow @@ T.var "s2" @@ T.var "v1")]

flow :: Binding
flow = define "Flow" $
  doc "A variant of the State monad with built-in logging and error handling" $
  T.forAlls ["s", "v"] $ T.wrap $
  T.var "s" ~> (use trace ~> (use flowState @@ T.var "s" @@ T.var "v"))

flowState :: Binding
flowState = define "FlowState" $
  doc "The result of evaluating a Flow" $
  T.forAlls ["s", "v"] $ T.record [
    "value">:
      doc "The resulting value, or nothing in the case of failure" $
      T.optional (T.var "v"),
    "state">:
      doc "The final state" $
      T.var "s",
    "trace">:
      doc "The trace (log) produced during evaluation" $
      use trace]

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
      T.map (use Core.name) (use Core.term)]
