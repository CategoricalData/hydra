{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Compute where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "Abstractions for single- and bidirectional transformations"
  where
    ns = Namespace "hydra.compute"
    core = typeref $ moduleNamespace Core.module_
    compute = typeref ns

    def = datatype ns

    elements = [

      def "Adapter" $
        doc "A two-level bidirectional encoder which adapts types to types and terms to terms" $
        forAlls ["s1", "s2", "t1", "t2", "v1", "v2"] $ record [
          "isLossy">:
            doc "Whether information may be lost in the course of this adaptation" $
            boolean,
          "source">:
            doc "The source type" $
            var "t1",
          "target">:
            doc "The target type" $
            var "t2",
          "coder">:
            doc "The coder for transforming instances of the source type to instances of the target type" $
            compute "Coder" @@ "s1" @@ "s2" @@ "v1" @@ "v2"],

      def "Bicoder" $
        doc "A two-level encoder and decoder, operating both at a type level and an instance (data) level" $
        forAlls ["s1", "s2", "t1", "t2", "v1", "v2"] $ record [
          "encode">:
            doc "A function from source types to adapters" $
            "t1" --> compute "Adapter" @@ "s1" @@ "s2" @@ "t1" @@ "t2" @@ "v1" @@ "v2",
          "decode">:
            doc "A function from target types to adapters" $
            "t2" --> compute "Adapter" @@ "s2" @@ "s1" @@ "t2" @@ "t1" @@ "v2" @@ "v1"],

      def "Coder" $
        doc "An encoder and decoder; a bidirectional flow between two types" $
        forAlls ["s1", "s2", "v1", "v2"] $ record [
          "encode">:
            doc "A function from source values to a flow of target values" $
            ("v1" --> compute "Flow" @@ "s1" @@ "v2"),
          "decode">:
            doc "A function from target values to a flow of source values" $
            ("v2" --> compute "Flow" @@ "s2" @@ "v1")],

      def "Flow" $
        doc "A variant of the State monad with built-in logging and error handling" $
        forAlls ["s", "v"] $ wrap $
        function "s" (compute "Trace" --> compute "FlowState" @@ "s" @@ "v"),

      def "FlowState" $
        doc "The result of evaluating a Flow" $
        forAlls ["s", "v"] $ record [
          "value">:
            doc "The resulting value, or nothing in the case of failure" $
            optional "v",
          "state">:
            doc "The final state" $
            "s",
          "trace">:
            doc "The trace (log) produced during evaluation" $
            compute "Trace"],

      def "Trace" $
        doc "A container for logging and error information" $
        record [
          "stack">:
            doc "A stack of context labels" $
            list string,
          "messages">:
            doc "A log of informational messages" $
            list string,
          "other">:
            doc "A map of string keys to arbitrary terms as values, for application-specific use" $
            Types.map (core "Name") (core "Term")]]
