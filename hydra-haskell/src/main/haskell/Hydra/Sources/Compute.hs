{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Compute where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


hydraComputeModule :: Module Kv
hydraComputeModule = Module ns elements [hydraCoreModule] $
    Just "Abstractions for single- and bidirectional transformations"
  where
    ns = Namespace "hydra/compute"
    core = nsref $ moduleNamespace hydraCoreModule
    compute = nsref ns

    def = datatype ns

    elements = [

      def "Adapter" $
        doc "A two-level bidirectional encoder which adapts types to types and terms to terms" $
        lambda "s1" $ lambda "s2" $ lambda "t1" $ lambda "t2" $ lambda "v1" $ lambda "v2" $ record [
          "isLossy">: boolean,
          "source">: var "t1",
          "target">: var "t2",
          "coder">: compute "Coder" @@ "s1" @@ "s2" @@ "v1" @@ "v2"],

      def "Coder" $
        doc "An encoder and decoder; a bidirectional flow between two types" $
        lambda "s1" $ lambda "s2" $ lambda "v1" $ lambda "v2" $ record [
          "encode">: ("v1" --> compute "Flow" @@ "s1" @@ "v2"),
          "decode">: ("v2" --> compute "Flow" @@ "s2" @@ "v1")],

      def "Flow" $
        doc "A variant of the State monad with built-in logging and error handling" $
        lambda "s" $ lambda "x" $
        function "s" (compute "Trace" --> compute "FlowState" @@ "s" @@ "x"),

      def "FlowState" $
        doc "The result of evaluating a Flow" $
        lambda "s" $ lambda "x" $ record [
          "value">: optional "x",
          "state">: "s",
          "trace">: compute "Trace"],

      def "Kv" $
        doc "A key/value map which serves as a built-in metadata container for terms" $
        record [
          "annotations">:
            doc "A map of annotation names to annotation values" $
            Types.map string (core "Term" @@ compute "Kv")],

      def "Trace" $
        doc "A container for logging and error information" $
        record [
          "stack">: list string,
          "messages">: list string,
          "other">:
            doc "A map of string keys to arbitrary terms as values, for application-specific use" $
            Types.map string (core "Term" @@ compute "Kv")]]
