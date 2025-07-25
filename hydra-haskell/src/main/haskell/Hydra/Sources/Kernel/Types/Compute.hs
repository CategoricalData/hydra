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
          "isLossy">: boolean,
          "source">: var "t1",
          "target">: var "t2",
          "coder">: compute "Coder" @@ "s1" @@ "s2" @@ "v1" @@ "v2"],

      def "Bicoder" $
        doc "A two-level encoder and decoder, operating both at a type level and an instance (data) level" $
        forAlls ["s1", "s2", "t1", "t2", "v1", "v2"] $ record [
          "encode">: "t1" --> compute "Adapter" @@ "s1" @@ "s2" @@ "t1" @@ "t2" @@ "v1" @@ "v2",
          "decode">: "t2" --> compute "Adapter" @@ "s2" @@ "s1" @@ "t2" @@ "t1" @@ "v2" @@ "v1"],

      def "Coder" $
        doc "An encoder and decoder; a bidirectional flow between two types" $
        forAlls ["s1", "s2", "v1", "v2"] $ record [
          "encode">: ("v1" --> compute "Flow" @@ "s1" @@ "v2"),
          "decode">: ("v2" --> compute "Flow" @@ "s2" @@ "v1")],

      def "Flow" $
        doc "A variant of the State monad with built-in logging and error handling" $
        forAlls ["s", "v"] $ wrap $
        function "s" (compute "Trace" --> compute "FlowState" @@ "s" @@ "v"),

      def "FlowState" $
        doc "The result of evaluating a Flow" $
        forAlls ["s", "v"] $ record [
          "value">: optional "v",
          "state">: "s",
          "trace">: compute "Trace"],

      def "Trace" $
        doc "A container for logging and error information" $
        record [
          "stack">: list string,
          "messages">: list string,
          "other">:
            doc "A map of string keys to arbitrary terms as values, for application-specific use" $
            Types.map (core "Name") (core "Term")]]
