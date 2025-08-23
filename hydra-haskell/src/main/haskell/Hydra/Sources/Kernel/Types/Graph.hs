{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Graph where

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

import qualified Hydra.Sources.Kernel.Types.Compute as Compute


module_ :: Module
module_ = Module ns elements [Compute.module_] [Core.module_] $
    Just "The extension to graphs of Hydra's core type system (hydra.core)"
  where
    ns = Namespace "hydra.graph"
    core = typeref $ moduleNamespace Core.module_
    compute = typeref $ moduleNamespace Compute.module_
    graph = typeref ns
    def = datatype ns

    elements = [

      def "Graph" $
        doc "A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph" $
        record [

          -- TODO: remove this; replace it with 'environment'
          "elements">:
            doc "All of the elements in the graph" $
            Types.map (core "Name") (core "Binding"),

          "environment">:
            doc "The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)" $
            Types.map (core "Name") (optional $ core "Term"),
          "types">:
            doc "The typing environment of the graph" $
            Types.map (core "Name") (core "TypeScheme"),
          "body">:
            doc "The body of the term which generated this context" $
            core "Term",
          "primitives">:
            doc "All supported primitive constants and functions, by name" $
            Types.map (core "Name") (graph "Primitive"),
          "schema">:
            doc "The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph." $
            optional $ graph "Graph"],

      def "Primitive" $
        doc "A built-in function" $
        record [
          "name">:
            doc "The unique name of the primitive function" $
            core "Name",
          "type">:
            doc "The type signature of the primitive function" $
            core "TypeScheme",
          "implementation">:
            doc "A concrete implementation of the primitive function" $
            list (core "Term") --> compute "Flow" @@ graph "Graph" @@ core "Term"],

      def "TermCoder" $
        doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms" $
        forAll "a" $ record [
          "type">: core "Type",
          "coder">: compute "Coder" @@ graph "Graph" @@ graph "Graph" @@ core "Term" @@ "a"]]
