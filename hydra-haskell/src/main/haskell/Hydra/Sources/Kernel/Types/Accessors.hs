module Hydra.Sources.Kernel.Types.Accessors where

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
    Just "A model for term access patterns"
  where
    ns = Namespace "hydra.accessors"
    def = datatype ns
    accessors = typeref ns
    core = typeref $ moduleNamespace Core.module_

    elements = [

      def "AccessorEdge" $
        doc "An edge in an accessor graph, connecting two nodes via a path" $
        record [
          "source">:
            doc "The source node of the edge" $
            accessors "AccessorNode",
          "path">:
            doc "The accessor path connecting source to target" $
            accessors "AccessorPath",
          "target">:
            doc "The target node of the edge" $
            accessors "AccessorNode"],

      def "AccessorGraph" $
        doc "A graph of accessor nodes and edges, representing term access patterns" $
        record [
          "nodes">:
            doc "All nodes in the graph" $
            list $ accessors "AccessorNode",
          "edges">:
            doc "All edges in the graph" $
            list $ accessors "AccessorEdge"],

      def "AccessorNode" $
        doc "A node in an accessor graph, representing a term or subterm" $
        record [
          "name">:
            doc "The qualified name of the term" $
            core "Name",
          "label">:
            doc "A human-readable label for the node" $
            string,
          "id" >:
            doc "A unique identifier for the node" $
            string],

      def "AccessorPath" $
        doc "A sequence of term accessors forming a path through a term" $
        wrap $ list $ accessors "TermAccessor",

      def "TermAccessor" $
        doc "A function which maps from a term to a particular immediate subterm" $
        union [
          "annotatedBody">:
            doc "Access the body of an annotated term" $
            unit,
          "applicationFunction">:
            doc "Access the function of an application term" $
            unit,
          "applicationArgument">:
            doc "Access the argument of an application term" $
            unit,
          "lambdaBody">:
            doc "Access the body of a lambda term" $
            unit,
          "unionCasesDefault">:
            doc "Access the default case of a union elimination" $
            unit,
          "unionCasesBranch">:
            doc "Access a specific branch of a union elimination by field name" $
            core "Name",
          "letBody">:
            doc "Access the body of a let term" $
            unit,
          "letBinding">:
            doc "Access a specific binding in a let term by variable name" $
            core "Name",
          "listElement">:
            doc "Access an element of a list by index" $
            int32,
          "mapKey">:
            doc "Access a key in a map by index" $
            int32,
          "mapValue">:
            doc "Access a value in a map by index" $
            int32,
          "maybeTerm">:
            doc "Access the term inside a Just value" $
            unit,
          "productTerm">:
            doc "Access an element of a product (tuple) by index" $
            int32,
          "recordField">:
            doc "Access a field of a record by field name" $
            core "Name",
          "setElement">:
            doc "Access an element of a set by index" $
            int32,
          "sumTerm">:
            doc "Access the term inside a sum variant" $
            unit,
          "typeLambdaBody">:
            doc "Access the body of a type lambda term" $
            unit,
          "typeApplicationTerm">:
            doc "Access the term being applied to a type" $
            unit,
          "injectionTerm">:
            doc "Access the term inside a union injection" $
            unit,
          "wrappedTerm">:
            doc "Access the term inside a wrapped term" $
            unit]]
