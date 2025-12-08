{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Accessors where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.accessors"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "A model for term access patterns"
  where
    elements = [
      accessorEdge,
      accessorGraph,
      accessorNode,
      accessorPath,
      termAccessor]

accessorEdge :: Binding
accessorEdge = define "AccessorEdge" $
  doc "An edge in an accessor graph, connecting two nodes via a path" $
  T.record [
    "source">:
      doc "The source node of the edge" $
      use accessorNode,
    "path">:
      doc "The accessor path connecting source to target" $
      use accessorPath,
    "target">:
      doc "The target node of the edge" $
      use accessorNode]

accessorGraph :: Binding
accessorGraph = define "AccessorGraph" $
  doc "A graph of accessor nodes and edges, representing term access patterns" $
  T.record [
    "nodes">:
      doc "All nodes in the graph" $
      T.list $ use accessorNode,
    "edges">:
      doc "All edges in the graph" $
      T.list $ use accessorEdge]

accessorNode :: Binding
accessorNode = define "AccessorNode" $
  doc "A node in an accessor graph, representing a term or subterm" $
  T.record [
    "name">:
      doc "The qualified name of the term" $
      use Core.name,
    "label">:
      doc "A human-readable label for the node" $
      T.string,
    "id" >:
      doc "A unique identifier for the node" $
      T.string]

accessorPath :: Binding
accessorPath = define "AccessorPath" $
  doc "A sequence of term accessors forming a path through a term" $
  T.wrap $ T.list $ use termAccessor

termAccessor :: Binding
termAccessor = define "TermAccessor" $
  doc "A function which maps from a term to a particular immediate subterm" $
  T.union [
    "annotatedBody">:
      doc "Access the body of an annotated term" $
      T.unit,
    "applicationFunction">:
      doc "Access the function of an application term" $
      T.unit,
    "applicationArgument">:
      doc "Access the argument of an application term" $
      T.unit,
    "lambdaBody">:
      doc "Access the body of a lambda term" $
      T.unit,
    "unionCasesDefault">:
      doc "Access the default case of a union elimination" $
      T.unit,
    "unionCasesBranch">:
      doc "Access a specific branch of a union elimination by field name" $
      use Core.name,
    "letBody">:
      doc "Access the body of a let term" $
      T.unit,
    "letBinding">:
      doc "Access a specific binding in a let term by variable name" $
      use Core.name,
    "listElement">:
      doc "Access an element of a list by index" $
      T.int32,
    "mapKey">:
      doc "Access a key in a map by index" $
      T.int32,
    "mapValue">:
      doc "Access a value in a map by index" $
      T.int32,
    "maybeTerm">:
      doc "Access the term inside a Just value" $
      T.unit,
    "productTerm">:
      doc "Access an element of a product (tuple) by index" $
      T.int32,
    "recordField">:
      doc "Access a field of a record by field name" $
      use Core.name,
    "setElement">:
      doc "Access an element of a set by index" $
      T.int32,
    "sumTerm">:
      doc "Access the term inside a sum variant" $
      T.unit,
    "typeLambdaBody">:
      doc "Access the body of a type lambda term" $
      T.unit,
    "typeApplicationTerm">:
      doc "Access the term being applied to a type" $
      T.unit,
    "injectionTerm">:
      doc "Access the term inside a union injection" $
      T.unit,
    "wrappedTerm">:
      doc "Access the term inside a wrapped term" $
      T.unit]
