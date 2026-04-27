module Hydra.Sources.Error.Pg where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Pg.Model      as PgModel


ns :: Namespace
ns = Namespace "hydra.error.pg"

define :: String -> Type -> Binding
define = defineType ns

pg :: String -> Type
pg = typeref PgModel.ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [],
            moduleTypeDependencies = [PgModel.ns],
            moduleDescription = Just "Error types for property graph validation"}
  where
    definitions = [
      invalidEdgeError,
      invalidElementPropertyError,
      invalidGraphEdgeError,
      invalidGraphError,
      invalidGraphVertexError,
      invalidPropertyError,
      invalidValueError,
      invalidVertexError,
      noSuchEdgeLabelError,
      noSuchVertexLabelError,
      wrongVertexLabelError]

invalidEdgeError :: Binding
invalidEdgeError = define "InvalidEdgeError" $
  doc "An error indicating that an edge is invalid" $
  T.union [
    "id">:
      doc "The edge id value is invalid" $
      invalidValueError,
    "inVertexLabel">:
      doc "The in-vertex has the wrong label" $
      wrongVertexLabelError,
    "inVertexNotFound">:
      doc "The in-vertex does not exist in the graph" $
      T.unit,
    "label">:
      doc "The edge label does not exist in the schema" $
      noSuchEdgeLabelError,
    "outVertexLabel">:
      doc "The out-vertex has the wrong label" $
      wrongVertexLabelError,
    "outVertexNotFound">:
      doc "The out-vertex does not exist in the graph" $
      T.unit,
    "property">:
      doc "A property of the edge is invalid" $
      invalidElementPropertyError]

invalidElementPropertyError :: Binding
invalidElementPropertyError = define "InvalidElementPropertyError" $
  doc "An invalid property on a vertex or edge, identified by its key" $
  T.record [
    "key">:
      doc "The key of the invalid property" $
      pg "PropertyKey",
    "error">:
      doc "The specific error" $
      invalidPropertyError]

invalidGraphEdgeError :: Binding
invalidGraphEdgeError = define "InvalidGraphEdgeError" $
  doc "An invalid edge within a graph, identified by its id" $
  T.forAll "v" $ T.record [
    "id">:
      doc "The id of the invalid edge" $
      "v",
    "error">:
      doc "The specific error" $
      invalidEdgeError]

invalidGraphError :: Binding
invalidGraphError = define "InvalidGraphError" $
  doc "An error indicating that a property graph is invalid" $
  T.forAll "v" $ T.union [
    "edge">:
      doc "An edge in the graph is invalid" $
      T.apply invalidGraphEdgeError "v",
    "vertex">:
      doc "A vertex in the graph is invalid" $
      T.apply invalidGraphVertexError "v"]

invalidGraphVertexError :: Binding
invalidGraphVertexError = define "InvalidGraphVertexError" $
  doc "An invalid vertex within a graph, identified by its id" $
  T.forAll "v" $ T.record [
    "id">:
      doc "The id of the invalid vertex" $
      "v",
    "error">:
      doc "The specific error" $
      invalidVertexError]

invalidPropertyError :: Binding
invalidPropertyError = define "InvalidPropertyError" $
  doc "An error indicating that a property is invalid" $
  T.union [
    "invalidValue">:
      doc "The property value failed type validation" $
      invalidValueError,
    "missingRequired">:
      doc "A required property is missing" $
      pg "PropertyKey",
    "unexpectedKey">:
      doc "A property has an unexpected key not in the schema" $
      pg "PropertyKey"]

invalidValueError :: Binding
invalidValueError = define "InvalidValueError" $
  doc "An error indicating that a value does not match the expected type" $
  T.record [
    "expectedType">:
      doc "The expected type, as a string" $
      T.string,
    "value">:
      doc "The actual value, as a string" $
      T.string]

invalidVertexError :: Binding
invalidVertexError = define "InvalidVertexError" $
  doc "An error indicating that a vertex is invalid" $
  T.union [
    "id">:
      doc "The vertex id value is invalid" $
      invalidValueError,
    "label">:
      doc "The vertex label does not exist in the schema" $
      noSuchVertexLabelError,
    "property">:
      doc "A property of the vertex is invalid" $
      invalidElementPropertyError]

noSuchEdgeLabelError :: Binding
noSuchEdgeLabelError = define "NoSuchEdgeLabelError" $
  doc "An error indicating that an edge label does not exist in the schema" $
  T.record [
    "label">:
      doc "The edge label that was not found" $
      pg "EdgeLabel"]

noSuchVertexLabelError :: Binding
noSuchVertexLabelError = define "NoSuchVertexLabelError" $
  doc "An error indicating that a vertex label does not exist in the schema" $
  T.record [
    "label">:
      doc "The vertex label that was not found" $
      pg "VertexLabel"]

wrongVertexLabelError :: Binding
wrongVertexLabelError = define "WrongVertexLabelError" $
  doc "An error indicating that a vertex has the wrong label" $
  T.record [
    "expected">:
      doc "The expected vertex label" $
      pg "VertexLabel",
    "actual">:
      doc "The actual vertex label" $
      pg "VertexLabel"]
