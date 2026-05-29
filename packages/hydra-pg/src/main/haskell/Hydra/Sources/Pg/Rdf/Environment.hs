-- | Environment types for property graph to RDF mapping.

module Hydra.Sources.Pg.Rdf.Environment where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Sources.Rdf.Syntax    as RdfSyntax
import qualified Hydra.Sources.Pg.Model      as PgModel


ns :: ModuleName
ns = ModuleName "hydra.pg.rdf.environment"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [RdfSyntax.ns, PgModel.ns, Core.ns],
            moduleDescription = Just "Environment types for property graph to RDF mapping"}
  where
    definitions = [
      pgRdfEnvironment]

core :: String -> Type
core = typeref Core.ns

env :: String -> Type
env = typeref ns

pg :: String -> Type
pg = typeref PgModel.ns

-- | The environment for property graph to RDF mapping, providing configurable
-- functions for encoding property graph labels, keys, ids, and values as RDF terms.
pgRdfEnvironment :: TypeDefinition
pgRdfEnvironment = define "PgRdfEnvironment" $
  doc "The environment for property graph to RDF mapping" $
  T.forAlls ["v"] $ T.record [
    "encodeVertexId">:
      doc "A function which encodes a vertex id as an RDF IRI" $
      T.function "v" (rdf "Iri"),
    "encodeVertexLabel">:
      doc "A function which encodes a vertex label as an RDF IRI" $
      T.function (pg "VertexLabel") (rdf "Iri"),
    "encodeEdgeId">:
      doc "A function which encodes an edge id as an RDF IRI" $
      T.function "v" (rdf "Iri"),
    "encodeEdgeLabel">:
      doc "A function which encodes an edge label as an RDF IRI" $
      T.function (pg "EdgeLabel") (rdf "Iri"),
    "encodePropertyKey">:
      doc "A function which encodes a property key as an RDF IRI" $
      T.function (pg "PropertyKey") (rdf "Iri"),
    "encodePropertyValue">:
      doc "A function which encodes a property value as an RDF literal" $
      T.function "v" (rdf "Literal")]

rdf :: String -> Type
rdf = typeref RdfSyntax.ns
