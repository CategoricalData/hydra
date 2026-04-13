-- Note: this is an automatically generated file. Do not edit.

-- | Environment types for property graph to RDF mapping

module Hydra.Pg.Rdf.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Rdf.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | The environment for property graph to RDF mapping
data PgRdfEnvironment v =
  PgRdfEnvironment {
    -- | A function which encodes a vertex id as an RDF IRI
    pgRdfEnvironmentEncodeVertexId :: (v -> Syntax.Iri),
    -- | A function which encodes a vertex label as an RDF IRI
    pgRdfEnvironmentEncodeVertexLabel :: (Model.VertexLabel -> Syntax.Iri),
    -- | A function which encodes an edge id as an RDF IRI
    pgRdfEnvironmentEncodeEdgeId :: (v -> Syntax.Iri),
    -- | A function which encodes an edge label as an RDF IRI
    pgRdfEnvironmentEncodeEdgeLabel :: (Model.EdgeLabel -> Syntax.Iri),
    -- | A function which encodes a property key as an RDF IRI
    pgRdfEnvironmentEncodePropertyKey :: (Model.PropertyKey -> Syntax.Iri),
    -- | A function which encodes a property value as an RDF literal
    pgRdfEnvironmentEncodePropertyValue :: (v -> Syntax.Literal)}

_PgRdfEnvironment = Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"

_PgRdfEnvironment_encodeVertexId = Core.Name "encodeVertexId"

_PgRdfEnvironment_encodeVertexLabel = Core.Name "encodeVertexLabel"

_PgRdfEnvironment_encodeEdgeId = Core.Name "encodeEdgeId"

_PgRdfEnvironment_encodeEdgeLabel = Core.Name "encodeEdgeLabel"

_PgRdfEnvironment_encodePropertyKey = Core.Name "encodePropertyKey"

_PgRdfEnvironment_encodePropertyValue = Core.Name "encodePropertyValue"
