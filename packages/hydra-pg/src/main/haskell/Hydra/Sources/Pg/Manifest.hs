-- | Package manifest for hydra-pg.
--
-- Owns the property graph DSL sources: the PG model, PG coder, GraphSON,
-- Cypher, GQL, TinkerPop, and Graphviz support. See
-- feature_290_packaging-plan.md, "Sync system redesign / Package manifests".
--
-- Note: Decode/Encode meta-sources under dist/haskell/hydra-pg/ are NOT
-- included here. They are second-generation DSL sources produced by
-- running code generation against hydra-pg's type modules; they are
-- currently imported directly by heads/haskell/Sources/Ext.hs for onward
-- processing.

module Hydra.Sources.Pg.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Cypher.Features as CypherFeatures
import qualified Hydra.Sources.Cypher.OpenCypher as OpenCypher
import qualified Hydra.Sources.Error.Pg as ErrorPg
import qualified Hydra.Sources.Gql.OpenGql as OpenGql
import qualified Hydra.Sources.Gql.PathAlgebra.Expressions as PathAlgebraExpressions
import qualified Hydra.Sources.Gql.PathAlgebra.Syntax as PathAlgebraSyntax
import qualified Hydra.Sources.Graphviz.Coder as GraphvizCoder
import qualified Hydra.Sources.Graphviz.Dot as DotSyntax
import qualified Hydra.Sources.Graphviz.Serde as GraphvizSerde
import qualified Hydra.Sources.Pg.Coder as PgCoder
import qualified Hydra.Sources.Pg.Graphson.Coder as GraphsonCoder
import qualified Hydra.Sources.Pg.Graphson.Construct as GraphsonConstruct
import qualified Hydra.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Sources.Pg.Graphson.Utils as GraphsonUtils
import qualified Hydra.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Pg.Printing as PgPrinting
import qualified Hydra.Sources.Pg.Query as PgQuery
import qualified Hydra.Sources.Pg.Rdf.Environment as PgRdfEnvironment
import qualified Hydra.Sources.Pg.Rdf.Mappings as PgRdfMappings
import qualified Hydra.Sources.Pg.TermsToElements as TermsToElements
import qualified Hydra.Sources.Pg.Utils as PgUtils
import qualified Hydra.Sources.Tinkerpop.Features as TinkerpopFeatures
import qualified Hydra.Sources.Tinkerpop.Gremlin as Gremlin
import qualified Hydra.Sources.Tinkerpop.Language as TinkerpopLanguage
import qualified Hydra.Sources.Validate.Pg as ValidatePg

mainModules :: [Module]
mainModules = [
  CypherFeatures.module_,
  ErrorPg.module_,
  GraphsonCoder.module_,
  GraphsonConstruct.module_,
  GraphsonSyntax.module_,
  GraphsonUtils.module_,
  GraphvizCoder.module_,
  DotSyntax.module_,
  GraphvizSerde.module_,
  Gremlin.module_,
  OpenCypher.module_,
  OpenGql.module_,
  PathAlgebraExpressions.module_,
  PathAlgebraSyntax.module_,
  PgCoder.module_,
  PgMapping.module_,
  PgModel.module_,
  PgPrinting.module_,
  PgQuery.module_,
  PgRdfEnvironment.module_,
  PgRdfMappings.module_,
  TermsToElements.module_,
  PgUtils.module_,
  TinkerpopFeatures.module_,
  TinkerpopLanguage.module_,
  ValidatePg.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/<Pkg>/<Name>.hs). Only modules whose
-- derived DSL is actually imported elsewhere are listed — extend the
-- list as new consumers appear.
--
-- Current consumers (as of 2026-05-16):
--   * Hydra.Dsl.Pg.Model — imported by Hydra.Sources.Pg.Rdf.Mappings
--
-- Note: the hand-written DSL modules Hydra.Dsl.Pg.Mappings and
-- Hydra.Dsl.Pg.Schemas (used by the genpg demos) are NOT derived from
-- type modules; they live in packages/hydra-pg/.../Hydra/Dsl/Pg/ and
-- don't go through this list.
dslTypeModules :: [Module]
dslTypeModules = [
  PgModel.module_]

testModules :: [Module]
testModules = []
