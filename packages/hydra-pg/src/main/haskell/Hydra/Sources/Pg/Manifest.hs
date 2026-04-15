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
) where

import Hydra.Kernel

import qualified Hydra.Sources.Cypher.Features as CypherFeatures
import qualified Hydra.Sources.Cypher.OpenCypher as OpenCypher
import qualified Hydra.Sources.Error.Pg as ErrorPg
import qualified Hydra.Sources.Gql.OpenGql as OpenGql
import qualified Hydra.Sources.Gql.PathAlgebra.Expressions as PathAlgebraExpressions
import qualified Hydra.Sources.Gql.PathAlgebra.Syntax as PathAlgebraSyntax
import qualified Hydra.Sources.Graphviz.Coder as GraphvizCoder
import qualified Hydra.Sources.Graphviz.Dot as GraphvizDot
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
import qualified Hydra.Sources.Pg.TermsToElements as PgTermsToElements
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
  GraphvizDot.module_,
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
  PgTermsToElements.module_,
  PgUtils.module_,
  TinkerpopFeatures.module_,
  TinkerpopLanguage.module_,
  ValidatePg.module_]

testModules :: [Module]
testModules = []
