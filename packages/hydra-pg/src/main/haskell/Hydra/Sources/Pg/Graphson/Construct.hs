{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Pg.Graphson.Construct where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  aggregateMap, adjacentEdgeToGraphson, edgePropertyToGraphson,
  graphsonVertexToJsonCoder, pgVertexWithAdjacentEdgesToGraphsonVertex,
  pgVertexWithAdjacentEdgesToJson, vertexPropertyToGraphson)
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Sources.Pg.Graphson.Coder as GraphsonCoder
import qualified Hydra.Sources.Pg.Graphson.Coder as Coder (vertexToJson)
import qualified Hydra.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Json.Model as JsonModel
import qualified Hydra.Pg.Graphson.Syntax as G  -- Generated phantom types
import qualified Hydra.Pg.Model as PG            -- Generated PG model types
import qualified Hydra.Json.Model as JM          -- Generated JSON types


ns :: ModuleName
ns = ModuleName "hydra.pg.graphson.construct"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (([GraphsonCoder.ns] L.++ (kernelTypesModuleNames L.++ [GraphsonSyntax.ns, PgModel.ns, JsonModel.ns]))),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Functions for constructing GraphSON vertices from property graph vertices.")}
  where
    definitions = [
      toDefinition adjacentEdgeToGraphson,
      toDefinition (aggregateMap :: TypedTermDefinition ([(G.PropertyKey, G.Value)] -> M.Map G.PropertyKey [G.Value])),
      toDefinition edgePropertyToGraphson,
      toDefinition graphsonVertexToJsonCoder,
      toDefinition pgVertexWithAdjacentEdgesToGraphsonVertex,
      toDefinition pgVertexWithAdjacentEdgesToJson,
      toDefinition vertexPropertyToGraphson]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Convert a PG adjacent edge to GraphSON format
adjacentEdgeToGraphson :: TypedTermDefinition ((v -> Either Error G.Value) -> PG.AdjacentEdge v -> Either Error (G.EdgeLabel, G.AdjacentEdge))
adjacentEdgeToGraphson = define "adjacentEdgeToGraphson" $
  doc "Convert a property graph adjacent edge to a GraphSON adjacent edge" $
  "encodeValue" ~> "edge" ~>
    "label" <~ (project PG._AdjacentEdge PG._AdjacentEdge_label @@ var "edge") $
    "edgeId" <~ (project PG._AdjacentEdge PG._AdjacentEdge_id @@ var "edge") $
    "vertexId" <~ (project PG._AdjacentEdge PG._AdjacentEdge_vertex @@ var "edge") $
    "props" <~ (project PG._AdjacentEdge PG._AdjacentEdge_properties @@ var "edge") $
    Eithers.bind
      (var "encodeValue" @@ var "edgeId")
      ("gid" ~>
        Eithers.bind
          (var "encodeValue" @@ var "vertexId")
          ("gv" ~>
            Eithers.bind
              (Eithers.mapList (edgePropertyToGraphson @@ var "encodeValue") (Maps.toList $ var "props"))
              ("propPairs" ~>
                right $
                  pair
                    (wrap G._EdgeLabel (unwrap PG._EdgeLabel @@ var "label"))
                    (record G._AdjacentEdge [
                      G._AdjacentEdge_id>>: var "gid",
                      G._AdjacentEdge_vertexId>>: var "gv",
                      G._AdjacentEdge_properties>>: ((Maps.fromList (var "propPairs")) :: TypedTerm (M.Map G.PropertyKey G.Value))]))))

-- | Aggregate a list of key-value pairs into a map of lists
-- `Ord k` + `forall` because the generated `Hydra.Dsl.Lib.Maps` exposes the primitive's `Ord` key
-- constraint (the old hand-written `Meta.Lib.Maps` did not), which also forces a placeholder concrete
-- type at registration in `definitions`. See #467.
aggregateMap :: forall k v. Ord k => TypedTermDefinition ([(k, v)] -> M.Map k [v])
aggregateMap = define "aggregateMap" $
  doc "Aggregate a list of key-value pairs into a map where each key maps to a list of values" $
  "pairs" ~>
    Lists.foldl
      ("m" ~> "p" ~>
        "k" <~ ((Pairs.first $ var "p") :: TypedTerm k) $
        "v" <~ (Pairs.second $ var "p") $
        "existing" <~ ((Maps.lookup (var "k") ((var "m") :: TypedTerm (M.Map k [v]))) :: TypedTerm (Y.Maybe [v])) $
        ((Maps.insert (var "k")
          (Optionals.cases (var "existing") (Lists.pure $ var "v") ("vs" ~> Lists.cons (var "v") (var "vs")))
          ((var "m") :: TypedTerm (M.Map k [v]))) :: TypedTerm (M.Map k [v])))
      (Maps.empty :: TypedTerm (M.Map k [v]))
      (var "pairs")

-- | Convert a PG edge property to GraphSON format
edgePropertyToGraphson :: TypedTermDefinition ((v -> Either Error G.Value) -> (PG.PropertyKey, v) -> Either Error (G.PropertyKey, G.Value))
edgePropertyToGraphson = define "edgePropertyToGraphson" $
  doc "Convert a property graph edge property to a GraphSON property" $
  "encodeValue" ~> "prop" ~>
    Eithers.map
      ("gv" ~>
        pair
          (wrap G._PropertyKey (unwrap PG._PropertyKey @@ (Pairs.first $ var "prop")))
          (var "gv"))
      (var "encodeValue" @@ (Pairs.second $ var "prop"))

-- | A coder that converts GraphSON vertices to JSON
graphsonVertexToJsonCoder :: TypedTermDefinition (Coder G.Vertex JM.Value)
graphsonVertexToJsonCoder = define "graphsonVertexToJsonCoder" $
  doc "A coder that converts GraphSON vertices to JSON. Decoding is not supported." $
  Coders.coder
    ("_cx" ~> "v" ~> right (Coder.vertexToJson @@ var "v"))
    ("_cx" ~> "_" ~> left (Error.errorOther $ Error.otherError $ string "decoding GraphSON JSON is currently unsupported"))

-- Type references
gson :: String -> Type
gson = Bootstrap.typeref GraphsonSyntax.ns

jsonValue :: Type
jsonValue = Bootstrap.typeref JsonModel.ns "Value"

pg :: String -> Type
pg = Bootstrap.typeref PgModel.ns

-- | Convert a PG vertex with adjacent edges to a GraphSON vertex
pgVertexWithAdjacentEdgesToGraphsonVertex :: TypedTermDefinition ((v -> Either Error G.Value) -> PG.VertexWithAdjacentEdges v -> Either Error G.Vertex)
pgVertexWithAdjacentEdgesToGraphsonVertex = define "pgVertexWithAdjacentEdgesToGraphsonVertex" $
  doc "Convert a property graph vertex with adjacent edges to a GraphSON vertex" $
  "encodeValue" ~> "vae" ~>
    "vertex" <~ (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_vertex @@ var "vae") $
    "ins" <~ (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_ins @@ var "vae") $
    "outs" <~ (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_outs @@ var "vae") $
    "label" <~ (project PG._Vertex PG._Vertex_label @@ var "vertex") $
    "vertexId" <~ (project PG._Vertex PG._Vertex_id @@ var "vertex") $
    "props" <~ (project PG._Vertex PG._Vertex_properties @@ var "vertex") $
    Eithers.bind
      (var "encodeValue" @@ var "vertexId")
      ("gid" ~>
        Eithers.bind
          (Eithers.mapList (vertexPropertyToGraphson @@ var "encodeValue") (Maps.toList $ var "props"))
          ("propPairs" ~>
            Eithers.bind
              (Eithers.mapList (adjacentEdgeToGraphson @@ var "encodeValue") (var "ins"))
              ("inPairs" ~>
                Eithers.bind
                  (Eithers.mapList (adjacentEdgeToGraphson @@ var "encodeValue") (var "outs"))
                  ("outPairs" ~>
                    right $
                      record G._Vertex [
                        G._Vertex_id>>: var "gid",
                        G._Vertex_label>>: just (wrap G._VertexLabel (unwrap PG._VertexLabel @@ var "label")),
                        G._Vertex_inEdges>>: (aggregateMap :: TypedTermDefinition ([(G.EdgeLabel, G.AdjacentEdge)] -> M.Map G.EdgeLabel [G.AdjacentEdge])) @@ var "inPairs",
                        G._Vertex_outEdges>>: (aggregateMap :: TypedTermDefinition ([(G.EdgeLabel, G.AdjacentEdge)] -> M.Map G.EdgeLabel [G.AdjacentEdge])) @@ var "outPairs",
                        G._Vertex_properties>>: (aggregateMap :: TypedTermDefinition ([(G.PropertyKey, G.VertexPropertyValue)] -> M.Map G.PropertyKey [G.VertexPropertyValue])) @@ var "propPairs"]))))

-- | Convert a PG vertex with adjacent edges directly to JSON
pgVertexWithAdjacentEdgesToJson :: TypedTermDefinition ((v -> Either Error G.Value) -> PG.VertexWithAdjacentEdges v -> Either Error JM.Value)
pgVertexWithAdjacentEdgesToJson = define "pgVertexWithAdjacentEdgesToJson" $
  doc "Convert a property graph vertex with adjacent edges to JSON" $
  "encodeValue" ~> "vertex" ~>
    Eithers.bind
      (pgVertexWithAdjacentEdgesToGraphsonVertex @@ var "encodeValue" @@ var "vertex")
      ("gVertex" ~>
        right $ Coder.vertexToJson @@ var "gVertex")

-- | Convert a PG vertex property to GraphSON format
vertexPropertyToGraphson :: TypedTermDefinition ((v -> Either Error G.Value) -> (PG.PropertyKey, v) -> Either Error (G.PropertyKey, G.VertexPropertyValue))
vertexPropertyToGraphson = define "vertexPropertyToGraphson" $
  doc "Convert a property graph vertex property to a GraphSON vertex property" $
  "encodeValue" ~> "prop" ~>
    Eithers.map
      ("gv" ~>
        pair
          (wrap G._PropertyKey (unwrap PG._PropertyKey @@ (Pairs.first $ var "prop")))
          (record G._VertexPropertyValue [
            G._VertexPropertyValue_id>>: nothing,
            G._VertexPropertyValue_value>>: var "gv"]))
      (var "encodeValue" @@ (Pairs.second $ var "prop"))
