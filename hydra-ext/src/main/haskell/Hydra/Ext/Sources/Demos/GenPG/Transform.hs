module Hydra.Ext.Sources.Demos.GenPG.Transform where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Reduction as Reduction
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Types.Tabular as TabularModel
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Pg.Model as PG            -- Generated PG model types
import qualified Hydra.Tabular as Tab            -- Generated tabular types
import qualified Hydra.Relational as Rel         -- Generated relational types
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.demos.genpg.transform"

module_ :: Module
module_ = Module ns elements
    [Reduction.ns, Rewriting.ns, ExtractCore.ns]  -- term dependencies
    (kernelTypesNamespaces L.++ [PgModel.ns, TabularModel.ns]) $  -- type dependencies
    Just "Functions for transforming property graph mappings into property graph elements."
  where
    elements = [
      toBinding evaluateEdge,
      toBinding evaluateProperties,
      toBinding evaluateVertex,
      toBinding findTablesInTerm,
      toBinding findTablesInTerms,
      toBinding elementSpecsByTable,
      toBinding tableForEdge,
      toBinding tableForVertex,
      toBinding termRowToRecord,
      toBinding transformRecord]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Type references
pg :: String -> Type
pg = Bootstrap.typeref PgModel.ns

tab :: String -> Type
tab = Bootstrap.typeref TabularModel.ns

-- | Evaluate properties by applying each spec to the record and extracting optional values
evaluateProperties :: TBinding (M.Map PG.PropertyKey Term -> Term -> Flow Graph (M.Map PG.PropertyKey Term))
evaluateProperties = define "evaluateProperties" $
  doc "Evaluate property specifications against a record term" $
  "specs" ~> "record" ~>
    Flows.map
      ("pairs" ~> Maps.fromList $ Maybes.cat $ var "pairs")
      (Flows.mapList
        ("pair" ~>
          "k" <~ Pairs.first (var "pair") $
          "spec" <~ Pairs.second (var "pair") $
          Flows.bind
            (Reduction.reduceTerm @@ boolean True @@ (Core.termApplication $ Core.application (var "spec") (var "record")))
            ("value" ~>
              match _Term Nothing [
                _Term_maybe>>: "mv" ~>
                  Flows.pure $ Maybes.map ("v" ~> pair (var "k") (var "v")) (var "mv")]
              @@ (Rewriting.deannotateTerm @@ var "value")))
        (Maps.toList $ var "specs"))

-- | Evaluate an edge specification against a record term
evaluateEdge :: TBinding (PG.Edge Term -> Term -> Flow Graph (Maybe (PG.Edge Term)))
evaluateEdge = define "evaluateEdge" $
  doc "Evaluate an edge specification against a record term to produce an optional edge" $
  "edgeSpec" ~> "record" ~>
    "label" <~ (project PG._Edge PG._Edge_label @@ var "edgeSpec") $
    "idSpec" <~ (project PG._Edge PG._Edge_id @@ var "edgeSpec") $
    "outSpec" <~ (project PG._Edge PG._Edge_out @@ var "edgeSpec") $
    "inSpec" <~ (project PG._Edge PG._Edge_in @@ var "edgeSpec") $
    "propSpecs" <~ (project PG._Edge PG._Edge_properties @@ var "edgeSpec") $
    Flows.bind
      (Reduction.reduceTerm @@ boolean True @@ (Core.termApplication $ Core.application (var "idSpec") (var "record")))
      ("id" ~>
        Flows.bind
          (Flows.bind
            (Reduction.reduceTerm @@ boolean True @@ (Core.termApplication $ Core.application (var "outSpec") (var "record")))
            (ExtractCore.maybeTerm @@ ("t" ~> Flows.pure (var "t"))))
          ("mOutId" ~>
            Flows.bind
              (Flows.bind
                (Reduction.reduceTerm @@ boolean True @@ (Core.termApplication $ Core.application (var "inSpec") (var "record")))
                (ExtractCore.maybeTerm @@ ("t" ~> Flows.pure (var "t"))))
              ("mInId" ~>
                Flows.bind
                  (evaluateProperties @@ var "propSpecs" @@ var "record")
                  ("props" ~>
                    Flows.pure $
                      Maybes.bind (var "mOutId")
                        ("outId" ~>
                          Maybes.map
                            ("inId" ~>
                              record PG._Edge [
                                PG._Edge_label>>: var "label",
                                PG._Edge_id>>: var "id",
                                PG._Edge_out>>: var "outId",
                                PG._Edge_in>>: var "inId",
                                PG._Edge_properties>>: var "props"])
                            (var "mInId"))))))

-- | Evaluate a vertex specification against a record term
evaluateVertex :: TBinding (PG.Vertex Term -> Term -> Flow Graph (Maybe (PG.Vertex Term)))
evaluateVertex = define "evaluateVertex" $
  doc "Evaluate a vertex specification against a record term to produce an optional vertex" $
  "vertexSpec" ~> "record" ~>
    "label" <~ (project PG._Vertex PG._Vertex_label @@ var "vertexSpec") $
    "idSpec" <~ (project PG._Vertex PG._Vertex_id @@ var "vertexSpec") $
    "propSpecs" <~ (project PG._Vertex PG._Vertex_properties @@ var "vertexSpec") $
    Flows.bind
      (Flows.bind
        (Reduction.reduceTerm @@ boolean True @@ (Core.termApplication $ Core.application (var "idSpec") (var "record")))
        (ExtractCore.maybeTerm @@ ("t" ~> Flows.pure (var "t"))))
      ("mId" ~>
        Flows.bind
          (evaluateProperties @@ var "propSpecs" @@ var "record")
          ("props" ~>
            Flows.pure $
              Maybes.map
                ("id" ~>
                  record PG._Vertex [
                    PG._Vertex_label>>: var "label",
                    PG._Vertex_id>>: var "id",
                    PG._Vertex_properties>>: var "props"])
                (var "mId")))

-- | Find table names referenced in a term by looking for record projections
findTablesInTerm :: TBinding (Term -> S.Set String)
findTablesInTerm = define "findTablesInTerm" $
  doc "Find table names referenced in a term by looking for record projections" $
  "term" ~>
    Rewriting.foldOverTerm @@ Coders.traversalOrderPre
      @@ ("names" ~> "t" ~>
        match _Term (Just $ var "names") [
          _Term_function>>: "f" ~>
            match _Function (Just $ var "names") [
              _Function_elimination>>: "e" ~>
                match _Elimination (Just $ var "names") [
                  _Elimination_record>>: "proj" ~>
                    Sets.insert
                      (Core.unName (project _Projection _Projection_typeName @@ var "proj"))
                      (var "names")]
                @@ var "e"]
            @@ var "f"]
        @@ var "t")
      @@ Sets.empty
      @@ var "term"

-- | Find table names referenced in multiple terms
findTablesInTerms :: TBinding ([Term] -> S.Set String)
findTablesInTerms = define "findTablesInTerms" $
  doc "Find table names referenced in multiple terms" $
  "terms" ~>
    Sets.unions $ Lists.map findTablesInTerm (var "terms")

-- | Get the table name for an edge specification
tableForEdge :: TBinding (PG.Edge Term -> Either String String)
tableForEdge = define "tableForEdge" $
  doc "Get the table name for an edge specification. Returns an error if not exactly one table is referenced." $
  "edge" ~>
    "label" <~ (project PG._Edge PG._Edge_label @@ var "edge") $
    "id" <~ (project PG._Edge PG._Edge_id @@ var "edge") $
    "outId" <~ (project PG._Edge PG._Edge_out @@ var "edge") $
    "inId" <~ (project PG._Edge PG._Edge_in @@ var "edge") $
    "props" <~ (project PG._Edge PG._Edge_properties @@ var "edge") $
    "tables" <~ (findTablesInTerms @@ Lists.concat2
      (list [var "id", var "outId", var "inId"])
      (Maps.elems $ var "props")) $
    Logic.ifElse (Equality.equal (Sets.size $ var "tables") (int32 1))
      (right $ Lists.head $ Sets.toList $ var "tables")
      (left $ Strings.cat $ list [
        string "Specification for ",
        unwrap PG._EdgeLabel @@ var "label",
        string " edges has wrong number of tables"])

-- | Get the table name for a vertex specification
tableForVertex :: TBinding (PG.Vertex Term -> Either String String)
tableForVertex = define "tableForVertex" $
  doc "Get the table name for a vertex specification. Returns an error if not exactly one table is referenced." $
  "vertex" ~>
    "label" <~ (project PG._Vertex PG._Vertex_label @@ var "vertex") $
    "id" <~ (project PG._Vertex PG._Vertex_id @@ var "vertex") $
    "props" <~ (project PG._Vertex PG._Vertex_properties @@ var "vertex") $
    "tables" <~ (findTablesInTerms @@ Lists.cons (var "id") (Maps.elems $ var "props")) $
    Logic.ifElse (Equality.equal (Sets.size $ var "tables") (int32 1))
      (right $ Lists.head $ Sets.toList $ var "tables")
      (left $ Strings.cat $ list [
        string "Specification for ",
        unwrap PG._VertexLabel @@ var "label",
        string " vertices has wrong number of tables"])

-- | Group element specifications by their source table
elementSpecsByTable :: TBinding (PG.LazyGraph Term -> Either String (M.Map String ([PG.Vertex Term], [PG.Edge Term])))
elementSpecsByTable = define "elementSpecsByTable" $
  doc "Group element specifications by their source table" $
  "graph" ~>
    "vertices" <~ (project PG._LazyGraph PG._LazyGraph_vertices @@ var "graph") $
    "edges" <~ (project PG._LazyGraph PG._LazyGraph_edges @@ var "graph") $
    -- Map vertices to (table, vertex) pairs
    Eithers.bind
      (Eithers.mapList
        ("v" ~> Eithers.map ("t" ~> pair (var "t") (var "v")) (tableForVertex @@ var "v"))
        (var "vertices"))
      ("vertexPairs" ~>
        -- Map edges to (table, edge) pairs
        Eithers.bind
          (Eithers.mapList
            ("e" ~> Eithers.map ("t" ~> pair (var "t") (var "e")) (tableForEdge @@ var "e"))
            (var "edges"))
          ("edgePairs" ~>
            -- Build the map by folding over pairs
            "addVertex" <~ ("m" ~> "p" ~>
              "table" <~ Pairs.first (var "p") $
              "v" <~ Pairs.second (var "p") $
              "existing" <~ Maps.lookup (var "table") (var "m") $
              "current" <~ Maybes.fromMaybe (pair (list ([] :: [TTerm (PG.Vertex Term)])) (list ([] :: [TTerm (PG.Edge Term)]))) (var "existing") $
              Maps.insert (var "table")
                (pair
                  (Lists.cons (var "v") (Pairs.first $ var "current"))
                  (Pairs.second $ var "current"))
                (var "m")) $
            "addEdge" <~ ("m" ~> "p" ~>
              "table" <~ Pairs.first (var "p") $
              "e" <~ Pairs.second (var "p") $
              "existing" <~ Maps.lookup (var "table") (var "m") $
              "current" <~ Maybes.fromMaybe (pair (list ([] :: [TTerm (PG.Vertex Term)])) (list ([] :: [TTerm (PG.Edge Term)]))) (var "existing") $
              Maps.insert (var "table")
                (pair
                  (Pairs.first $ var "current")
                  (Lists.cons (var "e") (Pairs.second $ var "current")))
                (var "m")) $
            "vertexMap" <~ Lists.foldl (var "addVertex") Maps.empty (var "vertexPairs") $
            right $ Lists.foldl (var "addEdge") (var "vertexMap") (var "edgePairs")))

-- | Convert a data row to a record term given a table type
termRowToRecord :: TBinding (Tab.TableType -> Tab.DataRow Term -> Term)
termRowToRecord = define "termRowToRecord" $
  doc "Convert a data row to a record term given a table type" $
  "tableType" ~> "row" ~>
    "tname" <~ (unwrap Rel._RelationName @@ (project Tab._TableType Tab._TableType_name @@ var "tableType")) $
    "colTypes" <~ (project Tab._TableType Tab._TableType_columns @@ var "tableType") $
    "cells" <~ (unwrap Tab._DataRow @@ var "row") $
    Core.termRecord $ Core.record (wrap _Name $ var "tname") $
      Lists.zipWith
        ("colType" ~> "mvalue" ~>
          "cname" <~ (unwrap Rel._ColumnName @@ (project Tab._ColumnType Tab._ColumnType_name @@ var "colType")) $
          Core.field (wrap _Name $ var "cname") (Core.termMaybe $ var "mvalue"))
        (var "colTypes")
        (var "cells")

-- | Transform a record through vertex and edge specifications
transformRecord :: TBinding ([PG.Vertex Term] -> [PG.Edge Term] -> Term -> Flow Graph ([PG.Vertex Term], [PG.Edge Term]))
transformRecord = define "transformRecord" $
  doc "Transform a record through vertex and edge specifications to produce vertices and edges" $
  "vspecs" ~> "especs" ~> "record" ~>
    Flows.bind
      (Flows.mapList ("spec" ~> evaluateVertex @@ var "spec" @@ var "record") (var "vspecs"))
      ("mVertices" ~>
        Flows.bind
          (Flows.mapList ("spec" ~> evaluateEdge @@ var "spec" @@ var "record") (var "especs"))
          ("mEdges" ~>
            Flows.pure $ pair (Maybes.cat $ var "mVertices") (Maybes.cat $ var "mEdges")))
