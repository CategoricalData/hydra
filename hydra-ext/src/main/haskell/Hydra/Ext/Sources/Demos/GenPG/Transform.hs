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
import qualified Hydra.Sources.Kernel.Types.Relational as RelationalModel
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
    (kernelTypesNamespaces L.++ [PgModel.ns, TabularModel.ns, RelationalModel.ns]) $  -- type dependencies
    Just "Functions for transforming property graph mappings into property graph elements."
  where
    elements = [
      toBinding concatPairs,
      toBinding decodeCell,
      toBinding decodeRow,
      toBinding decodeTable,
      toBinding elementIsEdge,
      toBinding elementIsVertex,
      toBinding elementSpecsByTable,
      toBinding evaluateEdge,
      toBinding evaluateProperties,
      toBinding evaluateVertex,
      toBinding findTablesInTerm,
      toBinding findTablesInTerms,
      toBinding listAny,
      toBinding makeLazyGraph,
      toBinding normalizeField,
      toBinding parseCsvChar,
      toBinding parseCsvLine,
      toBinding parseSingleLine,
      toBinding parseTableLines,
      toBinding stripWhitespace,
      toBinding tableForEdge,
      toBinding tableForVertex,
      toBinding tableTypesByName,
      toBinding termRowToRecord,
      toBinding transformRecord,
      toBinding transformTableRows]

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
    -- Lift the match extractor outside the inner lambda to avoid Python inline match issues
    -- This takes the key as parameter so it doesn't need to capture it
    "extractMaybe" <~ ("k" ~> "term" ~>
      match _Term Nothing [
        _Term_maybe>>: "mv" ~>
          Flows.pure $ Maybes.map ("v" ~> pair (var "k") (var "v")) (var "mv")]
      @@ var "term") $
    Flows.map
      ("pairs" ~> Maps.fromList $ Maybes.cat $ var "pairs")
      (Flows.mapList
        ("pair" ~>
          "k" <~ Pairs.first (var "pair") $
          "spec" <~ Pairs.second (var "pair") $
          Flows.bind
            (Reduction.reduceTerm @@ boolean True @@ (Core.termApplication $ Core.application (var "spec") (var "record")))
            ("value" ~> var "extractMaybe" @@ var "k" @@ (Rewriting.deannotateTerm @@ var "value")))
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

-- | Check if an element is an edge
elementIsEdge :: TBinding (PG.Element a -> Bool)
elementIsEdge = define "elementIsEdge" $
  doc "Check if an element is an edge" $
  "el" ~>
    match PG._Element (Just $ boolean False) [
      PG._Element_edge>>: constant $ boolean True]
    @@ var "el"

-- | Check if an element is a vertex
elementIsVertex :: TBinding (PG.Element a -> Bool)
elementIsVertex = define "elementIsVertex" $
  doc "Check if an element is a vertex" $
  "el" ~>
    match PG._Element (Just $ boolean False) [
      PG._Element_vertex>>: constant $ boolean True]
    @@ var "el"

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

--------------------------------------------------------------------------------
-- CSV Parsing (pure functions)

-- | Parse a CSV line into a list of optional string values
-- Empty fields become Nothing, non-empty fields become Just value
-- Handles quoted fields (double-quote to escape quotes within quoted fields)
parseCsvLine :: TBinding (String -> Either String [Maybe String])
parseCsvLine = define "parseCsvLine" $
  doc "Parse a CSV line into fields. Empty fields become Nothing." $
  "line" ~>
    -- State is (accumulator, currentField, inQuotes) as a nested pair
    -- ((acc, field), inQuotes)
    "chars" <~ Strings.toList (var "line") $
    "initState" <~ pair (pair (list ([] :: [TTerm (Maybe String)])) (string "")) (boolean False) $
    "finalState" <~ Lists.foldl parseCsvChar (var "initState") (var "chars") $
    -- Extract final state
    "acc" <~ Pairs.first (Pairs.first $ var "finalState") $
    "field" <~ Pairs.second (Pairs.first $ var "finalState") $
    "inQuotes" <~ Pairs.second (var "finalState") $
    -- Finalize: check for unclosed quote, add final field
    Logic.ifElse (var "inQuotes")
      (left $ string "Unclosed quoted field")
      (right $ Lists.reverse $ Lists.cons (normalizeField @@ var "field") (var "acc"))

-- | Process a single character during CSV parsing
-- State: ((accumulator, currentField), inQuotes)
parseCsvChar :: TBinding ((([Maybe String], String), Bool) -> Int -> (([Maybe String], String), Bool))
parseCsvChar = define "parseCsvChar" $
  doc "Process a single character during CSV parsing" $
  "state" ~> "c" ~>
    "acc" <~ Pairs.first (Pairs.first $ var "state") $
    "field" <~ Pairs.second (Pairs.first $ var "state") $
    "inQuotes" <~ Pairs.second (var "state") $
    Logic.ifElse (Equality.equal (var "c") (int32 34))  -- '"' = 34
      (-- Quote character
        Logic.ifElse (var "inQuotes")
          (-- Inside quotes - this ends the quoted section
            pair (pair (var "acc") (var "field")) (boolean False))
          (-- Not inside quotes - start quoted section (only if field is empty)
            Logic.ifElse (Strings.null $ var "field")
              (pair (pair (var "acc") (var "field")) (boolean True))
              (-- Quote inside non-empty unquoted field - just add it (simplified behavior)
                pair (pair (var "acc") (Strings.cat2 (var "field") (string "\""))) (var "inQuotes"))))
      (-- Not a quote
        Logic.ifElse (Logic.and (Equality.equal (var "c") (int32 44)) (Logic.not $ var "inQuotes"))  -- ',' = 44
          (-- Comma outside quotes - end of field
            pair (pair (Lists.cons (normalizeField @@ var "field") (var "acc")) (string "")) (boolean False))
          (-- Regular character - append to field
            pair (pair (var "acc") (Strings.cat2 (var "field") (Strings.fromList $ list [var "c"]))) (var "inQuotes")))

-- | Normalize a CSV field - empty string becomes Nothing, otherwise Just
normalizeField :: TBinding (String -> Maybe String)
normalizeField = define "normalizeField" $
  doc "Normalize a CSV field value - empty becomes Nothing" $
  "s" ~>
    Logic.ifElse (Strings.null $ var "s")
      nothing
      (just $ var "s")

-- | Concatenate two pairs of lists (used for accumulating vertices and edges)
concatPairs :: TBinding (([a], [b]) -> ([a], [b]) -> ([a], [b]))
concatPairs = define "concatPairs" $
  doc "Concatenate two pairs of lists" $
  "acc" ~> "p" ~>
    pair
      (Lists.concat2 (Pairs.first $ var "acc") (Pairs.first $ var "p"))
      (Lists.concat2 (Pairs.second $ var "acc") (Pairs.second $ var "p"))

-- | Build a map from table name to table type for efficient lookup
tableTypesByName :: TBinding ([Tab.TableType] -> M.Map Rel.RelationName Tab.TableType)
tableTypesByName = define "tableTypesByName" $
  doc "Build a map from table name to table type" $
  "tableTypes" ~>
    Maps.fromList $ Lists.map
      ("t" ~> pair (project Tab._TableType Tab._TableType_name @@ var "t") (var "t"))
      (var "tableTypes")

-- | Strip leading and trailing whitespace from a string
stripWhitespace :: TBinding (String -> String)
stripWhitespace = define "stripWhitespace" $
  doc "Strip leading and trailing whitespace from a string" $
  "s" ~>
    -- Convert to list of chars, drop leading spaces, reverse, drop leading spaces, reverse back
    "chars" <~ Strings.toList (var "s") $
    "isSpaceChar" <~ ("c" ~> Chars.isSpace (var "c")) $
    "trimLeft" <~ Lists.dropWhile (var "isSpaceChar") (var "chars") $
    "trimRight" <~ Lists.reverse (Lists.dropWhile (var "isSpaceChar") (Lists.reverse $ var "trimLeft")) $
    Strings.fromList (var "trimRight")

-- | Check if any element in a list satisfies a predicate
listAny :: TBinding ((a -> Bool) -> [a] -> Bool)
listAny = define "listAny" $
  doc "Check if any element in a list satisfies a predicate" $
  "pred" ~> "xs" ~>
    Logic.not $ Lists.null $ Lists.filter (var "pred") (var "xs")

-- | Parse a single CSV line, returning Nothing for empty/whitespace-only lines
parseSingleLine :: TBinding (String -> Either String (Maybe [Maybe String]))
parseSingleLine = define "parseSingleLine" $
  doc "Parse a single CSV line, returning Nothing for empty lines" $
  "line" ~>
    "trimmed" <~ (stripWhitespace @@ var "line") $
    Logic.ifElse (Strings.null $ var "trimmed")
      (right nothing)
      (Eithers.map ("x" ~> just (var "x")) (parseCsvLine @@ var "trimmed"))

-- | Parse raw CSV lines into a Table of strings
-- Takes: hasHeader flag, list of raw lines
-- Returns: Either error or Table String
parseTableLines :: TBinding (Bool -> [String] -> Either String (Tab.Table String))
parseTableLines = define "parseTableLines" $
  doc "Parse raw CSV lines into a Table of strings" $
  "hasHeader" ~> "rawLines" ~>
    -- Parse each line (returns Either String (Maybe [Maybe String]) for each)
    Eithers.bind
      (Eithers.mapList ("ln" ~> parseSingleLine @@ var "ln") (var "rawLines"))
      ("parsedRows" ~>
        -- Filter out empty lines (Nothing values) to get [[Maybe String]]
        "rows" <~ Maybes.cat (var "parsedRows") $
        -- Build the table based on hasHeader flag
        Logic.ifElse (var "hasHeader")
          (-- With header: first row is header, rest are data
            "headerRow" <~ Lists.head (var "rows") $
            "dataRows" <~ Lists.tail (var "rows") $
            -- Check for null headers
            Logic.ifElse (listAny @@ ("m" ~> Maybes.isNothing (var "m")) @@ var "headerRow")
              (left $ string "null header column(s)")
              (right $ record Tab._Table [
                Tab._Table_header>>: just (wrap Tab._HeaderRow $ Maybes.cat $ var "headerRow"),
                Tab._Table_data>>: Lists.map ("r" ~> wrap Tab._DataRow (var "r")) (var "dataRows")]))
          (-- No header: all rows are data
            right $ record Tab._Table [
              Tab._Table_header>>: nothing,
              Tab._Table_data>>: Lists.map ("r" ~> wrap Tab._DataRow (var "r")) (var "rows")]))

-- | Transform all rows from a decoded table through vertex/edge specs
-- This is the pure part of table transformation (runs in Flow monad)
transformTableRows :: TBinding ([PG.Vertex Term] -> [PG.Edge Term] -> Tab.TableType -> [Tab.DataRow Term] -> Flow Graph ([PG.Vertex Term], [PG.Edge Term]))
transformTableRows = define "transformTableRows" $
  doc "Transform all rows from a table through vertex/edge specifications" $
  "vspecs" ~> "especs" ~> "tableType" ~> "rows" ~>
    Flows.map
      ("pairs" ~> Lists.foldl concatPairs (pair (list ([] :: [TTerm (PG.Vertex Term)])) (list ([] :: [TTerm (PG.Edge Term)]))) (var "pairs"))
      (Flows.mapList
        ("row" ~> transformRecord @@ var "vspecs" @@ var "especs" @@ (termRowToRecord @@ var "tableType" @@ var "row"))
        (var "rows"))

-- | Construct a LazyGraph from lists of vertices and edges
makeLazyGraph :: TBinding ([PG.Vertex Term] -> [PG.Edge Term] -> PG.LazyGraph Term)
makeLazyGraph = define "makeLazyGraph" $
  doc "Construct a LazyGraph from vertices and edges" $
  "vertices" ~> "edges" ~>
    record PG._LazyGraph [
      PG._LazyGraph_vertices>>: var "vertices",
      PG._LazyGraph_edges>>: var "edges"]

--------------------------------------------------------------------------------
-- Table Decoding (pure functions)

-- | Decode a table of strings into a table of terms based on column types
decodeTable :: TBinding (Tab.TableType -> Tab.Table String -> Either String (Tab.Table Term))
decodeTable = define "decodeTable" $
  doc "Decode a table of strings into a table of terms based on column type specifications" $
  "tableType" ~> "table" ~>
    "colTypes" <~ (project Tab._TableType Tab._TableType_columns @@ var "tableType") $
    "header" <~ (project Tab._Table Tab._Table_header @@ var "table") $
    "rows" <~ (project Tab._Table Tab._Table_data @@ var "table") $
    Eithers.map
      ("decodedRows" ~>
        record Tab._Table [
          Tab._Table_header>>: var "header",
          Tab._Table_data>>: var "decodedRows"])
      (Eithers.mapList
        ("row" ~> decodeRow @@ var "colTypes" @@ var "row")
        (var "rows"))

-- | Decode a single row based on column types
decodeRow :: TBinding ([Tab.ColumnType] -> Tab.DataRow String -> Either String (Tab.DataRow Term))
decodeRow = define "decodeRow" $
  doc "Decode a single data row based on column types" $
  "colTypes" ~> "row" ~>
    "cells" <~ (unwrap Tab._DataRow @@ var "row") $
    Eithers.map
      ("decodedCells" ~> wrap Tab._DataRow (var "decodedCells"))
      (Eithers.mapList
        ("pair" ~>
          "colType" <~ Pairs.first (var "pair") $
          "mvalue" <~ Pairs.second (var "pair") $
          decodeCell @@ var "colType" @@ var "mvalue")
        (Lists.zip (var "colTypes") (var "cells")))

-- | Decode a single cell value based on its column type
decodeCell :: TBinding (Tab.ColumnType -> Maybe String -> Either String (Maybe Term))
decodeCell = define "decodeCell" $
  doc "Decode a single cell value based on its column type" $
  "colType" ~> "mvalue" ~>
    "cname" <~ (unwrap Rel._ColumnName @@ (project Tab._ColumnType Tab._ColumnType_name @@ var "colType")) $
    "typ" <~ (project Tab._ColumnType Tab._ColumnType_type @@ var "colType") $
    -- Lift the decoder function to a let binding before Maybes.maybe
    -- This avoids Python issues with match statements inside inline lambdas
    "decodeValue" <~ ("value" ~>
      "parseError" <~ (Strings.cat $ list [
        string "Invalid value for column ",
        var "cname",
        string ": ",
        var "value"]) $
      match _Type (Just $ left $ Strings.cat $ list [
        string "Unsupported type for column ",
        var "cname"]) [
        _Type_literal>>: "lt" ~>
          match _LiteralType (Just $ left $ Strings.cat $ list [
            string "Unsupported literal type for column ",
            var "cname"]) [
            _LiteralType_boolean>>: constant $
              Maybes.maybe
                (left $ var "parseError")
                ("parsed" ~> right $ just $ Core.termLiteral $ Core.literalBoolean $ var "parsed")
                (Literals.readBoolean $ var "value"),
            _LiteralType_float>>: "ft" ~>
              match _FloatType (Just $ left $ Strings.cat $ list [
                string "Unsupported float type for column ",
                var "cname"]) [
                _FloatType_bigfloat>>: constant $
                  Maybes.maybe
                    (left $ var "parseError")
                    ("parsed" ~> right $ just $ Core.termLiteral $ Core.literalFloat $ Core.floatValueBigfloat $ var "parsed")
                    (Literals.readBigfloat $ var "value"),
                _FloatType_float32>>: constant $
                  Maybes.maybe
                    (left $ var "parseError")
                    ("parsed" ~> right $ just $ Core.termLiteral $ Core.literalFloat $ Core.floatValueFloat32 $ var "parsed")
                    (Literals.readFloat32 $ var "value"),
                _FloatType_float64>>: constant $
                  Maybes.maybe
                    (left $ var "parseError")
                    ("parsed" ~> right $ just $ Core.termLiteral $ Core.literalFloat $ Core.floatValueFloat64 $ var "parsed")
                    (Literals.readFloat64 $ var "value")]
              @@ var "ft",
            _LiteralType_integer>>: "it" ~>
              match _IntegerType (Just $ left $ Strings.cat $ list [
                string "Unsupported integer type for column ",
                var "cname"]) [
                _IntegerType_int32>>: constant $
                  Maybes.maybe
                    (left $ var "parseError")
                    ("parsed" ~> right $ just $ Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ var "parsed")
                    (Literals.readInt32 $ var "value"),
                _IntegerType_int64>>: constant $
                  Maybes.maybe
                    (left $ var "parseError")
                    ("parsed" ~> right $ just $ Core.termLiteral $ Core.literalInteger $ Core.integerValueInt64 $ var "parsed")
                    (Literals.readInt64 $ var "value")]
              @@ var "it",
            _LiteralType_string>>: constant $
              right $ just $ Core.termLiteral $ Core.literalString $ var "value"]
          @@ var "lt"]
      @@ var "typ") $
    Maybes.maybe
      (right nothing)  -- No value - return Nothing
      (var "decodeValue")  -- Has value - use the lifted decoder function
      (var "mvalue")
