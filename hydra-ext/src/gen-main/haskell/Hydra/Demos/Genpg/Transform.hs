-- Note: this is an automatically generated file. Do not edit.

-- | Functions for transforming property graph mappings into property graph elements.

module Hydra.Demos.Genpg.Transform where

import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Tabular as Tabular
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Concatenate two pairs of lists
concatPairs :: ([t0], [t1]) -> ([t0], [t1]) -> ([t0], [t1])
concatPairs acc p = (Lists.concat2 (Pairs.first acc) (Pairs.first p), (Lists.concat2 (Pairs.second acc) (Pairs.second p)))

-- | Decode a single cell value based on its column type
decodeCell :: Tabular.ColumnType -> Maybe String -> Either String (Maybe Core.Term)
decodeCell colType mvalue =

      let cname = Relational.unColumnName (Tabular.columnTypeName colType)
          typ = Tabular.columnTypeType colType
          decodeValue =
                  \value ->
                    let parseError =
                            Strings.cat [
                              "Invalid value for column ",
                              cname,
                              ": ",
                              value]
                    in case typ of
                      Core.TypeLiteral v0 -> case v0 of
                        Core.LiteralTypeBoolean -> Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralBoolean parsed)))) (Literals.readBoolean value)
                        Core.LiteralTypeFloat v1 -> case v1 of
                          Core.FloatTypeBigfloat -> Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat parsed))))) (Literals.readBigfloat value)
                          Core.FloatTypeFloat32 -> Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 parsed))))) (Literals.readFloat32 value)
                          Core.FloatTypeFloat64 -> Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 parsed))))) (Literals.readFloat64 value)
                          _ -> Left (Strings.cat [
                            "Unsupported float type for column ",
                            cname])
                        Core.LiteralTypeInteger v1 -> case v1 of
                          Core.IntegerTypeInt32 -> Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 parsed))))) (Literals.readInt32 value)
                          Core.IntegerTypeInt64 -> Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 parsed))))) (Literals.readInt64 value)
                          _ -> Left (Strings.cat [
                            "Unsupported integer type for column ",
                            cname])
                        Core.LiteralTypeString -> Right (Just (Core.TermLiteral (Core.LiteralString value)))
                        _ -> Left (Strings.cat [
                          "Unsupported literal type for column ",
                          cname])
                      _ -> Left (Strings.cat [
                        "Unsupported type for column ",
                        cname])
      in (Maybes.maybe (Right Nothing) decodeValue mvalue)

-- | Decode a single data row based on column types
decodeRow :: [Tabular.ColumnType] -> Tabular.DataRow String -> Either String (Tabular.DataRow Core.Term)
decodeRow colTypes row =

      let cells = Tabular.unDataRow row
      in (Eithers.map (\decodedCells -> Tabular.DataRow decodedCells) (Eithers.mapList (\pair ->
        let colType = Pairs.first pair
            mvalue = Pairs.second pair
        in (decodeCell colType mvalue)) (Lists.zip colTypes cells)))

-- | Decode a table of strings into a table of terms based on column type specifications
decodeTable :: Tabular.TableType -> Tabular.Table String -> Either String (Tabular.Table Core.Term)
decodeTable tableType table =

      let colTypes = Tabular.tableTypeColumns tableType
          header = Tabular.tableHeader table
          rows = Tabular.tableData table
      in (Eithers.map (\decodedRows -> Tabular.Table {
        Tabular.tableHeader = header,
        Tabular.tableData = decodedRows}) (Eithers.mapList (\row -> decodeRow colTypes row) rows))

-- | Check if an element is an edge
elementIsEdge :: Model.Element t0 -> Bool
elementIsEdge el =
    (\x -> case x of
      Model.ElementEdge _ -> True
      _ -> False) el

-- | Check if an element is a vertex
elementIsVertex :: Model.Element t0 -> Bool
elementIsVertex el =
    (\x -> case x of
      Model.ElementVertex _ -> True
      _ -> False) el

-- | Group element specifications by their source table
elementSpecsByTable :: Model.LazyGraph Core.Term -> Either String (M.Map String ([Model.Vertex Core.Term], [Model.Edge Core.Term]))
elementSpecsByTable graph =

      let vertices = Model.lazyGraphVertices graph
          edges = Model.lazyGraphEdges graph
      in (Eithers.bind (Eithers.mapList (\v -> Eithers.map (\t -> (t, v)) (tableForVertex v)) vertices) (\vertexPairs -> Eithers.bind (Eithers.mapList (\e -> Eithers.map (\t -> (t, e)) (tableForEdge e)) edges) (\edgePairs ->
        let addVertex =
                \m -> \p ->
                  let table = Pairs.first p
                      v = Pairs.second p
                      existing = Maps.lookup table m
                      current = Maybes.fromMaybe ([], []) existing
                  in (Maps.insert table (Lists.cons v (Pairs.first current), (Pairs.second current)) m)
            addEdge =
                    \m -> \p ->
                      let table = Pairs.first p
                          e = Pairs.second p
                          existing = Maps.lookup table m
                          current = Maybes.fromMaybe ([], []) existing
                      in (Maps.insert table (Pairs.first current, (Lists.cons e (Pairs.second current))) m)
            vertexMap = Lists.foldl addVertex Maps.empty vertexPairs
        in (Right (Lists.foldl addEdge vertexMap edgePairs)))))

-- | Evaluate an edge specification against a record term to produce an optional edge
evaluateEdge :: Context.Context -> Graph.Graph -> Model.Edge Core.Term -> Core.Term -> Either (Context.InContext Error.Error) (Maybe (Model.Edge Core.Term))
evaluateEdge cx g edgeSpec record =

      let label = Model.edgeLabel edgeSpec
          idSpec = Model.edgeId edgeSpec
          outSpec = Model.edgeOut edgeSpec
          inSpec = Model.edgeIn edgeSpec
          propSpecs = Model.edgeProperties edgeSpec
      in (Eithers.bind (Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
        Core.applicationFunction = idSpec,
        Core.applicationArgument = record}))) (\id -> Eithers.bind (Eithers.bind (Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
        Core.applicationFunction = outSpec,
        Core.applicationArgument = record}))) (\_term -> Core_.maybeTerm cx (\t -> Right t) g _term)) (\mOutId -> Eithers.bind (Eithers.bind (Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
        Core.applicationFunction = inSpec,
        Core.applicationArgument = record}))) (\_term -> Core_.maybeTerm cx (\t -> Right t) g _term)) (\mInId -> Eithers.bind (evaluateProperties cx g propSpecs record) (\props -> Right (Maybes.bind mOutId (\outId -> Maybes.map (\inId -> Model.Edge {
        Model.edgeLabel = label,
        Model.edgeId = id,
        Model.edgeOut = outId,
        Model.edgeIn = inId,
        Model.edgeProperties = props}) mInId)))))))

-- | Evaluate property specifications against a record term
evaluateProperties :: Ord t0 => (Context.Context -> Graph.Graph -> M.Map t0 Core.Term -> Core.Term -> Either (Context.InContext Error.Error) (M.Map t0 Core.Term))
evaluateProperties cx g specs record =

      let extractMaybe =
              \k -> \term -> case term of
                Core.TermMaybe v0 -> Right (Maybes.map (\v -> (k, v)) v0)
      in (Eithers.map (\pairs -> Maps.fromList (Maybes.cat pairs)) (Eithers.mapList (\pair ->
        let k = Pairs.first pair
            spec = Pairs.second pair
        in (Eithers.bind (Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
          Core.applicationFunction = spec,
          Core.applicationArgument = record}))) (\value -> extractMaybe k (Rewriting.deannotateTerm value)))) (Maps.toList specs)))

-- | Evaluate a vertex specification against a record term to produce an optional vertex
evaluateVertex :: Context.Context -> Graph.Graph -> Model.Vertex Core.Term -> Core.Term -> Either (Context.InContext Error.Error) (Maybe (Model.Vertex Core.Term))
evaluateVertex cx g vertexSpec record =

      let label = Model.vertexLabel vertexSpec
          idSpec = Model.vertexId vertexSpec
          propSpecs = Model.vertexProperties vertexSpec
      in (Eithers.bind (Eithers.bind (Reduction.reduceTerm cx g True (Core.TermApplication (Core.Application {
        Core.applicationFunction = idSpec,
        Core.applicationArgument = record}))) (\_term -> Core_.maybeTerm cx (\t -> Right t) g _term)) (\mId -> Eithers.bind (evaluateProperties cx g propSpecs record) (\props -> Right (Maybes.map (\id -> Model.Vertex {
        Model.vertexLabel = label,
        Model.vertexId = id,
        Model.vertexProperties = props}) mId))))

-- | Find table names referenced in a term by looking for record projections
findTablesInTerm :: Core.Term -> S.Set String
findTablesInTerm term =
    Rewriting.foldOverTerm Coders.TraversalOrderPre (\names -> \t -> case t of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination v1 -> case v1 of
          Core.EliminationRecord v2 -> Sets.insert (Core.unName (Core.projectionTypeName v2)) names
          _ -> names
        _ -> names
      _ -> names) Sets.empty term

-- | Find table names referenced in multiple terms
findTablesInTerms :: [Core.Term] -> S.Set String
findTablesInTerms terms = Sets.unions (Lists.map findTablesInTerm terms)

-- | Check if any element in a list satisfies a predicate
listAny :: (t0 -> Bool) -> [t0] -> Bool
listAny pred xs = Logic.not (Lists.null (Lists.filter pred xs))

-- | Construct a LazyGraph from vertices and edges
makeLazyGraph :: [Model.Vertex t0] -> [Model.Edge t0] -> Model.LazyGraph t0
makeLazyGraph vertices edges =
    Model.LazyGraph {
      Model.lazyGraphVertices = vertices,
      Model.lazyGraphEdges = edges}

-- | Normalize a CSV field value - empty becomes Nothing
normalizeField :: String -> Maybe String
normalizeField s = Logic.ifElse (Strings.null s) Nothing (Just s)

-- | Process a single character during CSV parsing
parseCsvChar :: (([Maybe String], String), Bool) -> Int -> (([Maybe String], String), Bool)
parseCsvChar state c =

      let acc = Pairs.first (Pairs.first state)
          field = Pairs.second (Pairs.first state)
          inQuotes = Pairs.second state
      in (Logic.ifElse (Equality.equal c 34) (Logic.ifElse inQuotes ((acc, field), False) (Logic.ifElse (Strings.null field) ((acc, field), True) ((acc, (Strings.cat2 field "\"")), inQuotes))) (Logic.ifElse (Logic.and (Equality.equal c 44) (Logic.not inQuotes)) ((Lists.cons (normalizeField field) acc, ""), False) ((acc, (Strings.cat2 field (Strings.fromList [
        c]))), inQuotes)))

-- | Parse a CSV line into fields. Empty fields become Nothing.
parseCsvLine :: String -> Either String [Maybe String]
parseCsvLine line =

      let chars = Strings.toList line
          initState = (([], ""), False)
          finalState = Lists.foldl parseCsvChar initState chars
          acc = Pairs.first (Pairs.first finalState)
          field = Pairs.second (Pairs.first finalState)
          inQuotes = Pairs.second finalState
      in (Logic.ifElse inQuotes (Left "Unclosed quoted field") (Right (Lists.reverse (Lists.cons (normalizeField field) acc))))

-- | Parse a single CSV line, returning Nothing for empty lines
parseSingleLine :: String -> Either String (Maybe [Maybe String])
parseSingleLine line =

      let trimmed = stripWhitespace line
      in (Logic.ifElse (Strings.null trimmed) (Right Nothing) (Eithers.map (\x -> Just x) (parseCsvLine trimmed)))

-- | Parse raw CSV lines into a Table of strings
parseTableLines :: Bool -> [String] -> Either String (Tabular.Table String)
parseTableLines hasHeader rawLines =
    Eithers.bind (Eithers.mapList (\ln -> parseSingleLine ln) rawLines) (\parsedRows ->
      let rows = Maybes.cat parsedRows
      in (Logic.ifElse hasHeader (
        let headerRow = Lists.head rows
            dataRows = Lists.tail rows
        in (Logic.ifElse (listAny (\m -> Maybes.isNothing m) headerRow) (Left "null header column(s)") (Right (Tabular.Table {
          Tabular.tableHeader = (Just (Tabular.HeaderRow (Maybes.cat headerRow))),
          Tabular.tableData = (Lists.map (\r -> Tabular.DataRow r) dataRows)})))) (Right (Tabular.Table {
        Tabular.tableHeader = Nothing,
        Tabular.tableData = (Lists.map (\r -> Tabular.DataRow r) rows)}))))

-- | Strip leading and trailing whitespace from a string
stripWhitespace :: String -> String
stripWhitespace s =

      let chars = Strings.toList s
          isSpaceChar = \c -> Chars.isSpace c
          trimLeft = Lists.dropWhile isSpaceChar chars
          trimRight = Lists.reverse (Lists.dropWhile isSpaceChar (Lists.reverse trimLeft))
      in (Strings.fromList trimRight)

-- | Get the table name for an edge specification. Returns an error if not exactly one table is referenced.
tableForEdge :: Model.Edge Core.Term -> Either String String
tableForEdge edge =

      let label = Model.edgeLabel edge
          id = Model.edgeId edge
          outId = Model.edgeOut edge
          inId = Model.edgeIn edge
          props = Model.edgeProperties edge
          tables =
                  findTablesInTerms (Lists.concat2 [
                    id,
                    outId,
                    inId] (Maps.elems props))
      in (Logic.ifElse (Equality.equal (Sets.size tables) 1) (Right (Lists.head (Sets.toList tables))) (Left (Strings.cat [
        "Specification for ",
        (Model.unEdgeLabel label),
        " edges has wrong number of tables"])))

-- | Get the table name for a vertex specification. Returns an error if not exactly one table is referenced.
tableForVertex :: Model.Vertex Core.Term -> Either String String
tableForVertex vertex =

      let label = Model.vertexLabel vertex
          id = Model.vertexId vertex
          props = Model.vertexProperties vertex
          tables = findTablesInTerms (Lists.cons id (Maps.elems props))
      in (Logic.ifElse (Equality.equal (Sets.size tables) 1) (Right (Lists.head (Sets.toList tables))) (Left (Strings.cat [
        "Specification for ",
        (Model.unVertexLabel label),
        " vertices has wrong number of tables"])))

-- | Build a map from table name to table type
tableTypesByName :: [Tabular.TableType] -> M.Map Relational.RelationName Tabular.TableType
tableTypesByName tableTypes = Maps.fromList (Lists.map (\t -> (Tabular.tableTypeName t, t)) tableTypes)

-- | Convert a data row to a record term given a table type
termRowToRecord :: Tabular.TableType -> Tabular.DataRow Core.Term -> Core.Term
termRowToRecord tableType row =

      let tname = Relational.unRelationName (Tabular.tableTypeName tableType)
          colTypes = Tabular.tableTypeColumns tableType
          cells = Tabular.unDataRow row
      in (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name tname),
        Core.recordFields = (Lists.zipWith (\colType -> \mvalue ->
          let cname = Relational.unColumnName (Tabular.columnTypeName colType)
          in Core.Field {
            Core.fieldName = (Core.Name cname),
            Core.fieldTerm = (Core.TermMaybe mvalue)}) colTypes cells)}))

-- | Transform a record through vertex and edge specifications to produce vertices and edges
transformRecord :: Context.Context -> Graph.Graph -> [Model.Vertex Core.Term] -> [Model.Edge Core.Term] -> Core.Term -> Either (Context.InContext Error.Error) ([Model.Vertex Core.Term], [Model.Edge Core.Term])
transformRecord cx g vspecs especs record =
    Eithers.bind (Eithers.mapList (\spec -> evaluateVertex cx g spec record) vspecs) (\mVertices -> Eithers.bind (Eithers.mapList (\spec -> evaluateEdge cx g spec record) especs) (\mEdges -> Right (Maybes.cat mVertices, (Maybes.cat mEdges))))

-- | Transform all rows from a table through vertex/edge specifications
transformTableRows :: Context.Context -> Graph.Graph -> [Model.Vertex Core.Term] -> [Model.Edge Core.Term] -> Tabular.TableType -> [Tabular.DataRow Core.Term] -> Either (Context.InContext Error.Error) ([Model.Vertex Core.Term], [Model.Edge Core.Term])
transformTableRows cx g vspecs especs tableType rows =
    Eithers.map (\pairs -> Lists.foldl concatPairs ([], []) pairs) (Eithers.mapList (\row -> transformRecord cx g vspecs especs (termRowToRecord tableType row)) rows)
