-- Note: this is an automatically generated file. Do not edit.

-- | Functions for transforming property graph mappings into property graph elements.

module Hydra.Demos.Genpg.Transform where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
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

concatPairs :: (([t0], [t1]) -> ([t0], [t1]) -> ([t0], [t1]))
concatPairs acc p = (Lists.concat2 (Pairs.first acc) (Pairs.first p), (Lists.concat2 (Pairs.second acc) (Pairs.second p)))

-- | Decode a single cell value based on its column type
decodeCell :: (Tabular.ColumnType -> Maybe String -> Either String (Maybe Core.Term))
decodeCell colType mvalue =  
  let cname = (Relational.unColumnName (Tabular.columnTypeName colType))
  in  
    let typ = (Tabular.columnTypeType colType)
    in (Maybes.maybe (Right Nothing) (\value ->  
      let parseError = (Strings.cat [
              "Invalid value for column ",
              cname,
              ": ",
              value])
      in ((\x -> case x of
        Core.TypeLiteral v1 -> ((\x -> case x of
          Core.LiteralTypeBoolean -> (Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralBoolean parsed)))) (Literals.readBoolean value))
          Core.LiteralTypeFloat v2 -> ((\x -> case x of
            Core.FloatTypeBigfloat -> (Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat parsed))))) (Literals.readFloat64 value))
            Core.FloatTypeFloat32 -> (Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 parsed))))) (Literals.readFloat32 value))
            Core.FloatTypeFloat64 -> (Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 parsed))))) (Literals.readFloat64 value))
            _ -> (Left (Strings.cat [
              "Unsupported float type for column ",
              cname]))) v2)
          Core.LiteralTypeInteger v2 -> ((\x -> case x of
            Core.IntegerTypeInt32 -> (Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 parsed))))) (Literals.readInt32 value))
            Core.IntegerTypeInt64 -> (Maybes.maybe (Left parseError) (\parsed -> Right (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 parsed))))) (Literals.readInt64 value))
            _ -> (Left (Strings.cat [
              "Unsupported integer type for column ",
              cname]))) v2)
          Core.LiteralTypeString -> (Right (Just (Core.TermLiteral (Core.LiteralString value))))
          _ -> (Left (Strings.cat [
            "Unsupported literal type for column ",
            cname]))) v1)
        _ -> (Left (Strings.cat [
          "Unsupported type for column ",
          cname]))) typ)) mvalue)

-- | Decode a single data row based on column types
decodeRow :: ([Tabular.ColumnType] -> Tabular.DataRow String -> Either String (Tabular.DataRow Core.Term))
decodeRow colTypes row =  
  let cells = (Tabular.unDataRow row)
  in (Eithers.map (\decodedCells -> Tabular.DataRow decodedCells) (Eithers.mapList (\pair ->  
    let colType = (Pairs.first pair)
    in  
      let mvalue = (Pairs.second pair)
      in (decodeCell colType mvalue)) (Lists.zip colTypes cells)))

-- | Decode a table of strings into a table of terms based on column type specifications
decodeTable :: (Tabular.TableType -> Tabular.Table String -> Either String (Tabular.Table Core.Term))
decodeTable tableType table =  
  let colTypes = (Tabular.tableTypeColumns tableType)
  in  
    let header = (Tabular.tableHeader table)
    in  
      let rows = (Tabular.tableData table)
      in (Eithers.map (\decodedRows -> Tabular.Table {
        Tabular.tableHeader = header,
        Tabular.tableData = decodedRows}) (Eithers.mapList (\row -> decodeRow colTypes row) rows))

elementIsEdge :: (Model.Element t0 -> Bool)
elementIsEdge el = ((\x -> case x of
  Model.ElementEdge _ -> True
  _ -> False) el)

elementIsVertex :: (Model.Element t0 -> Bool)
elementIsVertex el = ((\x -> case x of
  Model.ElementVertex _ -> True
  _ -> False) el)

-- | Group element specifications by their source table
elementSpecsByTable :: (Model.LazyGraph Core.Term -> Either String (M.Map String ([Model.Vertex Core.Term], [Model.Edge Core.Term])))
elementSpecsByTable graph =  
  let vertices = (Model.lazyGraphVertices graph)
  in  
    let edges = (Model.lazyGraphEdges graph)
    in (Eithers.bind (Eithers.mapList (\v -> Eithers.map (\t -> (t, v)) (tableForVertex v)) vertices) (\vertexPairs -> Eithers.bind (Eithers.mapList (\e -> Eithers.map (\t -> (t, e)) (tableForEdge e)) edges) (\edgePairs ->  
      let addVertex = (\m -> \p ->  
              let table = (Pairs.first p)
              in  
                let v = (Pairs.second p)
                in  
                  let existing = (Maps.lookup table m)
                  in  
                    let current = (Maybes.fromMaybe ([], []) existing)
                    in (Maps.insert table (Lists.cons v (Pairs.first current), (Pairs.second current)) m))
      in  
        let addEdge = (\m -> \p ->  
                let table = (Pairs.first p)
                in  
                  let e = (Pairs.second p)
                  in  
                    let existing = (Maps.lookup table m)
                    in  
                      let current = (Maybes.fromMaybe ([], []) existing)
                      in (Maps.insert table (Pairs.first current, (Lists.cons e (Pairs.second current))) m))
        in  
          let vertexMap = (Lists.foldl addVertex Maps.empty vertexPairs)
          in (Right (Lists.foldl addEdge vertexMap edgePairs)))))

-- | Evaluate an edge specification against a record term to produce an optional edge
evaluateEdge :: (Model.Edge Core.Term -> Core.Term -> Compute.Flow Graph.Graph (Maybe (Model.Edge Core.Term)))
evaluateEdge edgeSpec record =  
  let label = (Model.edgeLabel edgeSpec)
  in  
    let idSpec = (Model.edgeId edgeSpec)
    in  
      let outSpec = (Model.edgeOut edgeSpec)
      in  
        let inSpec = (Model.edgeIn edgeSpec)
        in  
          let propSpecs = (Model.edgeProperties edgeSpec)
          in (Flows.bind (Reduction.reduceTerm True (Core.TermApplication (Core.Application {
            Core.applicationFunction = idSpec,
            Core.applicationArgument = record}))) (\id -> Flows.bind (Flows.bind (Reduction.reduceTerm True (Core.TermApplication (Core.Application {
            Core.applicationFunction = outSpec,
            Core.applicationArgument = record}))) (Core_.maybeTerm (\t -> Flows.pure t))) (\mOutId -> Flows.bind (Flows.bind (Reduction.reduceTerm True (Core.TermApplication (Core.Application {
            Core.applicationFunction = inSpec,
            Core.applicationArgument = record}))) (Core_.maybeTerm (\t -> Flows.pure t))) (\mInId -> Flows.bind (evaluateProperties propSpecs record) (\props -> Flows.pure (Maybes.bind mOutId (\outId -> Maybes.map (\inId -> Model.Edge {
            Model.edgeLabel = label,
            Model.edgeId = id,
            Model.edgeOut = outId,
            Model.edgeIn = inId,
            Model.edgeProperties = props}) mInId)))))))

evaluateProperties :: (Ord t0) => (M.Map t0 Core.Term -> Core.Term -> Compute.Flow Graph.Graph (M.Map t0 Core.Term))
evaluateProperties specs record = (Flows.map (\pairs -> Maps.fromList (Maybes.cat pairs)) (Flows.mapList (\pair ->  
  let k = (Pairs.first pair)
  in  
    let spec = (Pairs.second pair)
    in (Flows.bind (Reduction.reduceTerm True (Core.TermApplication (Core.Application {
      Core.applicationFunction = spec,
      Core.applicationArgument = record}))) (\value -> (\x -> case x of
      Core.TermMaybe v1 -> (Flows.pure (Maybes.map (\v -> (k, v)) v1))) (Rewriting.deannotateTerm value)))) (Maps.toList specs)))

-- | Evaluate a vertex specification against a record term to produce an optional vertex
evaluateVertex :: (Model.Vertex Core.Term -> Core.Term -> Compute.Flow Graph.Graph (Maybe (Model.Vertex Core.Term)))
evaluateVertex vertexSpec record =  
  let label = (Model.vertexLabel vertexSpec)
  in  
    let idSpec = (Model.vertexId vertexSpec)
    in  
      let propSpecs = (Model.vertexProperties vertexSpec)
      in (Flows.bind (Flows.bind (Reduction.reduceTerm True (Core.TermApplication (Core.Application {
        Core.applicationFunction = idSpec,
        Core.applicationArgument = record}))) (Core_.maybeTerm (\t -> Flows.pure t))) (\mId -> Flows.bind (evaluateProperties propSpecs record) (\props -> Flows.pure (Maybes.map (\id -> Model.Vertex {
        Model.vertexLabel = label,
        Model.vertexId = id,
        Model.vertexProperties = props}) mId))))

-- | Find table names referenced in a term by looking for record projections
findTablesInTerm :: (Core.Term -> S.Set String)
findTablesInTerm term = (Rewriting.foldOverTerm Coders.TraversalOrderPre (\names -> \t -> (\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationRecord v3 -> (Sets.insert (Core.unName (Core.projectionTypeName v3)) names)
      _ -> names) v2)
    _ -> names) v1)
  _ -> names) t) Sets.empty term)

-- | Find table names referenced in multiple terms
findTablesInTerms :: ([Core.Term] -> S.Set String)
findTablesInTerms terms = (Sets.unions (Lists.map findTablesInTerm terms))

listAny :: ((t0 -> Bool) -> [t0] -> Bool)
listAny pred xs = (Logic.not (Lists.null (Lists.filter pred xs)))

makeLazyGraph :: ([Model.Vertex t0] -> [Model.Edge t0] -> Model.LazyGraph t0)
makeLazyGraph vertices edges = Model.LazyGraph {
  Model.lazyGraphVertices = vertices,
  Model.lazyGraphEdges = edges}

-- | Normalize a CSV field value - empty becomes Nothing
normalizeField :: (String -> Maybe String)
normalizeField s = (Logic.ifElse (Strings.null s) Nothing (Just s))

-- | Process a single character during CSV parsing
parseCsvChar :: ((([Maybe String], String), Bool) -> Int -> (([Maybe String], String), Bool))
parseCsvChar state c =  
  let acc = (Pairs.first (Pairs.first state))
  in  
    let field = (Pairs.second (Pairs.first state))
    in  
      let inQuotes = (Pairs.second state)
      in (Logic.ifElse (Equality.equal c 34) (Logic.ifElse inQuotes ((acc, field), False) (Logic.ifElse (Strings.null field) ((acc, field), True) ((acc, (Strings.cat2 field "\"")), inQuotes))) (Logic.ifElse (Logic.and (Equality.equal c 44) (Logic.not inQuotes)) ((Lists.cons (normalizeField field) acc, ""), False) ((acc, (Strings.cat2 field (Strings.fromList [
        c]))), inQuotes)))

-- | Parse a CSV line into fields. Empty fields become Nothing.
parseCsvLine :: (String -> Either String [Maybe String])
parseCsvLine line =  
  let chars = (Strings.toList line)
  in  
    let initState = (([], ""), False)
    in  
      let finalState = (Lists.foldl parseCsvChar initState chars)
      in  
        let acc = (Pairs.first (Pairs.first finalState))
        in  
          let field = (Pairs.second (Pairs.first finalState))
          in  
            let inQuotes = (Pairs.second finalState)
            in (Logic.ifElse inQuotes (Left "Unclosed quoted field") (Right (Lists.reverse (Lists.cons (normalizeField field) acc))))

-- | Parse a single CSV line, returning Nothing for empty lines
parseSingleLine :: (String -> Either String (Maybe [Maybe String]))
parseSingleLine line =  
  let trimmed = (stripWhitespace line)
  in (Logic.ifElse (Strings.null trimmed) (Right Nothing) (Eithers.map (\x -> Just x) (parseCsvLine trimmed)))

-- | Parse raw CSV lines into a Table of strings
parseTableLines :: (Bool -> [String] -> Either String (Tabular.Table String))
parseTableLines hasHeader rawLines = (Eithers.bind (Eithers.mapList (\ln -> parseSingleLine ln) rawLines) (\parsedRows ->  
  let rows = (Maybes.cat parsedRows)
  in (Logic.ifElse hasHeader ( 
    let headerRow = (Lists.head rows)
    in  
      let dataRows = (Lists.tail rows)
      in (Logic.ifElse (listAny (\m -> Maybes.isNothing m) headerRow) (Left "null header column(s)") (Right (Tabular.Table {
        Tabular.tableHeader = (Just (Tabular.HeaderRow (Maybes.cat headerRow))),
        Tabular.tableData = (Lists.map (\r -> Tabular.DataRow r) dataRows)})))) (Right (Tabular.Table {
    Tabular.tableHeader = Nothing,
    Tabular.tableData = (Lists.map (\r -> Tabular.DataRow r) rows)})))))

-- | Strip leading and trailing whitespace from a string
stripWhitespace :: (String -> String)
stripWhitespace s =  
  let chars = (Strings.toList s)
  in  
    let isSpaceChar = (\c -> Chars.isSpace c)
    in  
      let trimLeft = (Lists.dropWhile isSpaceChar chars)
      in  
        let trimRight = (Lists.reverse (Lists.dropWhile isSpaceChar (Lists.reverse trimLeft)))
        in (Strings.fromList trimRight)

-- | Get the table name for an edge specification. Returns an error if not exactly one table is referenced.
tableForEdge :: (Model.Edge Core.Term -> Either String String)
tableForEdge edge =  
  let label = (Model.edgeLabel edge)
  in  
    let id = (Model.edgeId edge)
    in  
      let outId = (Model.edgeOut edge)
      in  
        let inId = (Model.edgeIn edge)
        in  
          let props = (Model.edgeProperties edge)
          in  
            let tables = (findTablesInTerms (Lists.concat2 [
                    id,
                    outId,
                    inId] (Maps.elems props)))
            in (Logic.ifElse (Equality.equal (Sets.size tables) 1) (Right (Lists.head (Sets.toList tables))) (Left (Strings.cat [
              "Specification for ",
              Model.unEdgeLabel label,
              " edges has wrong number of tables"])))

-- | Get the table name for a vertex specification. Returns an error if not exactly one table is referenced.
tableForVertex :: (Model.Vertex Core.Term -> Either String String)
tableForVertex vertex =  
  let label = (Model.vertexLabel vertex)
  in  
    let id = (Model.vertexId vertex)
    in  
      let props = (Model.vertexProperties vertex)
      in  
        let tables = (findTablesInTerms (Lists.cons id (Maps.elems props)))
        in (Logic.ifElse (Equality.equal (Sets.size tables) 1) (Right (Lists.head (Sets.toList tables))) (Left (Strings.cat [
          "Specification for ",
          Model.unVertexLabel label,
          " vertices has wrong number of tables"])))

-- | Build a map from table name to table type
tableTypesByName :: ([Tabular.TableType] -> M.Map Relational.RelationName Tabular.TableType)
tableTypesByName tableTypes = (Maps.fromList (Lists.map (\t -> (Tabular.tableTypeName t, t)) tableTypes))

-- | Convert a data row to a record term given a table type
termRowToRecord :: (Tabular.TableType -> Tabular.DataRow Core.Term -> Core.Term)
termRowToRecord tableType row =  
  let tname = (Relational.unRelationName (Tabular.tableTypeName tableType))
  in  
    let colTypes = (Tabular.tableTypeColumns tableType)
    in  
      let cells = (Tabular.unDataRow row)
      in (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name tname),
        Core.recordFields = (Lists.zipWith (\colType -> \mvalue ->  
          let cname = (Relational.unColumnName (Tabular.columnTypeName colType))
          in Core.Field {
            Core.fieldName = (Core.Name cname),
            Core.fieldTerm = (Core.TermMaybe mvalue)}) colTypes cells)}))

-- | Transform a record through vertex and edge specifications to produce vertices and edges
transformRecord :: ([Model.Vertex Core.Term] -> [Model.Edge Core.Term] -> Core.Term -> Compute.Flow Graph.Graph ([Model.Vertex Core.Term], [Model.Edge Core.Term]))
transformRecord vspecs especs record = (Flows.bind (Flows.mapList (\spec -> evaluateVertex spec record) vspecs) (\mVertices -> Flows.bind (Flows.mapList (\spec -> evaluateEdge spec record) especs) (\mEdges -> Flows.pure (Maybes.cat mVertices, (Maybes.cat mEdges)))))

-- | Transform all rows from a table through vertex/edge specifications
transformTableRows :: ([Model.Vertex Core.Term] -> [Model.Edge Core.Term] -> Tabular.TableType -> [Tabular.DataRow Core.Term] -> Compute.Flow Graph.Graph ([Model.Vertex Core.Term], [Model.Edge Core.Term]))
transformTableRows vspecs especs tableType rows = (Flows.map (\pairs -> Lists.foldl concatPairs ([], []) pairs) (Flows.mapList (\row -> transformRecord vspecs especs (termRowToRecord tableType row)) rows))
