module Hydra.Ext.Staging.Pg.Graphson.Utils where

import Hydra.Kernel
import Hydra.Ext.Staging.Pg.Graphson.Coder
import Hydra.Ext.Org.Json.Coder
import Hydra.Ext.Dsl.Pg.Mappings
import qualified Hydra.Json.Model as Json
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Pg.Graphson.Syntax as G
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Show.Core as ShowCore
import Hydra.Ext.Staging.Pg.Utils hiding (pgElementToJson, pgElementsToJson, lazyGraphToElements)
import qualified Hydra.Json.Writer as JsonWriter

import qualified Control.Monad as CM
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


elementsToVerticesWithAdjacentEdges :: Ord v => [PG.Element v] -> [PG.VertexWithAdjacentEdges v]
elementsToVerticesWithAdjacentEdges els = M.elems vertexMap1
  where
    vertices = E.lefts eithers
    edges = E.rights eithers
    eithers = fmap toEither els
      where
        toEither el = case el of
          PG.ElementVertex v -> Left v
          PG.ElementEdge e -> Right e
    vertexMap0 = M.fromList $ fmap toPair vertices
      where
        toPair v = (PG.vertexId v, PG.VertexWithAdjacentEdges v [] [])
    vertexMap1 = L.foldl addEdges vertexMap0 edges
      where
        addEdges vertexMap (PG.Edge label id outV inV props) = addEdge True $ addEdge False vertexMap
          where
            addEdge out vertexMap = case M.lookup focusV vertexMap of
                Nothing -> vertexMap -- Disconnected edges are ignored
                Just v -> M.insert focusV (appendE v) vertexMap
              where
                focusV = if out then outV else inV
                otherV = if out then inV else outV
                adjEdge = PG.AdjacentEdge label id otherV props
                appendE v = if out
                  then v {PG.vertexWithAdjacentEdgesOuts = (adjEdge:(PG.vertexWithAdjacentEdgesOuts v))}
                  else v {PG.vertexWithAdjacentEdgesIns = (adjEdge:(PG.vertexWithAdjacentEdgesIns v))}

exampleGraphsonContext :: GraphsonContext s String
exampleGraphsonContext = GraphsonContext $ Coder encodeValue decodeValue
  where
    encodeValue s = pure $ G.ValueString s
    decodeValue _ = fail "decoding from GraphSON is not yet supported"

pgElementToJson :: PGM.Schema s t v -> PG.Element v -> Flow s Json.Value
pgElementToJson schema el = case el of
    PG.ElementVertex vertex -> do
      let labelJson = Json.ValueString $ PG.unVertexLabel $ PG.vertexLabel vertex
      idJson <- coderDecode (PGM.schemaVertexIds schema) (PG.vertexId vertex) >>= untypedTermToJson
      propsJson <- propsToJson $ PG.vertexProperties vertex
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes [
        Just ("label", labelJson),
        Just ("id", idJson),
        propsJson]
    PG.ElementEdge edge -> do
      let labelJson = Json.ValueString $ PG.unEdgeLabel $ PG.edgeLabel edge
      idJson <- coderDecode (PGM.schemaEdgeIds schema) (PG.edgeId edge) >>= untypedTermToJson
      outJson <- coderDecode (PGM.schemaVertexIds schema) (PG.edgeOut edge) >>= untypedTermToJson
      inJson <- coderDecode (PGM.schemaVertexIds schema) (PG.edgeIn edge) >>= untypedTermToJson
      propsJson <- propsToJson $ PG.edgeProperties edge
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes [
        Just ("label", labelJson),
        Just ("id", idJson),
        Just ("out", outJson),
        Just ("in", inJson),
        propsJson]
  where
    propsToJson pairs = if L.null pairs
        then pure Nothing
        else do
          p <- CM.mapM propToJson $ M.toList pairs
          return $ Just $ ("properties", Json.ValueObject $ M.fromList p)
      where
        propToJson (PG.PropertyKey key, v) = do
          json <- coderDecode (PGM.schemaPropertyValues schema) v >>= untypedTermToJson
          return (key, json)

lazyGraphToElements :: PG.LazyGraph v -> [PG.Element v]
lazyGraphToElements (PG.LazyGraph vertices edges) = fmap PG.ElementVertex vertices ++ fmap PG.ElementEdge edges

pgElementsToGraphson :: (Ord v, Show v) => GraphsonContext s v -> [PG.Element v] -> Flow s [Json.Value]
pgElementsToGraphson ctx els = CM.mapM encode vertices
  where
    vertices = elementsToVerticesWithAdjacentEdges els
    encode = coderEncode (pgVertexWithAdjacentEdgesToJsonCoder ctx)

pgElementsToJson :: PGM.Schema s t v -> [PG.Element v] -> Flow s Json.Value
pgElementsToJson schema els = Json.ValueArray <$> CM.mapM (pgElementToJson schema) els

propertyGraphGraphsonLastMile :: (Ord v, Show t, Show v) => GraphsonContext Graph v -> PGM.Schema Graph t v -> t -> t -> LastMile Graph (PG.Element v)
propertyGraphGraphsonLastMile ctx schema vidType eidType =
  LastMile (\typ -> typeApplicationTermToPropertyGraph schema typ vidType eidType) (\els -> jsonValuesToString <$> pgElementsToGraphson ctx els) "jsonl"
  where
    jsonValuesToString = L.intercalate "\n" . fmap JsonWriter.printJson

propertyGraphJsonLastMile :: (Show t, Show v) => PGM.Schema Graph t v -> t -> t -> LastMile Graph (PG.Element v)
propertyGraphJsonLastMile schema vidType eidType =
  LastMile (\typ -> typeApplicationTermToPropertyGraph schema typ vidType eidType) (\els -> JsonWriter.printJson <$> pgElementsToJson schema els) "json"

stringGraphsonContext :: GraphsonContext s String
stringGraphsonContext = GraphsonContext $ Coder encodeString decodeString
  where
    encodeString s = pure $ G.ValueString s
    decodeString v = case v of
      G.ValueString s -> pure s
      _ -> fail $ "expected a string value, got: " ++ show v

termGraphsonContext :: GraphsonContext s Term
termGraphsonContext = GraphsonContext $ Coder encodeTerm decodeTerm
  where
    encodeTerm term = case deannotateTerm term of
        TermLiteral lv -> case lv of
          LiteralBinary s -> pure $ G.ValueBinary $ Literals.binaryToStringBS s
          LiteralBoolean b -> pure $ G.ValueBoolean b
          LiteralFloat fv -> case fv of
            FloatValueBigfloat f -> pure $ G.ValueBigDecimal $ G.BigDecimalValue $ show f
            FloatValueFloat32 f -> pure $ G.ValueFloat $ G.FloatValueFinite f
            FloatValueFloat64 f -> pure $ G.ValueDouble $ G.DoubleValueFinite f
          LiteralInteger iv -> case iv of
            IntegerValueBigint i -> pure $ G.ValueBigInteger i
            IntegerValueInt32 i -> pure $ G.ValueInteger i
            IntegerValueInt64 i -> pure $ G.ValueLong i
            _ -> fail $ "integer type is not yet supported: " ++ show (integerValueType iv)
          LiteralString s -> pure $ G.ValueString s
        TermRecord r@(Record tname _) -> unexp $ TermRecord r
        TermUnit -> pure G.ValueNull
        t -> unexp t
      where
        unexp t = unexpected "literal or unit value" $ ShowCore.term t
    decodeTerm _ = fail "decoding from GraphSON is not yet supported"
