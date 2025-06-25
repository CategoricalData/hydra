module Hydra.Ext.Pg.Graphson.Utils where

import Hydra.Kernel
import Hydra.Ext.Pg.Graphson.Coder
import Hydra.Ext.Json.Coder
import Hydra.Dsl.Pg.Mappings
import qualified Hydra.Json as Json
import qualified Hydra.Pg.Graphson.Syntax as G
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Show.Core as ShowCore

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

pgElementToJson :: PGM.Schema Graph t v -> PG.Element v -> Flow Graph Json.Value
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

lazyGraphToElements :: LazyGraph v -> [PG.Element v]
lazyGraphToElements (LazyGraph vertices edges) = fmap PG.ElementVertex vertices ++ fmap PG.ElementEdge edges

pgElementsToGraphson :: (Ord v, Show v) => GraphsonContext s v -> [PG.Element v] -> Flow s [Json.Value]
pgElementsToGraphson ctx els = CM.mapM encode vertices
  where
    vertices = elementsToVerticesWithAdjacentEdges els
    encode = coderEncode (vertexToJsonCoder ctx)

pgElementsToJson :: PGM.Schema Graph t v -> [PG.Element v] -> Flow Graph Json.Value
pgElementsToJson schema els = Json.ValueArray <$> CM.mapM (pgElementToJson schema) els

termGraphsonContext :: GraphsonContext s Term
termGraphsonContext = GraphsonContext $ Coder encodeTerm decodeTerm
  where
    encodeTerm term = case fullyStripTerm term of
        TermLiteral lv -> case lv of
          LiteralBinary s -> pure $ G.ValueBinary s
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
        TermRecord r@(Record tname _) -> if tname == _Unit
          then pure G.ValueNull
          else unexp $ TermRecord r
        t -> unexp t
      where
        unexp t = unexpected "literal or unit value" $ ShowCore.term t
    decodeTerm _ = fail "decoding from GraphSON is not yet supported"
