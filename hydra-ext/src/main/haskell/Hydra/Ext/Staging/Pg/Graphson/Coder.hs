module Hydra.Ext.Staging.Pg.Graphson.Coder (
  GraphsonContext(..),
  graphsonVertexToJsonCoder,
  pgVertexWithAdjacentEdgesToGraphsonVertexCoder,
  pgVertexWithAdjacentEdgesToJsonCoder)
where

import Hydra.Kernel
import Hydra.Pg.Model as PG
import Hydra.Pg.Graphson.Syntax as G
import Hydra.Json as Json

import qualified Data.Char as C
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y

-- End to end ------------------------------------------------------------------

data GraphsonContext s v = GraphsonContext {
  graphsonContextValueCoder :: Coder s s v G.Value}

pgVertexWithAdjacentEdgesToJsonCoder :: GraphsonContext s v -> Coder s s (PG.VertexWithAdjacentEdges v) Json.Value
pgVertexWithAdjacentEdgesToJsonCoder ctx = composeCoders (pgVertexWithAdjacentEdgesToGraphsonVertexCoder ctx) graphsonVertexToJsonCoder

-- PG to GraphSON --------------------------------------------------------------

adjacentEdgeToGraphson :: (v -> Flow s G.Value) -> PG.AdjacentEdge v -> Flow s (G.EdgeLabel, G.AdjacentEdge)
adjacentEdgeToGraphson encodeValue (PG.AdjacentEdge (PG.EdgeLabel label) id v props) = do
    gid <- encodeValue id
    gv <- encodeValue v
    propPairs <- CM.mapM (edgePropertyToGraphson encodeValue) $ M.toList props
    return (G.EdgeLabel label, G.AdjacentEdge gid gv (M.fromList propPairs))

aggregateMap :: Ord k => [(k, v)] -> M.Map k [v]
aggregateMap = L.foldl addPair M.empty
  where
    addPair m (k, v) = M.insert k (case M.lookup k m of
      Nothing -> [v]
      Just l -> v:l) m

edgePropertyToGraphson :: (v -> Flow s G.Value) -> (PG.PropertyKey, v) -> Flow s (G.PropertyKey, G.Value)
edgePropertyToGraphson encodeValue (PG.PropertyKey k, v) = do
  gv <- encodeValue v
  return (G.PropertyKey k, gv)

vertexPropertyToGraphson :: (v -> Flow s G.Value) -> (PG.PropertyKey, v) -> Flow s (G.PropertyKey, G.VertexPropertyValue)
vertexPropertyToGraphson encodeValue (PG.PropertyKey k, v) = do
  gv <- encodeValue v
  return (G.PropertyKey k, G.VertexPropertyValue Nothing gv)

pgVertexWithAdjacentEdgesToGraphsonVertex :: GraphsonContext s v -> PG.VertexWithAdjacentEdges v -> Flow s G.Vertex
pgVertexWithAdjacentEdgesToGraphsonVertex ctx (PG.VertexWithAdjacentEdges (PG.Vertex (PG.VertexLabel label) id props) ins outs) = do
    gid <- encodeValue id
    propPairs <- CM.mapM (vertexPropertyToGraphson encodeValue) $ M.toList props
    inPairs <- CM.mapM (adjacentEdgeToGraphson encodeValue) ins
    outPairs <- CM.mapM (adjacentEdgeToGraphson encodeValue) outs
    return $ G.Vertex gid (Just $ G.VertexLabel label) (aggregateMap inPairs) (aggregateMap outPairs) (aggregateMap propPairs)
  where
    encodeValue = coderEncode $ graphsonContextValueCoder ctx

pgVertexWithAdjacentEdgesToGraphsonVertexCoder :: GraphsonContext s v -> Coder s s (PG.VertexWithAdjacentEdges v) G.Vertex
pgVertexWithAdjacentEdgesToGraphsonVertexCoder ctx = Coder (pgVertexWithAdjacentEdgesToGraphsonVertex ctx) decode
  where
    decode _ = fail "decoding GraphSON is currently unsupported"

-- GraphSON to JSON ------------------------------------------------------------

adjacentEdgeToJson :: Bool -> G.AdjacentEdge -> Json.Value
adjacentEdgeToJson out (G.AdjacentEdge id otherV props) = toJsonObject [
  ("id", Just $ valueToJson id),
  ("inV", if out then Just $ valueToJson otherV else Nothing),
  ("outV", if out then Nothing else Just $ valueToJson otherV),
  ("properties", edgePropertyMapToJson props)]

doubleValueToJson :: G.DoubleValue -> Json.Value
doubleValueToJson v = case v of
  G.DoubleValueFinite d -> Json.ValueNumber d
  G.DoubleValueInfinity -> Json.ValueString "Infinity"
  G.DoubleValueNegativeInfinity -> Json.ValueString "-Infinity"
  G.DoubleValueNotANumber -> Json.ValueString "NaN"

edgePropertyMapToJson :: M.Map G.PropertyKey G.Value -> Y.Maybe Json.Value
edgePropertyMapToJson m = if M.null m
    then Nothing
    else Just $ Json.ValueObject $ M.fromList $ fmap mapPair $ M.toList m
  where
    mapPair(G.PropertyKey k, v) = (k, valueToJson v)

floatValueToJson :: G.FloatValue -> Json.Value
floatValueToJson v = case v of
  G.FloatValueFinite f -> Json.ValueNumber $ realToFrac f
  G.FloatValueInfinity -> Json.ValueString "Infinity"
  G.FloatValueNegativeInfinity -> Json.ValueString "-Infinity"
  G.FloatValueNotANumber -> Json.ValueString "NaN"

-- | Map a GraphSON vertex to a JSON object. The reverse is currently unsupported.
graphsonVertexToJsonCoder :: Coder s s G.Vertex Json.Value
graphsonVertexToJsonCoder = Coder encode decode
  where
    encode = pure . vertexToJson
    decode _ = fail "decoding GraphSON JSON is currently unsupported"

optionalList :: (a -> Json.Value) -> [a] -> Y.Maybe Json.Value
optionalList mapping els = if L.null els
  then Nothing
  else Just $ Json.ValueArray $ fmap mapping els

mapToJson :: G.Map -> Json.Value
mapToJson (G.Map pairs) = Json.ValueArray $ L.concat $ fmap fromPair pairs
  where
    fromPair (G.ValuePair k v) = [valueToJson k, valueToJson v]

edgeMapToJson :: Bool -> M.Map G.EdgeLabel [G.AdjacentEdge] -> Y.Maybe Json.Value
edgeMapToJson out m = if M.null m
    then Nothing
    else Just $ Json.ValueObject $ M.fromList $ fmap mapPair $ M.toList m
  where
    mapPair (G.EdgeLabel l, vs) = (l, Json.ValueArray $ fmap (adjacentEdgeToJson out) vs)

toJsonObject :: [(String, Y.Maybe Json.Value)] -> Json.Value
toJsonObject pairs = Json.ValueObject $ M.fromList $ Y.catMaybes $ fmap mapPair pairs
  where
    mapPair (k, mv) = fmap (\v -> (k, v)) mv

typedValueToJson :: String -> Json.Value -> Json.Value
typedValueToJson typeName valueJson = toJsonObject [
  ("@type", Just $ Json.ValueString typeName),
  ("@value", Just $ valueJson)]

valueToJson :: G.Value -> Json.Value
valueToJson v = case v of
  G.ValueBigDecimal (G.BigDecimalValue s) -> typedValueToJson "g:BigDecimal" $ Json.ValueString s
  G.ValueBigInteger i -> typedValueToJson "g:BigInteger" $ Json.ValueNumber $ fromIntegral i -- Note: lossy
  G.ValueBinary s -> typedValueToJson "g:Binary" $ Json.ValueString s -- Note: in this context, the string is assumed to be base-64 encoded already
  G.ValueBoolean b -> Json.ValueBoolean b
  G.ValueByte b -> typedValueToJson "g:Byte" $ Json.ValueNumber $ fromIntegral b
  G.ValueChar c -> typedValueToJson "g:Char" $ Json.ValueString [C.chr $ fromIntegral c]
  G.ValueComposite (G.CompositeTypedValue (G.TypeName tname) fields) -> typedValueToJson tname $ mapToJson fields
  G.ValueDateTime (G.DateTime s) -> typedValueToJson "g:DateTime" $ Json.ValueString s
  G.ValueDouble dv -> typedValueToJson "g:Double" $ doubleValueToJson dv
  G.ValueDuration (G.Duration s) -> typedValueToJson "g:Duration" $ Json.ValueString s
  G.ValueFloat fv -> typedValueToJson "g:Float" $ floatValueToJson fv
  G.ValueInteger i -> typedValueToJson "g:Int32" $ Json.ValueNumber $ fromIntegral i
  G.ValueList vals -> typedValueToJson "g:List" $ Json.ValueArray (fmap valueToJson vals)
  G.ValueLong l -> typedValueToJson "g:Long" $ Json.ValueNumber $ fromIntegral l
  G.ValueMap m -> typedValueToJson "g:Map" $ mapToJson m
  G.ValueNull -> Json.ValueNull
  G.ValuePrimitive (G.PrimitiveTypedValue tname valString) -> typedValueToJson "g:PrimitivePdt" $ Json.ValueString valString
  G.ValueSet vals -> typedValueToJson "g:Set" $ Json.ValueArray (fmap valueToJson vals)
  G.ValueShort i -> typedValueToJson "g:Int16" $ Json.ValueNumber $ fromIntegral i
  G.ValueString s -> Json.ValueString s
  G.ValueUuid (G.Uuid s) -> typedValueToJson "g:UUID" $ Json.ValueString s

vertexPropertyValue :: G.VertexPropertyValue -> Json.Value
vertexPropertyValue (G.VertexPropertyValue mid v) = toJsonObject [
  ("id", fmap valueToJson mid),
  ("value", Just $ valueToJson v)]

vertexToJson :: G.Vertex -> Json.Value
vertexToJson (G.Vertex id label inE outE props) = toJsonObject [
  ("id", Just $ valueToJson id),
  ("label", fmap (Json.ValueString . G.unVertexLabel) label),
  ("inE", edgeMapToJson False inE),
  ("outE", edgeMapToJson True outE),
  ("properties", vertexPropertyMapToJson props)]

vertexPropertyMapToJson :: M.Map G.PropertyKey [G.VertexPropertyValue] -> Y.Maybe Json.Value
vertexPropertyMapToJson m = if M.null m
    then Nothing
    else Just $ Json.ValueObject $ M.fromList $ fmap mapPair $ M.toList m
  where
    mapPair (G.PropertyKey k, vs) = (k, Json.ValueArray $ fmap vertexPropertyValue vs)
