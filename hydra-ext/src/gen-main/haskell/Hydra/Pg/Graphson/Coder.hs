-- Note: this is an automatically generated file. Do not edit.

-- | Encoding functions for converting GraphSON syntax to JSON.

module Hydra.Pg.Graphson.Coder where

import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a GraphSON AdjacentEdge to a JSON Value. The Bool indicates whether this is an outgoing edge.
adjacentEdgeToJson :: (Bool -> Syntax.AdjacentEdge -> Model.Value)
adjacentEdgeToJson out ae = (toJsonObject [
  ("id", (Just (valueToJson (Syntax.adjacentEdgeId ae)))),
  ("inV", (Logic.ifElse out (Just (valueToJson (Syntax.adjacentEdgeVertexId ae))) Nothing)),
  ("outV", (Logic.ifElse out Nothing (Just (valueToJson (Syntax.adjacentEdgeVertexId ae))))),
  ("properties", (edgePropertyMapToJson (Syntax.adjacentEdgeProperties ae)))])

-- | Convert a GraphSON DoubleValue to a JSON Value
doubleValueToJson :: (Syntax.DoubleValue -> Model.Value)
doubleValueToJson x = case x of
  Syntax.DoubleValueFinite v1 -> (Model.ValueNumber v1)
  Syntax.DoubleValueInfinity -> (Model.ValueString "Infinity")
  Syntax.DoubleValueNegativeInfinity -> (Model.ValueString "-Infinity")
  Syntax.DoubleValueNotANumber -> (Model.ValueString "NaN")

-- | Convert a map of edges by label to an optional JSON Value
edgeMapToJson :: (Bool -> M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge] -> Maybe Model.Value)
edgeMapToJson out m = (Logic.ifElse (Maps.null m) Nothing (Just (Model.ValueObject (Maps.fromList (Lists.map (\p -> (Syntax.unEdgeLabel (Pairs.first p), (Model.ValueArray (Lists.map (adjacentEdgeToJson out) (Pairs.second p))))) (Maps.toList m))))))

-- | Convert a map of edge properties to an optional JSON Value
edgePropertyMapToJson :: (M.Map Syntax.PropertyKey Syntax.Value -> Maybe Model.Value)
edgePropertyMapToJson m = (Logic.ifElse (Maps.null m) Nothing (Just (Model.ValueObject (Maps.fromList (Lists.map (\p -> (Syntax.unPropertyKey (Pairs.first p), (valueToJson (Pairs.second p)))) (Maps.toList m))))))

-- | Convert a GraphSON FloatValue to a JSON Value
floatValueToJson :: (Syntax.FloatValue -> Model.Value)
floatValueToJson x = case x of
  Syntax.FloatValueFinite v1 -> (Model.ValueNumber (Literals.float32ToBigfloat v1))
  Syntax.FloatValueInfinity -> (Model.ValueString "Infinity")
  Syntax.FloatValueNegativeInfinity -> (Model.ValueString "-Infinity")
  Syntax.FloatValueNotANumber -> (Model.ValueString "NaN")

-- | Convert a GraphSON Map to a JSON array of alternating keys and values
mapToJson :: (Syntax.Map -> Model.Value)
mapToJson m = (Model.ValueArray (Lists.concat (Lists.map (\vp -> [
  valueToJson (Syntax.valuePairFirst vp),
  (valueToJson (Syntax.valuePairSecond vp))]) (Syntax.unMap m))))

-- | Create a JSON object from a list of key-value pairs, filtering out Nothing values
toJsonObject :: ([(String, (Maybe Model.Value))] -> Model.Value)
toJsonObject pairs = (Model.ValueObject (Maps.fromList (Maybes.cat (Lists.map (\p -> Maybes.map (\v -> (Pairs.first p, v)) (Pairs.second p)) pairs))))

-- | Create a typed JSON object with @type and @value fields
typedValueToJson :: (String -> Model.Value -> Model.Value)
typedValueToJson typeName valueJson = (toJsonObject [
  ("@type", (Just (Model.ValueString typeName))),
  ("@value", (Just valueJson))])

-- | Convert a GraphSON Value to a JSON Value
valueToJson :: (Syntax.Value -> Model.Value)
valueToJson x = case x of
  Syntax.ValueBigDecimal v1 -> (typedValueToJson "g:BigDecimal" (Model.ValueString (Syntax.unBigDecimalValue v1)))
  Syntax.ValueBigInteger v1 -> (typedValueToJson "g:BigInteger" (Model.ValueNumber (Literals.bigintToBigfloat v1)))
  Syntax.ValueBinary v1 -> (typedValueToJson "g:Binary" (Model.ValueString v1))
  Syntax.ValueBoolean v1 -> (Model.ValueBoolean v1)
  Syntax.ValueByte v1 -> (typedValueToJson "g:Byte" (Model.ValueNumber (Literals.bigintToBigfloat (Literals.uint8ToBigint v1))))
  Syntax.ValueChar v1 -> (typedValueToJson "g:Char" (Model.ValueString (Strings.fromList (Lists.pure (Literals.bigintToInt32 (Literals.uint32ToBigint v1))))))
  Syntax.ValueComposite v1 -> (typedValueToJson (Syntax.unTypeName (Syntax.compositeTypedValueType v1)) (mapToJson (Syntax.compositeTypedValueFields v1)))
  Syntax.ValueDateTime v1 -> (typedValueToJson "g:DateTime" (Model.ValueString (Syntax.unDateTime v1)))
  Syntax.ValueDouble v1 -> (typedValueToJson "g:Double" (doubleValueToJson v1))
  Syntax.ValueDuration v1 -> (typedValueToJson "g:Duration" (Model.ValueString (Syntax.unDuration v1)))
  Syntax.ValueFloat v1 -> (typedValueToJson "g:Float" (floatValueToJson v1))
  Syntax.ValueInteger v1 -> (typedValueToJson "g:Int32" (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint v1))))
  Syntax.ValueList v1 -> (typedValueToJson "g:List" (Model.ValueArray (Lists.map valueToJson v1)))
  Syntax.ValueLong v1 -> (typedValueToJson "g:Long" (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int64ToBigint v1))))
  Syntax.ValueMap v1 -> (typedValueToJson "g:Map" (mapToJson v1))
  Syntax.ValueNull -> Model.ValueNull
  Syntax.ValuePrimitive v1 -> (typedValueToJson "g:PrimitivePdt" (Model.ValueString (Syntax.primitiveTypedValueValue v1)))
  Syntax.ValueSet v1 -> (typedValueToJson "g:Set" (Model.ValueArray (Lists.map valueToJson v1)))
  Syntax.ValueShort v1 -> (typedValueToJson "g:Int16" (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int16ToBigint v1))))
  Syntax.ValueString v1 -> (Model.ValueString v1)
  Syntax.ValueUuid v1 -> (typedValueToJson "g:UUID" (Model.ValueString (Syntax.unUuid v1)))

-- | Convert a map of vertex properties to an optional JSON Value
vertexPropertyMapToJson :: (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue] -> Maybe Model.Value)
vertexPropertyMapToJson m = (Logic.ifElse (Maps.null m) Nothing (Just (Model.ValueObject (Maps.fromList (Lists.map (\p -> (Syntax.unPropertyKey (Pairs.first p), (Model.ValueArray (Lists.map vertexPropertyValueToJson (Pairs.second p))))) (Maps.toList m))))))

-- | Convert a GraphSON VertexPropertyValue to a JSON Value
vertexPropertyValueToJson :: (Syntax.VertexPropertyValue -> Model.Value)
vertexPropertyValueToJson vpv = (toJsonObject [
  ("id", (Maybes.map valueToJson (Syntax.vertexPropertyValueId vpv))),
  ("value", (Just (valueToJson (Syntax.vertexPropertyValueValue vpv))))])

-- | Convert a GraphSON Vertex to a JSON Value
vertexToJson :: (Syntax.Vertex -> Model.Value)
vertexToJson v = (toJsonObject [
  ("id", (Just (valueToJson (Syntax.vertexId v)))),
  ("label", (Maybes.map (\lbl -> Model.ValueString (Syntax.unVertexLabel lbl)) (Syntax.vertexLabel v))),
  ("inE", (edgeMapToJson False (Syntax.vertexInEdges v))),
  ("outE", (edgeMapToJson True (Syntax.vertexOutEdges v))),
  ("properties", (vertexPropertyMapToJson (Syntax.vertexProperties v)))])
