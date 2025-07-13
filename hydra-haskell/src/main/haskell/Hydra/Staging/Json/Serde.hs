module Hydra.Staging.Json.Serde where

import Hydra.Tools.Monads
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Monads as Monads
import qualified Hydra.Ext.Org.Json.Coder as JsonCoder
import qualified Hydra.Tools.Bytestrings as Bytestrings
import qualified Hydra.Json as Json

import qualified Data.ByteString.Lazy as BS
import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as SC
import qualified Data.Char as C
import qualified Data.String as String


aesonValueToBytes :: A.Value -> BS.ByteString
aesonValueToBytes = A.encode

aesonValueToJsonValue :: A.Value -> Json.Value
aesonValueToJsonValue v = case v of
  A.Object km -> Json.ValueObject $ M.fromList (mapPair <$> AKM.toList km)
    where
      mapPair (k, v) = (AK.toString k, aesonValueToJsonValue v)
  A.Array a -> Json.ValueArray (aesonValueToJsonValue <$> V.toList a)
  A.String t -> Json.ValueString $ T.unpack t
  A.Number s -> Json.ValueNumber $ SC.toRealFloat s
  A.Bool b -> Json.ValueBoolean b
  A.Null -> Json.ValueNull

bytesToAesonValue :: BS.ByteString -> Either String A.Value
bytesToAesonValue = A.eitherDecode

bytesToJsonValue :: BS.ByteString -> Either String Json.Value
bytesToJsonValue bs = aesonValueToJsonValue <$> bytesToAesonValue bs

jsonByteStringCoder :: Core.Type -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph Graph.Graph Core.Term BS.ByteString)
jsonByteStringCoder typ = do
  coder <- JsonCoder.jsonCoder typ
  return Compute.Coder {
    Compute.coderEncode = fmap jsonValueToBytes . Compute.coderEncode coder,
    Compute.coderDecode = \bs -> case bytesToJsonValue bs of
        Left msg -> Monads.fail $ "JSON parsing failed: " ++ msg
        Right v -> Compute.coderDecode coder v}

-- | A convenience which maps typed terms to and from pretty-printed JSON strings, as opposed to JSON objects
jsonStringCoder :: Core.Type -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph Graph.Graph Core.Term String)
jsonStringCoder typ = do
  serde <- jsonByteStringCoder typ
  return Compute.Coder {
    Compute.coderEncode = fmap Bytestrings.bytesToString . Compute.coderEncode serde,
    Compute.coderDecode = Compute.coderDecode serde . Bytestrings.stringToBytes}

jsonValueToAesonValue :: Json.Value -> A.Value
jsonValueToAesonValue v = case v of
    Json.ValueArray l -> A.Array $ V.fromList (jsonValueToAesonValue <$> l)
    Json.ValueBoolean b -> A.Bool b
    Json.ValueNull -> A.Null
    Json.ValueNumber d -> A.Number $ SC.fromFloatDigits d
    Json.ValueObject m -> A.Object $ AKM.fromList (mapPair <$> M.toList m)
      where
        mapPair (k, v) = (AK.fromString k, jsonValueToAesonValue v)
    Json.ValueString s -> A.String $ T.pack s

jsonValueToBytes :: Json.Value -> BS.ByteString
jsonValueToBytes = aesonValueToBytes . jsonValueToAesonValue

jsonValueToString :: Json.Value -> String
jsonValueToString = Bytestrings.bytesToString . jsonValueToBytes

jsonValuesToString :: [Json.Value] -> String
jsonValuesToString = L.intercalate "\n" . fmap jsonValueToString

stringToJsonValue :: String -> Either String Json.Value
stringToJsonValue = bytesToJsonValue . Bytestrings.stringToBytes
