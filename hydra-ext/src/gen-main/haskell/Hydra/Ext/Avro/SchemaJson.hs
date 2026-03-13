-- Note: this is an automatically generated file. Do not edit.

-- | JSON serialization and deserialization for Avro schemas

module Hydra.Ext.Avro.SchemaJson where

import qualified Hydra.Context as Context
import qualified Hydra.Error as Error
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

avro_aliases :: String
avro_aliases = "aliases"

avro_array :: String
avro_array = "array"

avro_ascending :: String
avro_ascending = "ascending"

avro_boolean :: String
avro_boolean = "boolean"

avro_bytes :: String
avro_bytes = "bytes"

avro_default :: String
avro_default = "default"

avro_descending :: String
avro_descending = "descending"

avro_doc :: String
avro_doc = "doc"

avro_double :: String
avro_double = "double"

avro_enum :: String
avro_enum = "enum"

avro_fields :: String
avro_fields = "fields"

avro_fixed :: String
avro_fixed = "fixed"

avro_float :: String
avro_float = "float"

avro_ignore :: String
avro_ignore = "ignore"

avro_int :: String
avro_int = "int"

avro_items :: String
avro_items = "items"

avro_long :: String
avro_long = "long"

avro_map :: String
avro_map = "map"

avro_name :: String
avro_name = "name"

avro_namespace :: String
avro_namespace = "namespace"

avro_null :: String
avro_null = "null"

avro_order :: String
avro_order = "order"

avro_record :: String
avro_record = "record"

avro_size :: String
avro_size = "size"

avro_string :: String
avro_string = "string"

avro_symbols :: String
avro_symbols = "symbols"

avro_type :: String
avro_type = "type"

avro_values :: String
avro_values = "values"

-- | Construct an error result with a message in context
err :: (Context.Context -> String -> Either (Context.InContext Error.Error) t0)
err cx msg = (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError msg)),
  Context.inContextContext = cx}))

-- | Construct an error for unexpected values
unexpectedE :: (Context.Context -> String -> String -> Either (Context.InContext Error.Error) t0)
unexpectedE cx expected found = (err cx (Strings.cat [
  "Expected ",
  expected,
  ", found: ",
  found]))

-- | Extract a JSON array or return an error
expectArrayE :: (t0 -> Model.Value -> Either t1 [Model.Value])
expectArrayE cx value = ((\x -> case x of
  Model.ValueArray v0 -> (Right v0)) value)

-- | Extract a JSON number or return an error
expectNumberE :: (t0 -> Model.Value -> Either t1 Double)
expectNumberE cx value = ((\x -> case x of
  Model.ValueNumber v0 -> (Right v0)) value)

-- | Extract a JSON object or return an error
expectObjectE :: (t0 -> Model.Value -> Either t1 (M.Map String Model.Value))
expectObjectE cx value = ((\x -> case x of
  Model.ValueObject v0 -> (Right v0)) value)

-- | Extract a JSON string or return an error
expectStringE :: (t0 -> Model.Value -> Either t1 String)
expectStringE cx value = ((\x -> case x of
  Model.ValueString v0 -> (Right v0)) value)

-- | Look up a required attribute in a JSON object map
requireE :: (Context.Context -> String -> M.Map String t0 -> Either (Context.InContext Error.Error) t0)
requireE cx fname m = (Maybes.maybe (err cx (Strings.cat [
  "required attribute ",
  (Literals.showString fname),
  " not found"])) (\v -> Right v) (Maps.lookup fname m))

-- | Look up a required array attribute in a JSON object map
requireArrayE :: (Context.Context -> String -> M.Map String Model.Value -> Either (Context.InContext Error.Error) [Model.Value])
requireArrayE cx fname m = (Eithers.bind (requireE cx fname m) (\v -> expectArrayE cx v))

-- | Look up a required number attribute in a JSON object map
requireNumberE :: (Context.Context -> String -> M.Map String Model.Value -> Either (Context.InContext Error.Error) Double)
requireNumberE cx fname m = (Eithers.bind (requireE cx fname m) (\v -> expectNumberE cx v))

-- | Look up a required string attribute in a JSON object map
requireStringE :: (Context.Context -> String -> M.Map String Model.Value -> Either (Context.InContext Error.Error) String)
requireStringE cx fname m = (Eithers.bind (requireE cx fname m) (\v -> expectStringE cx v))

-- | Look up an optional attribute in a JSON object map
optE :: Ord t0 => (t0 -> M.Map t0 t1 -> Maybe t1)
optE k m = (Maps.lookup k m)

-- | Look up an optional array attribute in a JSON object map
optArrayE :: Ord t1 => (t0 -> t1 -> M.Map t1 Model.Value -> Either t2 (Maybe [Model.Value]))
optArrayE cx fname m = (Maybes.maybe (Right Nothing) (\v -> Eithers.map (\a -> Maybes.pure a) (expectArrayE cx v)) (Maps.lookup fname m))

-- | Look up an optional string attribute in a JSON object map
optStringE :: Ord t1 => (t0 -> t1 -> M.Map t1 Model.Value -> Either t2 (Maybe String))
optStringE cx fname m = (Maybes.maybe (Right Nothing) (\v -> Eithers.map (\s -> Maybes.pure s) (expectStringE cx v)) (Maps.lookup fname m))

-- | Convert a JSON value to its string representation
showJsonValue :: (t0 -> t1)
showJsonValue v = (showJsonValue v)

-- | Parse a JSON string, returning Either for compatibility
stringToJsonValue :: (t0 -> t1)
stringToJsonValue s = (stringToJsonValue s)

-- | Create a coder between Avro schemas and JSON values
avroSchemaJsonCoder :: (t0 -> t1)
avroSchemaJsonCoder cx = (avroSchemaJsonCoder cx)

-- | Create a coder between Avro schemas and JSON strings
avroSchemaStringCoder :: (t0 -> t1)
avroSchemaStringCoder cx = (avroSchemaStringCoder cx)

-- | Decode aliases from a JSON object map
decodeAliases :: (t0 -> t1 -> t2)
decodeAliases cx m = (decodeAliases cx m)

-- | Decode an Avro enum type from a JSON object map
decodeEnum :: (t0 -> t1 -> t2)
decodeEnum cx m = (decodeEnum cx m)

-- | Decode an Avro field from a JSON object map
decodeField :: (t0 -> t1 -> t2)
decodeField cx m = (decodeField cx m)

-- | Decode an Avro fixed type from a JSON object map
decodeFixed :: (t0 -> t1 -> t2)
decodeFixed cx m = (decodeFixed cx m)

-- | Decode a named Avro schema from a JSON value
decodeNamedSchema :: (t0 -> t1 -> t2)
decodeNamedSchema cx value = (decodeNamedSchema cx value)

-- | Decode an Avro field ordering from a string
decodeOrder :: (t0 -> t1 -> t2)
decodeOrder cx o = (decodeOrder cx o)

-- | Decode an Avro record type from a JSON object map
decodeRecord :: (t0 -> t1 -> t2)
decodeRecord cx m = (decodeRecord cx m)

-- | Decode an Avro schema from a JSON value
decodeSchema :: (t0 -> t1 -> t2)
decodeSchema cx v = (decodeSchema cx v)

-- | Extract annotation entries (keys starting with @) from a JSON object map
getAnnotations :: (M.Map String t0 -> M.Map String t0)
getAnnotations m = (Maps.fromList (Maybes.cat (Lists.map (\entry ->  
  let k = (Pairs.first entry) 
      v = (Pairs.second entry)
  in (Logic.ifElse (Equality.equal (Strings.charAt 0 k) 64) (Maybes.pure (Strings.fromList (Lists.drop 1 (Strings.toList k)), v)) Nothing)) (Maps.toList m))))
