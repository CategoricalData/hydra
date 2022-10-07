module Hydra.Ext.Avro.SchemaJson where

import Hydra.Monads
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Ext.Json.Serde
import qualified Hydra.Ext.Avro.Schema as Avro
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Ext.Json.Eliminate

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Maybe as Y


avro_aliases = "aliases"
avro_array = "array"
avro_ascending = "ascending"
avro_boolean = "boolean"
avro_bytes = "bytes"
avro_default = "default"
avro_descending = "descending"
avro_doc = "doc"
avro_double = "double"
avro_enum = "enum"
avro_fields = "fields"
avro_fixed = "fixed"
avro_float = "float"
avro_ignore = "ignore"
avro_int = "int"
avro_items = "items"
avro_long = "long"
avro_map = "map"
avro_name = "name"
avro_namespace = "namespace"
avro_order = "order"
avro_record = "record"
avro_size = "size"
avro_string = "string"
avro_symbols = "symbols"
avro_type = "type"
avro_values = "values"

avroSchemaJsonCoder :: Coder s s Avro.Schema Json.Value
avroSchemaJsonCoder = Coder {
  coderEncode = \schema -> fail "not implemented",
  coderDecode = decodeNamedSchema}

avroSchemaStringCoder :: Coder s s Avro.Schema String
avroSchemaStringCoder = Coder {
  coderEncode = \schema -> valueToString <$> coderEncode avroSchemaJsonCoder schema,
  coderDecode = \s -> do
    json <- case stringToValue s of
      Left msg -> fail $ "failed to parse JSON: " ++ msg
      Right j -> pure j
    coderDecode avroSchemaJsonCoder json}

decodeAliases :: M.Map String Json.Value -> Flow s (Maybe [String])
decodeAliases m = do
  aliasesJson <- optArray avro_aliases m
  case aliasesJson of
    Nothing -> pure Nothing
    Just a -> Just <$> CM.mapM expectString a

decodeEnum :: M.Map String Json.Value -> Flow s Avro.NamedType
decodeEnum m = do
  symbolsJson <- requireArray avro_symbols m
  symbols <- CM.mapM expectString symbolsJson
  dflt <- optString avro_default m
  return $ Avro.NamedTypeEnum $ Avro.Enum_ symbols dflt

decodeField :: M.Map String Json.Value -> Flow s Avro.Field
decodeField m = do
  fname <- requireString avro_name m
  doc <- optString avro_doc m
  typ <- require avro_type m >>= decodeSchema
  let dflt = opt avro_default m
  order <- case opt avro_order m of
    Nothing -> pure Nothing
    Just o -> Just <$> (expectString o >>= decodeOrder)
  aliases <- decodeAliases m
  return $ Avro.Field fname doc typ dflt order aliases

decodeFixed :: M.Map String Json.Value -> Flow s Avro.NamedType
decodeFixed m = do
    size <- doubleToInt <$> requireNumber avro_size m
    return $ Avro.NamedTypeFixed $ Avro.Fixed size
  where
    doubleToInt d = if d < 0 then ceiling d else floor d

decodeNamedSchema :: Json.Value -> Flow s Avro.Schema
decodeNamedSchema value = do
  m <- expectObject value
  name <- requireString avro_name m
  ns <- optString avro_namespace m
  typ <- requireString avro_type m
  nt <- case M.lookup typ decoders of
    Nothing -> unexpected "Avro type" typ
    Just d -> d m
  aliases <- decodeAliases m
  doc <- optString avro_doc m
  return $ Avro.SchemaNamed $ Avro.Named name ns aliases doc nt
  where
    decoders = M.fromList [
      (avro_enum, decodeEnum),
      (avro_fixed, decodeFixed),
      (avro_record, decodeRecord)]

decodeOrder :: String -> Flow s Avro.Order
decodeOrder o = case M.lookup o orderMap of
    Nothing -> unexpected "ordering" o
    Just order -> pure order
  where
    orderMap = M.fromList [
      (avro_ascending, Avro.OrderAscending),
      (avro_descending, Avro.OrderDescending),
      (avro_ignore, Avro.OrderIgnore)]

decodeRecord :: M.Map String Json.Value -> Flow s Avro.NamedType
decodeRecord m = do
  fields <- requireArray avro_fields m >>= CM.mapM expectObject >>= CM.mapM decodeField
  return $ Avro.NamedTypeRecord $ Avro.Record fields

decodeSchema :: Json.Value -> Flow s Avro.Schema
decodeSchema v = case v of
  Json.ValueArray els -> Avro.SchemaUnion <$> (Avro.Union <$> (CM.mapM decodeSchema els))
  Json.ValueObject m -> do
      typ <- requireString avro_type m
      case M.lookup typ decoders of
        Nothing -> unexpected "\"array\" or \"map\"" typ
        Just d -> d m
    where
      decoders = M.fromList [
        (avro_array, \m -> do
          items <- require avro_items m >>= decodeSchema
          return $ Avro.SchemaArray $ Avro.Array items),
        (avro_enum, \m -> decodeNamedSchema $ Json.ValueObject m),
        (avro_fixed, \m -> decodeNamedSchema $ Json.ValueObject m),
        (avro_map, \m -> do
          values <- require avro_values m >>= decodeSchema
          return $ Avro.SchemaMap $ Avro.Map_ values),
        (avro_record, \m -> decodeNamedSchema $ Json.ValueObject m)]
  Json.ValueString s -> pure $ case M.lookup s schemas of
      Just prim -> Avro.SchemaPrimitive prim
      Nothing -> Avro.SchemaReference s
    where
      schemas = M.fromList [
        (avro_boolean, Avro.PrimitiveBoolean),
        (avro_bytes, Avro.PrimitiveBytes),
        (avro_double, Avro.PrimitiveDouble),
        (avro_float, Avro.PrimitiveFloat),
        (avro_int, Avro.PrimitiveInt),
        (avro_long, Avro.PrimitiveLong),
        (avro_string, Avro.PrimitiveString)]
  Json.ValueNull -> pure $ Avro.SchemaPrimitive $ Avro.PrimitiveNull
  _ -> unexpected "JSON array, object, or string" v