module Hydra.Ext.Staging.Avro.SchemaJson where

import Hydra.Kernel
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Json.Parser as JsonParser
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as Json

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found

-- | Either-based JSON extraction helpers

expectArrayE :: Context -> Json.Value -> Result [Json.Value]
expectArrayE cx value = case value of
  Json.ValueArray v -> Right v
  _ -> unexpectedE cx "JSON array" (showJsonValue value)

expectNumberE :: Context -> Json.Value -> Result Double
expectNumberE cx value = case value of
  Json.ValueNumber v -> Right v
  _ -> unexpectedE cx "JSON number" (showJsonValue value)

expectObjectE :: Context -> Json.Value -> Result (M.Map String Json.Value)
expectObjectE cx value = case value of
  Json.ValueObject v -> Right v
  _ -> unexpectedE cx "JSON object" (showJsonValue value)

expectStringE :: Context -> Json.Value -> Result String
expectStringE cx value = case value of
  Json.ValueString v -> Right v
  _ -> unexpectedE cx "JSON string" (showJsonValue value)

requireE :: (Ord k, Show k) => Context -> k -> M.Map k v -> Result v
requireE cx fname m = case M.lookup fname m of
  Nothing -> err cx $ "required attribute " ++ show fname ++ " not found"
  Just v -> Right v

requireArrayE :: Context -> String -> M.Map String Json.Value -> Result [Json.Value]
requireArrayE cx fname m = requireE cx fname m >>= expectArrayE cx

requireNumberE :: Context -> String -> M.Map String Json.Value -> Result Double
requireNumberE cx fname m = requireE cx fname m >>= expectNumberE cx

requireStringE :: Context -> String -> M.Map String Json.Value -> Result String
requireStringE cx fname m = requireE cx fname m >>= expectStringE cx

optE :: Ord k => k -> M.Map k v -> Maybe v
optE = M.lookup

optArrayE :: Context -> String -> M.Map String Json.Value -> Result (Maybe [Json.Value])
optArrayE cx fname m = case M.lookup fname m of
  Nothing -> Right Nothing
  Just v -> Just <$> expectArrayE cx v

optStringE :: Context -> String -> M.Map String Json.Value -> Result (Maybe String)
optStringE cx fname m = case M.lookup fname m of
  Nothing -> Right Nothing
  Just v -> Just <$> expectStringE cx v

showJsonValue :: Json.Value -> String
showJsonValue = show

-- | Parse a JSON string, returning Either for compatibility
stringToJsonValue :: String -> Either String Json.Value
stringToJsonValue s = case JsonParser.parseJson s of
  ParseResultSuccess success -> Right (parseSuccessValue success)
  ParseResultFailure err -> Left (parseErrorMessage err)


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
avro_null = "null"
avro_order = "order"
avro_record = "record"
avro_size = "size"
avro_string = "string"
avro_symbols = "symbols"
avro_type = "type"
avro_values = "values"

avroSchemaJsonCoder :: Context -> Coder Avro.Schema Json.Value
avroSchemaJsonCoder cx = Coder encode decode
  where
    encode _cx _schema = err cx "not implemented"
    decode cx' v = decodeNamedSchema cx' v

avroSchemaStringCoder :: Context -> Coder Avro.Schema String
avroSchemaStringCoder cx = Coder encode decode
  where
    jsonCoder = avroSchemaJsonCoder cx
    encode cx' schema = do
      json <- coderEncode jsonCoder cx' schema
      Right $ JsonWriter.printJson json
    decode cx' s = do
      json <- case stringToJsonValue s of
        Left msg -> err cx' $ "failed to parse JSON: " ++ msg
        Right j -> Right j
      coderDecode jsonCoder cx' json

decodeAliases :: Context -> M.Map String Json.Value -> Result (Maybe [String])
decodeAliases cx m = do
  aliasesJson <- optArrayE cx avro_aliases m
  case aliasesJson of
    Nothing -> Right Nothing
    Just a -> Just <$> CM.mapM (expectStringE cx) a

decodeEnum :: Context -> M.Map String Json.Value -> Result Avro.NamedType
decodeEnum cx m = do
  symbolsJson <- requireArrayE cx avro_symbols m
  symbols <- CM.mapM (expectStringE cx) symbolsJson
  dflt <- optStringE cx avro_default m
  return $ Avro.NamedTypeEnum $ Avro.Enum symbols dflt

decodeField :: Context -> M.Map String Json.Value -> Result Avro.Field
decodeField cx m = do
  fname <- requireStringE cx avro_name m
  doc <- optStringE cx avro_doc m
  typ <- requireE cx avro_type m >>= decodeSchema cx
  let dflt = optE avro_default m
  order <- case optE avro_order m of
    Nothing -> Right Nothing
    Just o -> Just <$> (expectStringE cx o >>= decodeOrder cx)
  aliases <- decodeAliases cx m
  let anns = getAnnotations m
  return $ Avro.Field fname doc typ dflt order aliases anns

decodeFixed :: Context -> M.Map String Json.Value -> Result Avro.NamedType
decodeFixed cx m = do
    size <- doubleToInt <$> requireNumberE cx avro_size m
    return $ Avro.NamedTypeFixed $ Avro.Fixed size
  where
    doubleToInt d = if d < 0 then ceiling d else floor d

decodeNamedSchema :: Context -> Json.Value -> Result Avro.Schema
decodeNamedSchema cx value = do
  m <- expectObjectE cx value
  name <- requireStringE cx avro_name m
  ns <- optStringE cx avro_namespace m
  typ <- requireStringE cx avro_type m
  nt <- case M.lookup typ decoders of
    Nothing -> unexpectedE cx "Avro type" $ show typ
    Just d -> d m
  aliases <- decodeAliases cx m
  doc <- optStringE cx avro_doc m
  let anns = getAnnotations m
  return $ Avro.SchemaNamed $ Avro.Named name ns aliases doc nt anns
  where
    decoders = M.fromList [
      (avro_enum, decodeEnum cx),
      (avro_fixed, decodeFixed cx),
      (avro_record, decodeRecord cx)]

decodeOrder :: Context -> String -> Result Avro.Order
decodeOrder cx o = case M.lookup o orderMap of
    Nothing -> unexpectedE cx "ordering" $ show o
    Just order -> Right order
  where
    orderMap = M.fromList [
      (avro_ascending, Avro.OrderAscending),
      (avro_descending, Avro.OrderDescending),
      (avro_ignore, Avro.OrderIgnore)]

decodeRecord :: Context -> M.Map String Json.Value -> Result Avro.NamedType
decodeRecord cx m = do
  fields <- requireArrayE cx avro_fields m >>= CM.mapM (expectObjectE cx) >>= CM.mapM (decodeField cx)
  return $ Avro.NamedTypeRecord $ Avro.Record fields

decodeSchema :: Context -> Json.Value -> Result Avro.Schema
decodeSchema cx v = case v of
  Json.ValueArray els -> Avro.SchemaUnion <$> (Avro.Union <$> (CM.mapM (decodeSchema cx) els))
  Json.ValueObject m -> do
      typ <- requireStringE cx avro_type m
      case M.lookup typ decoders of
        Nothing -> unexpectedE cx "\"array\" or \"map\"" $ show typ
        Just d -> d m
    where
      decoders = M.fromList [
        (avro_array, \m -> do
          items <- requireE cx avro_items m >>= decodeSchema cx
          return $ Avro.SchemaArray $ Avro.Array items),
        (avro_enum, \m -> decodeNamedSchema cx $ Json.ValueObject m),
        (avro_fixed, \m -> decodeNamedSchema cx $ Json.ValueObject m),
        (avro_map, \m -> do
          values <- requireE cx avro_values m >>= decodeSchema cx
          return $ Avro.SchemaMap $ Avro.Map values),
        (avro_record, \m -> decodeNamedSchema cx $ Json.ValueObject m)]
  Json.ValueString s -> Right $ case M.lookup s schemas of
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
        (avro_null, Avro.PrimitiveNull),
        (avro_string, Avro.PrimitiveString)]
  Json.ValueNull -> Right $ Avro.SchemaPrimitive $ Avro.PrimitiveNull
  _ -> unexpectedE cx "JSON array, object, or string" $ show v

getAnnotations :: M.Map String Json.Value -> M.Map String Json.Value
getAnnotations = M.fromList . Y.catMaybes . fmap toPair . M.toList
  where
    toPair (k, v) = if L.take 1 k == "@"
      then Just (L.drop 1 k, v)
      else Nothing
