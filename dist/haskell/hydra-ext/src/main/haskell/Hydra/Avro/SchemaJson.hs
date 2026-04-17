-- Note: this is an automatically generated file. Do not edit.

-- | JSON serialization and deserialization for Avro schemas

module Hydra.Avro.SchemaJson where

import qualified Hydra.Avro.Schema as Schema
import qualified Hydra.Coders as Coders
import qualified Hydra.Errors as Errors
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Parser as Parser
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | Create a coder between Avro schemas and JSON values
avroSchemaJsonCoder :: t0 -> Coders.Coder Schema.Schema Model.Value
avroSchemaJsonCoder cx =
    Coders.Coder {
      Coders.coderEncode = (\_cx -> \schema -> Right (encodeSchema schema)),
      Coders.coderDecode = (\cx2 -> \json -> decodeSchema cx2 json)}

-- | Create a coder between Avro schemas and JSON strings
avroSchemaStringCoder :: t0 -> Coders.Coder Schema.Schema String
avroSchemaStringCoder cx =
    Coders.Coder {
      Coders.coderEncode = (\_cx -> \schema -> Right (showJsonValue (encodeSchema schema))),
      Coders.coderDecode = (\cx2 -> \s -> Eithers.bind (Eithers.either (\e -> err cx2 e) (\v -> Right v) (stringToJsonValue s)) (\json -> decodeSchema cx2 json))}

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

-- | Decode aliases from a JSON object map
decodeAliases :: t0 -> M.Map String Model.Value -> Either t1 (Maybe [String])
decodeAliases cx m =
    Eithers.bind (optArrayE cx avro_aliases m) (\mArr -> Maybes.maybe (Right Nothing) (\arr -> Eithers.map (\strs -> Maybes.pure strs) (Eithers.mapList (expectStringE cx) arr)) mArr)

-- | Decode an Avro array schema from a JSON object map
decodeArraySchema :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.Schema
decodeArraySchema cx m =
    Eithers.bind (requireE cx avro_items m) (\items -> Eithers.map (\s -> Schema.SchemaArray (Schema.Array {
      Schema.arrayItems = s})) (decodeSchema cx items))

-- | Decode an Avro enum type from a JSON object map
decodeEnum :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.NamedType
decodeEnum cx m =
    Eithers.bind (requireArrayE cx avro_symbols m) (\syms -> Eithers.bind (Eithers.mapList (expectStringE cx) syms) (\symbols -> Eithers.bind (optStringE cx avro_default m) (\defVal -> Right (Schema.NamedTypeEnum (Schema.Enum {
      Schema.enumSymbols = symbols,
      Schema.enumDefault = defVal})))))

-- | Decode an Avro field from a JSON object map
decodeField :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.Field
decodeField cx m =
    Eithers.bind (requireStringE cx avro_name m) (\name -> Eithers.bind (optStringE cx avro_doc m) (\fdoc -> Eithers.bind (requireE cx avro_type m) (\typeJson -> Eithers.bind (decodeSchema cx typeJson) (\fieldType -> Eithers.bind (Eithers.bind (optStringE cx avro_order m) (\mOrd -> Eithers.mapMaybe (decodeOrder cx) mOrd)) (\order -> Eithers.bind (decodeAliases cx m) (\aliases -> Right (Schema.Field {
      Schema.fieldName = name,
      Schema.fieldDoc = fdoc,
      Schema.fieldType = fieldType,
      Schema.fieldDefault = (optE avro_default m),
      Schema.fieldOrder = order,
      Schema.fieldAliases = aliases,
      Schema.fieldAnnotations = (getAnnotations m)})))))))

-- | Decode an Avro fixed type from a JSON object map
decodeFixed :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.NamedType
decodeFixed cx m =
    Eithers.bind (requireNumberE cx avro_size m) (\n ->
      let size = Literals.bigintToInt32 (Literals.decimalToBigint n)
      in (Right (Schema.NamedTypeFixed (Schema.Fixed {
        Schema.fixedSize = size}))))

-- | Decode an Avro map schema from a JSON object map
decodeMapSchema :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.Schema
decodeMapSchema cx m =
    Eithers.bind (requireE cx avro_values m) (\values -> Eithers.map (\s -> Schema.SchemaMap (Schema.Map {
      Schema.mapValues = s})) (decodeSchema cx values))

-- | Decode a named Avro schema from a JSON object map and a decoded named type result
decodeNamedSchema :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.NamedType -> Either Errors.Error Schema.Schema
decodeNamedSchema cx m namedTypeResult =
    Eithers.bind (requireStringE cx avro_name m) (\name -> Eithers.bind (optStringE cx avro_namespace m) (\ns -> Eithers.bind (optStringE cx avro_doc m) (\sdoc -> Eithers.bind (decodeAliases cx m) (\aliases -> Eithers.bind namedTypeResult (\namedType -> Right (Schema.SchemaNamed (Schema.Named {
      Schema.namedName = name,
      Schema.namedNamespace = ns,
      Schema.namedAliases = aliases,
      Schema.namedDoc = sdoc,
      Schema.namedType = namedType,
      Schema.namedAnnotations = (getAnnotations m)})))))))

-- | Decode an Avro schema from a JSON object given the type name
decodeObjectSchema :: t0 -> M.Map String Model.Value -> String -> Either Errors.Error Schema.Schema
decodeObjectSchema cx m typeName =
    Logic.ifElse (Equality.equal typeName "array") (decodeArraySchema cx m) (Logic.ifElse (Equality.equal typeName "map") (decodeMapSchema cx m) (Logic.ifElse (Equality.equal typeName "record") (decodeNamedSchema cx m (decodeRecord cx m)) (Logic.ifElse (Equality.equal typeName "enum") (decodeNamedSchema cx m (decodeEnum cx m)) (Logic.ifElse (Equality.equal typeName "fixed") (decodeNamedSchema cx m (decodeFixed cx m)) (Maybes.maybe (err cx (Strings.cat [
      "unknown type: ",
      typeName])) (\p -> Right (Schema.SchemaPrimitive p)) (decodePrimitiveName typeName))))))

-- | Decode an Avro field ordering from a string
decodeOrder :: t0 -> String -> Either Errors.Error Schema.Order
decodeOrder cx o =
    Logic.ifElse (Equality.equal o "ascending") (Right Schema.OrderAscending) (Logic.ifElse (Equality.equal o "descending") (Right Schema.OrderDescending) (Logic.ifElse (Equality.equal o "ignore") (Right Schema.OrderIgnore) (err cx (Strings.cat [
      "unknown order: ",
      o]))))

-- | Decode a primitive type name string to a Primitive, or Nothing if not a primitive
decodePrimitiveName :: String -> Maybe Schema.Primitive
decodePrimitiveName s =
    Logic.ifElse (Equality.equal s "null") (Just Schema.PrimitiveNull) (Logic.ifElse (Equality.equal s "boolean") (Just Schema.PrimitiveBoolean) (Logic.ifElse (Equality.equal s "int") (Just Schema.PrimitiveInt) (Logic.ifElse (Equality.equal s "long") (Just Schema.PrimitiveLong) (Logic.ifElse (Equality.equal s "float") (Just Schema.PrimitiveFloat) (Logic.ifElse (Equality.equal s "double") (Just Schema.PrimitiveDouble) (Logic.ifElse (Equality.equal s "bytes") (Just Schema.PrimitiveBytes) (Logic.ifElse (Equality.equal s "string") (Just Schema.PrimitiveString) Nothing)))))))

-- | Decode an Avro record type from a JSON object map
decodeRecord :: t0 -> M.Map String Model.Value -> Either Errors.Error Schema.NamedType
decodeRecord cx m =
    Eithers.bind (requireArrayE cx avro_fields m) (\fieldJsons -> Eithers.bind (Eithers.mapList (\fj -> Eithers.bind (expectObjectE cx fj) (\fm -> decodeField cx fm)) fieldJsons) (\fields -> Right (Schema.NamedTypeRecord (Schema.Record {
      Schema.recordFields = fields}))))

-- | Decode an Avro schema from a JSON value
decodeSchema :: t0 -> Model.Value -> Either Errors.Error Schema.Schema
decodeSchema cx v =
    case v of
      Model.ValueString v0 -> Maybes.maybe (Right (Schema.SchemaReference v0)) (\p -> Right (Schema.SchemaPrimitive p)) (decodePrimitiveName v0)
      Model.ValueArray v0 -> Eithers.map (\decoded -> Schema.SchemaUnion (Schema.Union decoded)) (Eithers.mapList (decodeSchema cx) v0)
      Model.ValueObject v0 -> Eithers.bind (requireStringE cx avro_type v0) (\typeName -> decodeObjectSchema cx v0 typeName)
      _ -> err cx (Strings.cat [
        "unexpected JSON value for schema: ",
        (showJsonValue v)])

-- | Encode annotations as key-value pairs with @ prefix on keys
encodeAnnotations :: M.Map String t0 -> [(String, t0)]
encodeAnnotations m = Lists.map (\entry -> (Strings.cat2 "@" (Pairs.first entry), (Pairs.second entry))) (Maps.toList m)

-- | Encode an Avro array schema to a JSON object
encodeArray :: Schema.Array -> Model.Value
encodeArray arr =
    Model.ValueObject (Maps.fromList [
      ("type", (Model.ValueString "array")),
      ("items", (encodeSchema (Schema.arrayItems arr)))])

-- | Encode an Avro enum type as key-value pairs
encodeEnum :: Schema.Enum -> [(String, Model.Value)]
encodeEnum e =
    Lists.concat [
      [
        ("type", (Model.ValueString "enum"))],
      [
        ("symbols", (Model.ValueArray (Lists.map (\s -> Model.ValueString s) (Schema.enumSymbols e))))],
      (Maybes.maybe [] (\d -> [
        ("default", (Model.ValueString d))]) (Schema.enumDefault e))]

-- | Encode an Avro field to a JSON object
encodeField :: Schema.Field -> Model.Value
encodeField f =
    Model.ValueObject (Maps.fromList (Lists.concat [
      [
        ("name", (Model.ValueString (Schema.fieldName f)))],
      [
        ("type", (encodeSchema (Schema.fieldType f)))],
      (Maybes.maybe [] (\d -> [
        ("doc", (Model.ValueString d))]) (Schema.fieldDoc f)),
      (Maybes.maybe [] (\d -> [
        ("default", d)]) (Schema.fieldDefault f)),
      (Maybes.maybe [] (\o -> [
        encodeOrder o]) (Schema.fieldOrder f)),
      (Maybes.maybe [] (\als -> [
        ("aliases", (Model.ValueArray (Lists.map (\a -> Model.ValueString a) als)))]) (Schema.fieldAliases f)),
      (encodeAnnotations (Schema.fieldAnnotations f))]))

-- | Encode an Avro fixed type as key-value pairs
encodeFixed :: Schema.Fixed -> [(String, Model.Value)]
encodeFixed f =
    [
      ("type", (Model.ValueString "fixed")),
      ("size", (Model.ValueNumber (Literals.bigintToDecimal (Literals.int32ToBigint (Schema.fixedSize f)))))]

-- | Encode an Avro map schema to a JSON object
encodeMap :: Schema.Map -> Model.Value
encodeMap mp =
    Model.ValueObject (Maps.fromList [
      ("type", (Model.ValueString "map")),
      ("values", (encodeSchema (Schema.mapValues mp)))])

-- | Encode an Avro named type to a JSON object
encodeNamed :: Schema.Named -> Model.Value
encodeNamed n =
    Model.ValueObject (Maps.fromList (Lists.concat [
      [
        ("name", (Model.ValueString (Schema.namedName n)))],
      (Maybes.maybe [] (\ns -> [
        ("namespace", (Model.ValueString ns))]) (Schema.namedNamespace n)),
      (Maybes.maybe [] (\d -> [
        ("doc", (Model.ValueString d))]) (Schema.namedDoc n)),
      (Maybes.maybe [] (\als -> [
        ("aliases", (Model.ValueArray (Lists.map (\a -> Model.ValueString a) als)))]) (Schema.namedAliases n)),
      (encodeNamedType (Schema.namedType n)),
      (encodeAnnotations (Schema.namedAnnotations n))]))

-- | Encode the specific variant of a named Avro type
encodeNamedType :: Schema.NamedType -> [(String, Model.Value)]
encodeNamedType nt =
    case nt of
      Schema.NamedTypeEnum v0 -> encodeEnum v0
      Schema.NamedTypeFixed v0 -> encodeFixed v0
      Schema.NamedTypeRecord v0 -> encodeRecord v0

-- | Encode an Avro field ordering as a key-value pair
encodeOrder :: Schema.Order -> (String, Model.Value)
encodeOrder o =
    ("order", (Model.ValueString (case o of
      Schema.OrderAscending -> "ascending"
      Schema.OrderDescending -> "descending"
      Schema.OrderIgnore -> "ignore")))

-- | Encode an Avro primitive type as a JSON string
encodePrimitive :: Schema.Primitive -> Model.Value
encodePrimitive p =
    Model.ValueString (case p of
      Schema.PrimitiveNull -> "null"
      Schema.PrimitiveBoolean -> "boolean"
      Schema.PrimitiveInt -> "int"
      Schema.PrimitiveLong -> "long"
      Schema.PrimitiveFloat -> "float"
      Schema.PrimitiveDouble -> "double"
      Schema.PrimitiveBytes -> "bytes"
      Schema.PrimitiveString -> "string")

-- | Encode an Avro record type as key-value pairs
encodeRecord :: Schema.Record -> [(String, Model.Value)]
encodeRecord r =
    [
      ("type", (Model.ValueString "record")),
      ("fields", (Model.ValueArray (Lists.map encodeField (Schema.recordFields r))))]

-- | Encode an Avro schema to a JSON value
encodeSchema :: Schema.Schema -> Model.Value
encodeSchema schema =
    case schema of
      Schema.SchemaPrimitive v0 -> encodePrimitive v0
      Schema.SchemaArray v0 -> encodeArray v0
      Schema.SchemaMap v0 -> encodeMap v0
      Schema.SchemaNamed v0 -> encodeNamed v0
      Schema.SchemaReference v0 -> Model.ValueString v0
      Schema.SchemaUnion v0 -> encodeUnion v0

-- | Encode an Avro union as a JSON array of schemas
encodeUnion :: Schema.Union -> Model.Value
encodeUnion u = Model.ValueArray (Lists.map encodeSchema (Schema.unUnion u))

-- | Construct an error result with a message in context
err :: t0 -> String -> Either Errors.Error t1
err cx msg = Left (Errors.ErrorOther (Errors.OtherError msg))

-- | Extract a JSON array or return an error
expectArrayE :: t0 -> Model.Value -> Either t1 [Model.Value]
expectArrayE cx value =
    case value of
      Model.ValueArray v0 -> Right v0

-- | Extract a JSON number or return an error
expectNumberE :: t0 -> Model.Value -> Either t1 Sci.Scientific
expectNumberE cx value =
    case value of
      Model.ValueNumber v0 -> Right v0

-- | Extract a JSON object or return an error
expectObjectE :: t0 -> Model.Value -> Either t1 (M.Map String Model.Value)
expectObjectE cx value =
    case value of
      Model.ValueObject v0 -> Right v0

-- | Extract a JSON string or return an error
expectStringE :: t0 -> Model.Value -> Either t1 String
expectStringE cx value =
    case value of
      Model.ValueString v0 -> Right v0

-- | Extract annotation entries (keys starting with @) from a JSON object map
getAnnotations :: M.Map String t0 -> M.Map String t0
getAnnotations m =
    Maps.fromList (Maybes.cat (Lists.map (\entry ->
      let k = Pairs.first entry
          v = Pairs.second entry
      in (Logic.ifElse (Equality.equal (Maybes.fromMaybe 0 (Strings.maybeCharAt 0 k)) 64) (Maybes.pure (Strings.fromList (Lists.drop 1 (Strings.toList k)), v)) Nothing)) (Maps.toList m)))

-- | Look up an optional array attribute in a JSON object map
optArrayE :: Ord t1 => (t0 -> t1 -> M.Map t1 Model.Value -> Either t2 (Maybe [Model.Value]))
optArrayE cx fname m =
    Maybes.maybe (Right Nothing) (\v -> Eithers.map (\a -> Maybes.pure a) (expectArrayE cx v)) (Maps.lookup fname m)

-- | Look up an optional attribute in a JSON object map
optE :: Ord t0 => (t0 -> M.Map t0 t1 -> Maybe t1)
optE k m = Maps.lookup k m

-- | Look up an optional string attribute in a JSON object map
optStringE :: Ord t1 => (t0 -> t1 -> M.Map t1 Model.Value -> Either t2 (Maybe String))
optStringE cx fname m =
    Maybes.maybe (Right Nothing) (\v -> Eithers.map (\s -> Maybes.pure s) (expectStringE cx v)) (Maps.lookup fname m)

-- | Look up a required array attribute in a JSON object map
requireArrayE :: t0 -> String -> M.Map String Model.Value -> Either Errors.Error [Model.Value]
requireArrayE cx fname m = Eithers.bind (requireE cx fname m) (\v -> expectArrayE cx v)

-- | Look up a required attribute in a JSON object map
requireE :: t0 -> String -> M.Map String t1 -> Either Errors.Error t1
requireE cx fname m =
    Maybes.maybe (err cx (Strings.cat [
      "required attribute ",
      (Literals.showString fname),
      " not found"])) (\v -> Right v) (Maps.lookup fname m)

-- | Look up a required number attribute in a JSON object map
requireNumberE :: t0 -> String -> M.Map String Model.Value -> Either Errors.Error Sci.Scientific
requireNumberE cx fname m = Eithers.bind (requireE cx fname m) (\v -> expectNumberE cx v)

-- | Look up a required string attribute in a JSON object map
requireStringE :: t0 -> String -> M.Map String Model.Value -> Either Errors.Error String
requireStringE cx fname m = Eithers.bind (requireE cx fname m) (\v -> expectStringE cx v)

-- | Convert a JSON value to its string representation
showJsonValue :: Model.Value -> String
showJsonValue v = Writer.printJson v

-- | Parse a JSON string, returning Either for compatibility
stringToJsonValue :: String -> Either String Model.Value
stringToJsonValue s =
    (\x -> case x of
      Parsing.ParseResultSuccess v0 -> Right (Parsing.parseSuccessValue v0)
      Parsing.ParseResultFailure v0 -> Left (Parsing.parseErrorMessage v0)) (Parser.parseJson s)

-- | Construct an error for unexpected values
unexpectedE :: t0 -> String -> String -> Either Errors.Error t1
unexpectedE cx expected found =
    err cx (Strings.cat [
      "Expected ",
      expected,
      ", found: ",
      found])
