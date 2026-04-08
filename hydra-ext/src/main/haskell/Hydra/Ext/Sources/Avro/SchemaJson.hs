module Hydra.Ext.Sources.Avro.SchemaJson where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as JM
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Ext.Sources.Avro.Schema as AvroSchema
-- Result type alias (was previously imported from Staging module)
type Result a = Either Error a


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

avroSchemaPhantomNs :: Namespace
avroSchemaPhantomNs = Namespace "hydra.ext.org.apache.avro.schema"

jsonModelNs :: Namespace
jsonModelNs = Namespace "hydra.json.model"

jsonParserNs :: Namespace
jsonParserNs = Namespace "hydra.json.parser"

jsonWriterNs :: Namespace
jsonWriterNs = Namespace "hydra.json.writer"

ns :: Namespace
ns = Namespace "hydra.ext.avro.schemaJson"

module_ :: Module
module_ = Module ns definitions
    [jsonWriterNs, jsonParserNs]
    (AvroSchema.ns:jsonModelNs:Namespace "hydra.parsing":KernelTypes.kernelTypesNamespaces) $
    Just "JSON serialization and deserialization for Avro schemas"
  where
    definitions = [
      -- String constants
      toDefinition avro_aliases,
      toDefinition avro_array,
      toDefinition avro_ascending,
      toDefinition avro_boolean,
      toDefinition avro_bytes,
      toDefinition avro_default,
      toDefinition avro_descending,
      toDefinition avro_doc,
      toDefinition avro_double,
      toDefinition avro_enum,
      toDefinition avro_fields,
      toDefinition avro_fixed,
      toDefinition avro_float,
      toDefinition avro_ignore,
      toDefinition avro_int,
      toDefinition avro_items,
      toDefinition avro_long,
      toDefinition avro_map,
      toDefinition avro_name,
      toDefinition avro_namespace,
      toDefinition avro_null,
      toDefinition avro_order,
      toDefinition avro_record,
      toDefinition avro_size,
      toDefinition avro_string,
      toDefinition avro_symbols,
      toDefinition avro_type,
      toDefinition avro_values,
      -- Error helpers
      toDefinition err,
      toDefinition unexpectedE,
      -- JSON extraction helpers
      toDefinition expectArrayE,
      toDefinition expectNumberE,
      toDefinition expectObjectE,
      toDefinition expectStringE,
      toDefinition requireE,
      toDefinition requireArrayE,
      toDefinition requireNumberE,
      toDefinition requireStringE,
      toDefinition optE,
      toDefinition optArrayE,
      toDefinition optStringE,
      toDefinition showJsonValue,
      toDefinition stringToJsonValue,
      -- Encode functions
      toDefinition encodeSchema,
      toDefinition encodePrimitive,
      toDefinition encodeArray,
      toDefinition encodeMap,
      toDefinition encodeNamed,
      toDefinition encodeNamedType,
      toDefinition encodeEnumE,
      toDefinition encodeFixedE,
      toDefinition encodeRecordE,
      toDefinition encodeFieldE,
      toDefinition encodeOrderE,
      toDefinition encodeUnion,
      toDefinition encodeAnnotations,
      -- Coder functions
      toDefinition avroSchemaJsonCoder,
      toDefinition avroSchemaStringCoder,
      -- Decode functions
      toDefinition decodeAliases,
      toDefinition decodeArraySchema,
      toDefinition decodeEnum,
      toDefinition decodeField,
      toDefinition decodeFixed,
      toDefinition decodeMapSchema,
      toDefinition decodeNamedSchema,
      toDefinition decodeObjectSchema,
      toDefinition decodeOrder,
      toDefinition decodePrimitiveName,
      toDefinition decodeRecord,
      toDefinition decodeSchema,
      toDefinition getAnnotations]


-- | Error helpers

err :: TTermDefinition (Context -> String -> Result a)
err = define "err" $
  doc "Construct an error result with a message in context" $
  lambda "cx" $ lambda "msg" $
    Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx")

unexpectedE :: TTermDefinition (Context -> String -> String -> Result a)
unexpectedE = define "unexpectedE" $
  doc "Construct an error for unexpected values" $
  lambda "cx" $ lambda "expected" $ lambda "found" $
    err @@ var "cx" @@ (Strings.cat $ list [string "Expected ", var "expected", string ", found: ", var "found"])


-- | JSON extraction helpers

expectArrayE :: TTermDefinition (Context -> JM.Value -> Result [JM.Value])
expectArrayE = define "expectArrayE" $
  doc "Extract a JSON array or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_array>>: lambda "v" $ Phantoms.right (var "v")]

expectNumberE :: TTermDefinition (Context -> JM.Value -> Result Double)
expectNumberE = define "expectNumberE" $
  doc "Extract a JSON number or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_number>>: lambda "v" $ Phantoms.right (var "v")]

expectObjectE :: TTermDefinition (Context -> JM.Value -> Result (M.Map String JM.Value))
expectObjectE = define "expectObjectE" $
  doc "Extract a JSON object or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_object>>: lambda "v" $ Phantoms.right (var "v")]

expectStringE :: TTermDefinition (Context -> JM.Value -> Result String)
expectStringE = define "expectStringE" $
  doc "Extract a JSON string or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_string>>: lambda "v" $ Phantoms.right (var "v")]

requireE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result JM.Value)
requireE = define "requireE" $
  doc "Look up a required attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (err @@ var "cx" @@ (Strings.cat $ list [string "required attribute ", Literals.showString (var "fname"), string " not found"]))
      (lambda "v" $ Phantoms.right (var "v"))
      (Maps.lookup (var "fname") (var "m"))

requireArrayE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result [JM.Value])
requireArrayE = define "requireArrayE" $
  doc "Look up a required array attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectArrayE @@ var "cx" @@ var "v")

requireNumberE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result Double)
requireNumberE = define "requireNumberE" $
  doc "Look up a required number attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectNumberE @@ var "cx" @@ var "v")

requireStringE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result String)
requireStringE = define "requireStringE" $
  doc "Look up a required string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectStringE @@ var "cx" @@ var "v")

optE :: TTermDefinition (String -> M.Map String JM.Value -> Maybe JM.Value)
optE = define "optE" $
  doc "Look up an optional attribute in a JSON object map" $
  lambda "k" $ lambda "m" $
    Maps.lookup (var "k") (var "m")

optArrayE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result (Maybe [JM.Value]))
optArrayE = define "optArrayE" $
  doc "Look up an optional array attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (Phantoms.right nothing)
      (lambda "v" $ Eithers.map (lambda "a" $ Maybes.pure (var "a")) (expectArrayE @@ var "cx" @@ var "v"))
      (Maps.lookup (var "fname") (var "m"))

optStringE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result (Maybe String))
optStringE = define "optStringE" $
  doc "Look up an optional string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (Phantoms.right nothing)
      (lambda "v" $ Eithers.map (lambda "s" $ Maybes.pure (var "s")) (expectStringE @@ var "cx" @@ var "v"))
      (Maps.lookup (var "fname") (var "m"))

showJsonValue :: TTermDefinition (JM.Value -> String)
showJsonValue = define "showJsonValue" $
  doc "Convert a JSON value to its string representation" $
  lambda "v" $
    var "hydra.json.writer.printJson" @@ var "v"

stringToJsonValue :: TTermDefinition (String -> Either String JM.Value)
stringToJsonValue = define "stringToJsonValue" $
  doc "Parse a JSON string, returning Either for compatibility" $
  lambda "s" $
    cases Parsing._ParseResult (var "hydra.json.parser.parseJson" @@ var "s") Nothing [
      Parsing._ParseResult_success>>: lambda "success" $
        Phantoms.right (project Parsing._ParseSuccess Parsing._ParseSuccess_value @@ var "success"),
      Parsing._ParseResult_failure>>: lambda "failure" $
        Phantoms.left (project Parsing._ParseError Parsing._ParseError_message @@ var "failure")]


-- | String constants

avro_aliases :: TTermDefinition String
avro_aliases = define "avro_aliases" $ string "aliases"

avro_array :: TTermDefinition String
avro_array = define "avro_array" $ string "array"

avro_ascending :: TTermDefinition String
avro_ascending = define "avro_ascending" $ string "ascending"

avro_boolean :: TTermDefinition String
avro_boolean = define "avro_boolean" $ string "boolean"

avro_bytes :: TTermDefinition String
avro_bytes = define "avro_bytes" $ string "bytes"

avro_default :: TTermDefinition String
avro_default = define "avro_default" $ string "default"

avro_descending :: TTermDefinition String
avro_descending = define "avro_descending" $ string "descending"

avro_doc :: TTermDefinition String
avro_doc = define "avro_doc" $ string "doc"

avro_double :: TTermDefinition String
avro_double = define "avro_double" $ string "double"

avro_enum :: TTermDefinition String
avro_enum = define "avro_enum" $ string "enum"

avro_fields :: TTermDefinition String
avro_fields = define "avro_fields" $ string "fields"

avro_fixed :: TTermDefinition String
avro_fixed = define "avro_fixed" $ string "fixed"

avro_float :: TTermDefinition String
avro_float = define "avro_float" $ string "float"

avro_ignore :: TTermDefinition String
avro_ignore = define "avro_ignore" $ string "ignore"

avro_int :: TTermDefinition String
avro_int = define "avro_int" $ string "int"

avro_items :: TTermDefinition String
avro_items = define "avro_items" $ string "items"

avro_long :: TTermDefinition String
avro_long = define "avro_long" $ string "long"

avro_map :: TTermDefinition String
avro_map = define "avro_map" $ string "map"

avro_name :: TTermDefinition String
avro_name = define "avro_name" $ string "name"

avro_namespace :: TTermDefinition String
avro_namespace = define "avro_namespace" $ string "namespace"

avro_null :: TTermDefinition String
avro_null = define "avro_null" $ string "null"

avro_order :: TTermDefinition String
avro_order = define "avro_order" $ string "order"

avro_record :: TTermDefinition String
avro_record = define "avro_record" $ string "record"

avro_size :: TTermDefinition String
avro_size = define "avro_size" $ string "size"

avro_string :: TTermDefinition String
avro_string = define "avro_string" $ string "string"

avro_symbols :: TTermDefinition String
avro_symbols = define "avro_symbols" $ string "symbols"

avro_type :: TTermDefinition String
avro_type = define "avro_type" $ string "type"

avro_values :: TTermDefinition String
avro_values = define "avro_values" $ string "values"


-- | Encode functions

encodeSchema :: TTermDefinition (Avro.Schema -> JM.Value)
encodeSchema = define "encodeSchema" $
  doc "Encode an Avro schema to a JSON value" $
  lambda "schema" $
    cases Avro._Schema (var "schema") Nothing [
      Avro._Schema_primitive>>: lambda "p" $ encodePrimitive @@ var "p",
      Avro._Schema_array>>: lambda "arr" $ encodeArray @@ var "arr",
      Avro._Schema_map>>: lambda "mp" $ encodeMap @@ var "mp",
      Avro._Schema_named>>: lambda "n" $ encodeNamed @@ var "n",
      Avro._Schema_reference>>: lambda "ref" $ inject JM._Value JM._Value_string (var "ref"),
      Avro._Schema_union>>: lambda "u" $ encodeUnion @@ var "u"]

encodePrimitive :: TTermDefinition (Avro.Primitive -> JM.Value)
encodePrimitive = define "encodePrimitive" $
  doc "Encode an Avro primitive type as a JSON string" $
  lambda "p" $
    inject JM._Value JM._Value_string
      (cases Avro._Primitive (var "p") Nothing [
        Avro._Primitive_null>>: constant (string "null"),
        Avro._Primitive_boolean>>: constant (string "boolean"),
        Avro._Primitive_int>>: constant (string "int"),
        Avro._Primitive_long>>: constant (string "long"),
        Avro._Primitive_float>>: constant (string "float"),
        Avro._Primitive_double>>: constant (string "double"),
        Avro._Primitive_bytes>>: constant (string "bytes"),
        Avro._Primitive_string>>: constant (string "string")])

encodeArray :: TTermDefinition (Avro.Array -> JM.Value)
encodeArray = define "encodeArray" $
  doc "Encode an Avro array schema to a JSON object" $
  lambda "arr" $
    inject JM._Value JM._Value_object
      (Maps.fromList (list [
        pair (string "type") (inject JM._Value JM._Value_string (string "array")),
        pair (string "items") (encodeSchema @@ (project Avro._Array Avro._Array_items @@ var "arr"))]))

encodeMap :: TTermDefinition (Avro.Map -> JM.Value)
encodeMap = define "encodeMap" $
  doc "Encode an Avro map schema to a JSON object" $
  lambda "mp" $
    inject JM._Value JM._Value_object
      (Maps.fromList (list [
        pair (string "type") (inject JM._Value JM._Value_string (string "map")),
        pair (string "values") (encodeSchema @@ (project Avro._Map Avro._Map_values @@ var "mp"))]))

encodeNamed :: TTermDefinition (Avro.Named -> JM.Value)
encodeNamed = define "encodeNamed" $
  doc "Encode an Avro named type to a JSON object" $
  lambda "n" $
    inject JM._Value JM._Value_object
      (Maps.fromList (Lists.concat (list [
        list [pair (string "name") (inject JM._Value JM._Value_string (project Avro._Named Avro._Named_name @@ var "n"))],
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "ns" $ list [pair (string "namespace") (inject JM._Value JM._Value_string (var "ns"))]) (project Avro._Named Avro._Named_namespace @@ var "n"),
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "doc") (inject JM._Value JM._Value_string (var "d"))]) (project Avro._Named Avro._Named_doc @@ var "n"),
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "als" $ list [pair (string "aliases") (inject JM._Value JM._Value_array (Lists.map (lambda "a" $ inject JM._Value JM._Value_string (var "a")) (var "als")))]) (project Avro._Named Avro._Named_aliases @@ var "n"),
        encodeNamedType @@ (project Avro._Named Avro._Named_type @@ var "n"),
        encodeAnnotations @@ (project Avro._Named Avro._Named_annotations @@ var "n")])))

encodeNamedType :: TTermDefinition (Avro.NamedType -> [(String, JM.Value)])
encodeNamedType = define "encodeNamedType" $
  doc "Encode the specific variant of a named Avro type" $
  lambda "nt" $
    cases Avro._NamedType (var "nt") Nothing [
      Avro._NamedType_enum>>: lambda "e" $ encodeEnumE @@ var "e",
      Avro._NamedType_fixed>>: lambda "f" $ encodeFixedE @@ var "f",
      Avro._NamedType_record>>: lambda "r" $ encodeRecordE @@ var "r"]

encodeEnumE :: TTermDefinition (Avro.Enum -> [(String, JM.Value)])
encodeEnumE = define "encodeEnum" $
  doc "Encode an Avro enum type as key-value pairs" $
  lambda "e" $
    Lists.concat (list [
      list [pair (string "type") (inject JM._Value JM._Value_string (string "enum"))],
      list [pair (string "symbols") (inject JM._Value JM._Value_array (Lists.map (lambda "s" $ inject JM._Value JM._Value_string (var "s")) (project Avro._Enum Avro._Enum_symbols @@ var "e")))],
      Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "default") (inject JM._Value JM._Value_string (var "d"))]) (project Avro._Enum Avro._Enum_default @@ var "e")])

encodeFixedE :: TTermDefinition (Avro.Fixed -> [(String, JM.Value)])
encodeFixedE = define "encodeFixed" $
  doc "Encode an Avro fixed type as key-value pairs" $
  lambda "f" $
    list [
      pair (string "type") (inject JM._Value JM._Value_string (string "fixed")),
      pair (string "size") (inject JM._Value JM._Value_number (Literals.bigintToBigfloat (Literals.int32ToBigint (project Avro._Fixed Avro._Fixed_size @@ var "f"))))]

encodeRecordE :: TTermDefinition (Avro.Record -> [(String, JM.Value)])
encodeRecordE = define "encodeRecord" $
  doc "Encode an Avro record type as key-value pairs" $
  lambda "r" $
    list [
      pair (string "type") (inject JM._Value JM._Value_string (string "record")),
      pair (string "fields") (inject JM._Value JM._Value_array (Lists.map encodeFieldE (project Avro._Record Avro._Record_fields @@ var "r")))]

encodeFieldE :: TTermDefinition (Avro.Field -> JM.Value)
encodeFieldE = define "encodeField" $
  doc "Encode an Avro field to a JSON object" $
  lambda "f" $
    inject JM._Value JM._Value_object
      (Maps.fromList (Lists.concat (list [
        list [pair (string "name") (inject JM._Value JM._Value_string (project Avro._Field Avro._Field_name @@ var "f"))],
        list [pair (string "type") (encodeSchema @@ (project Avro._Field Avro._Field_type @@ var "f"))],
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "doc") (inject JM._Value JM._Value_string (var "d"))]) (project Avro._Field Avro._Field_doc @@ var "f"),
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "default") (var "d")]) (project Avro._Field Avro._Field_default @@ var "f"),
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "o" $ list [encodeOrderE @@ var "o"]) (project Avro._Field Avro._Field_order @@ var "f"),
        Maybes.maybe (list ([] :: [TTerm (String, JM.Value)])) (lambda "als" $ list [pair (string "aliases") (inject JM._Value JM._Value_array (Lists.map (lambda "a" $ inject JM._Value JM._Value_string (var "a")) (var "als")))]) (project Avro._Field Avro._Field_aliases @@ var "f"),
        encodeAnnotations @@ (project Avro._Field Avro._Field_annotations @@ var "f")])))

encodeOrderE :: TTermDefinition (Avro.Order -> (String, JM.Value))
encodeOrderE = define "encodeOrder" $
  doc "Encode an Avro field ordering as a key-value pair" $
  lambda "o" $
    pair (string "order") (inject JM._Value JM._Value_string
      (cases Avro._Order (var "o") Nothing [
        Avro._Order_ascending>>: constant (string "ascending"),
        Avro._Order_descending>>: constant (string "descending"),
        Avro._Order_ignore>>: constant (string "ignore")]))

encodeUnion :: TTermDefinition (Avro.Union -> JM.Value)
encodeUnion = define "encodeUnion" $
  doc "Encode an Avro union as a JSON array of schemas" $
  lambda "u" $
    inject JM._Value JM._Value_array
      (Lists.map encodeSchema (unwrap Avro._Union @@ var "u"))

encodeAnnotations :: TTermDefinition (M.Map String JM.Value -> [(String, JM.Value)])
encodeAnnotations = define "encodeAnnotations" $
  doc "Encode annotations as key-value pairs with @ prefix on keys" $
  lambda "m" $
    Lists.map
      (lambda "entry" $ pair
        (Strings.cat2 (string "@") (Pairs.first (var "entry")))
        (Pairs.second (var "entry")))
      (Maps.toList (var "m"))


-- | Coder functions

avroSchemaJsonCoder :: TTermDefinition (Context -> Coder Avro.Schema JM.Value)
avroSchemaJsonCoder = define "avroSchemaJsonCoder" $
  doc "Create a coder between Avro schemas and JSON values" $
  lambda "cx" $
    record _Coder [
      _Coder_encode>>: lambda "_cx" $ lambda "schema" $ Phantoms.right (encodeSchema @@ var "schema"),
      _Coder_decode>>: lambda "cx2" $ lambda "json" $ decodeSchema @@ var "cx2" @@ var "json"]

avroSchemaStringCoder :: TTermDefinition (Context -> Coder Avro.Schema String)
avroSchemaStringCoder = define "avroSchemaStringCoder" $
  doc "Create a coder between Avro schemas and JSON strings" $
  lambda "cx" $
    record _Coder [
      _Coder_encode>>: lambda "_cx" $ lambda "schema" $
        Phantoms.right (showJsonValue @@ (encodeSchema @@ var "schema")),
      _Coder_decode>>: lambda "cx2" $ lambda "s" $
        Eithers.bind
          (Eithers.either_
            (lambda "e" $ err @@ var "cx2" @@ var "e")
            (lambda "v" $ Phantoms.right (var "v"))
            (stringToJsonValue @@ var "s"))
          (lambda "json" $ decodeSchema @@ var "cx2" @@ var "json")]


-- | Decode functions

decodeAliases :: TTermDefinition (Context -> M.Map String JM.Value -> Result (Maybe [String]))
decodeAliases = define "decodeAliases" $
  doc "Decode aliases from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (optArrayE @@ var "cx" @@ avro_aliases @@ var "m")
      (lambda "mArr" $
        Maybes.maybe
          (Phantoms.right nothing)
          (lambda "arr" $
            Eithers.map
              (lambda "strs" $ Maybes.pure (var "strs"))
              (Eithers.mapList (expectStringE @@ var "cx") (var "arr")))
          (var "mArr"))

decodeEnum :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.NamedType)
decodeEnum = define "decodeEnum" $
  doc "Decode an Avro enum type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireArrayE @@ var "cx" @@ avro_symbols @@ var "m")
      (lambda "syms" $
        Eithers.bind (Eithers.mapList (expectStringE @@ var "cx") (var "syms"))
          (lambda "symbols" $
            Eithers.bind (optStringE @@ var "cx" @@ avro_default @@ var "m")
              (lambda "defVal" $
                Phantoms.right (inject Avro._NamedType Avro._NamedType_enum
                  (record Avro._Enum [
                    Avro._Enum_symbols>>: var "symbols",
                    Avro._Enum_default>>: var "defVal"])))))

decodeField :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.Field)
decodeField = define "decodeField" $
  doc "Decode an Avro field from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireStringE @@ var "cx" @@ avro_name @@ var "m")
      (lambda "name" $
        Eithers.bind (optStringE @@ var "cx" @@ avro_doc @@ var "m")
          (lambda "fdoc" $
            Eithers.bind (requireE @@ var "cx" @@ avro_type @@ var "m")
              (lambda "typeJson" $
                Eithers.bind (decodeSchema @@ var "cx" @@ var "typeJson")
                  (lambda "fieldType" $
                    Eithers.bind (Eithers.bind (optStringE @@ var "cx" @@ avro_order @@ var "m")
                      (lambda "mOrd" $
                        Eithers.mapMaybe (decodeOrder @@ var "cx") (var "mOrd")))
                      (lambda "order" $
                        Eithers.bind (decodeAliases @@ var "cx" @@ var "m")
                          (lambda "aliases" $
                            Phantoms.right (record Avro._Field [
                              Avro._Field_name>>: var "name",
                              Avro._Field_doc>>: var "fdoc",
                              Avro._Field_type>>: var "fieldType",
                              Avro._Field_default>>: optE @@ avro_default @@ var "m",
                              Avro._Field_order>>: var "order",
                              Avro._Field_aliases>>: var "aliases",
                              Avro._Field_annotations>>: getAnnotations @@ var "m"])))))))

decodeFixed :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.NamedType)
decodeFixed = define "decodeFixed" $
  doc "Decode an Avro fixed type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireNumberE @@ var "cx" @@ avro_size @@ var "m")
      (lambda "n" $
        lets ["size">: Literals.bigintToInt32 (Literals.bigfloatToBigint (var "n"))] $
        Phantoms.right $ inject Avro._NamedType Avro._NamedType_fixed $
          record Avro._Fixed [
            Avro._Fixed_size>>: var "size"])

decodeNamedSchema :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.NamedType -> Result Avro.Schema)
decodeNamedSchema = define "decodeNamedSchema" $
  doc "Decode a named Avro schema from a JSON object map and a decoded named type result" $
  lambda "cx" $ lambda "m" $ lambda "namedTypeResult" $
    Eithers.bind (requireStringE @@ var "cx" @@ avro_name @@ var "m")
      (lambda "name" $
        Eithers.bind (optStringE @@ var "cx" @@ avro_namespace @@ var "m")
          (lambda "ns" $
            Eithers.bind (optStringE @@ var "cx" @@ avro_doc @@ var "m")
              (lambda "sdoc" $
                Eithers.bind (decodeAliases @@ var "cx" @@ var "m")
                  (lambda "aliases" $
                    Eithers.bind (var "namedTypeResult")
                      (lambda "namedType" $
                        Phantoms.right (inject Avro._Schema Avro._Schema_named
                          (record Avro._Named [
                            Avro._Named_name>>: var "name",
                            Avro._Named_namespace>>: var "ns",
                            Avro._Named_aliases>>: var "aliases",
                            Avro._Named_doc>>: var "sdoc",
                            Avro._Named_type>>: var "namedType",
                            Avro._Named_annotations>>: getAnnotations @@ var "m"])))))))

decodeOrder :: TTermDefinition (Context -> String -> Result Avro.Order)
decodeOrder = define "decodeOrder" $
  doc "Decode an Avro field ordering from a string" $
  lambda "cx" $ lambda "o" $
    Logic.ifElse (Equality.equal (var "o") (string "ascending"))
      (Phantoms.right (inject Avro._Order Avro._Order_ascending unit))
      (Logic.ifElse (Equality.equal (var "o") (string "descending"))
        (Phantoms.right (inject Avro._Order Avro._Order_descending unit))
        (Logic.ifElse (Equality.equal (var "o") (string "ignore"))
          (Phantoms.right (inject Avro._Order Avro._Order_ignore unit))
          (err @@ var "cx" @@ (Strings.cat $ list [string "unknown order: ", var "o"]))))

decodeRecord :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.NamedType)
decodeRecord = define "decodeRecord" $
  doc "Decode an Avro record type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireArrayE @@ var "cx" @@ avro_fields @@ var "m")
      (lambda "fieldJsons" $
        Eithers.bind (Eithers.mapList
          (lambda "fj" $
            Eithers.bind (expectObjectE @@ var "cx" @@ var "fj")
              (lambda "fm" $ decodeField @@ var "cx" @@ var "fm"))
          (var "fieldJsons"))
          (lambda "fields" $
            Phantoms.right (inject Avro._NamedType Avro._NamedType_record
              (record Avro._Record [
                Avro._Record_fields>>: var "fields"]))))

decodePrimitiveName :: TTermDefinition (String -> Maybe Avro.Primitive)
decodePrimitiveName = define "decodePrimitiveName" $
  doc "Decode a primitive type name string to a Primitive, or Nothing if not a primitive" $
  lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "null"))
      (just (inject Avro._Primitive Avro._Primitive_null unit))
      (Logic.ifElse (Equality.equal (var "s") (string "boolean"))
        (just (inject Avro._Primitive Avro._Primitive_boolean unit))
        (Logic.ifElse (Equality.equal (var "s") (string "int"))
          (just (inject Avro._Primitive Avro._Primitive_int unit))
          (Logic.ifElse (Equality.equal (var "s") (string "long"))
            (just (inject Avro._Primitive Avro._Primitive_long unit))
            (Logic.ifElse (Equality.equal (var "s") (string "float"))
              (just (inject Avro._Primitive Avro._Primitive_float unit))
              (Logic.ifElse (Equality.equal (var "s") (string "double"))
                (just (inject Avro._Primitive Avro._Primitive_double unit))
                (Logic.ifElse (Equality.equal (var "s") (string "bytes"))
                  (just (inject Avro._Primitive Avro._Primitive_bytes unit))
                  (Logic.ifElse (Equality.equal (var "s") (string "string"))
                    (just (inject Avro._Primitive Avro._Primitive_string unit))
                    nothing)))))))

decodeSchema :: TTermDefinition (Context -> JM.Value -> Result Avro.Schema)
decodeSchema = define "decodeSchema" $
  doc "Decode an Avro schema from a JSON value" $
  lambda "cx" $ lambda "v" $
    cases JM._Value (var "v") (Just (err @@ var "cx" @@ (Strings.cat $ list [string "unexpected JSON value for schema: ", showJsonValue @@ var "v"]))) [
      -- String: primitive type name or reference
      JM._Value_string>>: lambda "s" $
        Maybes.maybe
          (Phantoms.right (inject Avro._Schema Avro._Schema_reference (var "s")))
          (lambda "p" $ Phantoms.right (inject Avro._Schema Avro._Schema_primitive (var "p")))
          (decodePrimitiveName @@ var "s"),
      -- Array: union type
      JM._Value_array>>: lambda "schemas" $
        Eithers.map
          (lambda "decoded" $ inject Avro._Schema Avro._Schema_union (wrap Avro._Union (var "decoded")))
          (Eithers.mapList (decodeSchema @@ var "cx") (var "schemas")),
      -- Object: named type or container
      JM._Value_object>>: lambda "m" $
        Eithers.bind (requireStringE @@ var "cx" @@ avro_type @@ var "m")
          (lambda "typeName" $ decodeObjectSchema @@ var "cx" @@ var "m" @@ var "typeName")]

decodeObjectSchema :: TTermDefinition (Context -> M.Map String JM.Value -> String -> Result Avro.Schema)
decodeObjectSchema = define "decodeObjectSchema" $
  doc "Decode an Avro schema from a JSON object given the type name" $
  lambda "cx" $ lambda "m" $ lambda "typeName" $
    Logic.ifElse (Equality.equal (var "typeName") (string "array"))
      (decodeArraySchema @@ var "cx" @@ var "m")
      (Logic.ifElse (Equality.equal (var "typeName") (string "map"))
        (decodeMapSchema @@ var "cx" @@ var "m")
        (Logic.ifElse (Equality.equal (var "typeName") (string "record"))
          (decodeNamedSchema @@ var "cx" @@ var "m" @@ (decodeRecord @@ var "cx" @@ var "m"))
          (Logic.ifElse (Equality.equal (var "typeName") (string "enum"))
            (decodeNamedSchema @@ var "cx" @@ var "m" @@ (decodeEnum @@ var "cx" @@ var "m"))
            (Logic.ifElse (Equality.equal (var "typeName") (string "fixed"))
              (decodeNamedSchema @@ var "cx" @@ var "m" @@ (decodeFixed @@ var "cx" @@ var "m"))
              -- Primitive type as object (unusual but valid, e.g. {"type": "string"})
              (Maybes.maybe
                (err @@ var "cx" @@ (Strings.cat $ list [string "unknown type: ", var "typeName"]))
                (lambda "p" $ Phantoms.right (inject Avro._Schema Avro._Schema_primitive (var "p")))
                (decodePrimitiveName @@ var "typeName"))))))

decodeArraySchema :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.Schema)
decodeArraySchema = define "decodeArraySchema" $
  doc "Decode an Avro array schema from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ avro_items @@ var "m")
      (lambda "items" $
        Eithers.map
          (lambda "s" $ inject Avro._Schema Avro._Schema_array (record Avro._Array [
            Avro._Array_items>>: var "s"]))
          (decodeSchema @@ var "cx" @@ var "items"))

decodeMapSchema :: TTermDefinition (Context -> M.Map String JM.Value -> Result Avro.Schema)
decodeMapSchema = define "decodeMapSchema" $
  doc "Decode an Avro map schema from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ avro_values @@ var "m")
      (lambda "values" $
        Eithers.map
          (lambda "s" $ inject Avro._Schema Avro._Schema_map (record Avro._Map [
            Avro._Map_values>>: var "s"]))
          (decodeSchema @@ var "cx" @@ var "values"))

getAnnotations :: TTermDefinition (M.Map String JM.Value -> M.Map String JM.Value)
getAnnotations = define "getAnnotations" $
  doc "Extract annotation entries (keys starting with @) from a JSON object map" $
  lambda "m" $
    Maps.fromList (Maybes.cat (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        Logic.ifElse
          (Equality.equal (Strings.charAt (int32 0) (var "k")) (int32 64))  -- 64 = '@'
          (Maybes.pure (pair (Strings.fromList (Lists.drop (int32 1) (Strings.toList (var "k")))) (var "v")))
          nothing)
      (Maps.toList (var "m"))))
