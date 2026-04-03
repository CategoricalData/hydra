module Hydra.Ext.Sources.Json.Schema.Serde where

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
import qualified Hydra.Dsl.Module                     as Module
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
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json.Model as J


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

jsonSchemaSyntaxNs :: Namespace
jsonSchemaSyntaxNs = Namespace "hydra.ext.org.json.schema"

jsonWriterNs :: Namespace
jsonWriterNs = Namespace "hydra.json.writer"

ns :: Namespace
ns = Namespace "hydra.ext.json.schema.serde"

module_ :: Module
module_ = Module ns elements
    [jsonWriterNs]
    (jsonSchemaSyntaxNs:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting JSON Schema documents to JSON values"
  where
    elements = [
      toDefinition key_additionalItems,
      toDefinition key_additionalProperties,
      toDefinition key_allOf,
      toDefinition key_anyOf,
      toDefinition key_definitions,
      toDefinition key_dependencies,
      toDefinition key_description_,
      toDefinition key_enum,
      toDefinition key_exclusiveMaximum,
      toDefinition key_exclusiveMinimum,
      toDefinition key_id,
      toDefinition key_items,
      toDefinition key_label,
      toDefinition key_maxItems,
      toDefinition key_maxLength_,
      toDefinition key_maxProperties,
      toDefinition key_maximum,
      toDefinition key_minItems,
      toDefinition key_minimum,
      toDefinition key_minLength_,
      toDefinition key_minProperties,
      toDefinition key_multipleOf,
      toDefinition key_not,
      toDefinition key_oneOf,
      toDefinition key_pattern,
      toDefinition key_patternProperties,
      toDefinition key_properties,
      toDefinition key_ref,
      toDefinition key_required,
      toDefinition key_schema,
      toDefinition key_title,
      toDefinition key_type_,
      toDefinition key_uniqueItems,
      toDefinition encodeAdditionalItems,
      toDefinition encodeArrayRestriction,
      toDefinition encodeInteger,
      toDefinition encodeItems,
      toDefinition encodeKeyword,
      toDefinition encodeKeywordSchemaOrArray,
      toDefinition encodeMultipleRestriction,
      toDefinition encodeNumericRestriction,
      toDefinition encodePatternProperty,
      toDefinition encodeProperty,
      toDefinition encodeObjectRestriction,
      toDefinition encodeRestriction,
      toDefinition encodeStringRestriction,
      toDefinition encodeSchema,
      toDefinition encodeSchemaOrArray,
      toDefinition encodeSchemaReference,
      toDefinition encodeType,
      toDefinition encodeTypeName,
      toDefinition jsonSchemaDocumentToJsonValue,
      toDefinition jsonSchemaDocumentToString,
      toDefinition fromObject,
      toDefinition toObject]


-- String constant keys

key_additionalItems :: TTermDefinition String
key_additionalItems = define "key_additionalItems" $
  string "additionalItems"

key_additionalProperties :: TTermDefinition String
key_additionalProperties = define "key_additionalProperties" $
  string "additionalProperties"

key_allOf :: TTermDefinition String
key_allOf = define "key_allOf" $
  string "allOf"

key_anyOf :: TTermDefinition String
key_anyOf = define "key_anyOf" $
  string "anyOf"

key_definitions :: TTermDefinition String
key_definitions = define "key_definitions" $
  string "$defs"

key_dependencies :: TTermDefinition String
key_dependencies = define "key_dependencies" $
  string "dependencies"

key_description_ :: TTermDefinition String
key_description_ = define "key_description" $
  string "description"

key_enum :: TTermDefinition String
key_enum = define "key_enum" $
  string "enum"

key_exclusiveMaximum :: TTermDefinition String
key_exclusiveMaximum = define "key_exclusiveMaximum" $
  string "exclusiveMaximum"

key_exclusiveMinimum :: TTermDefinition String
key_exclusiveMinimum = define "key_exclusiveMinimum" $
  string "exclusiveMinimum"

key_id :: TTermDefinition String
key_id = define "key_id" $
  string "$id"

key_items :: TTermDefinition String
key_items = define "key_items" $
  string "items"

key_label :: TTermDefinition String
key_label = define "key_label" $
  string "label"

key_maxItems :: TTermDefinition String
key_maxItems = define "key_maxItems" $
  string "maxItems"

key_maxLength_ :: TTermDefinition String
key_maxLength_ = define "key_maxLength" $
  string "maxLength"

key_maxProperties :: TTermDefinition String
key_maxProperties = define "key_maxProperties" $
  string "maxProperties"

key_maximum :: TTermDefinition String
key_maximum = define "key_maximum" $
  string "maximum"

key_minItems :: TTermDefinition String
key_minItems = define "key_minItems" $
  string "minItems"

key_minimum :: TTermDefinition String
key_minimum = define "key_minimum" $
  string "minimum"

key_minLength_ :: TTermDefinition String
key_minLength_ = define "key_minLength" $
  string "minLength"

key_minProperties :: TTermDefinition String
key_minProperties = define "key_minProperties" $
  string "minProperties"

key_multipleOf :: TTermDefinition String
key_multipleOf = define "key_multipleOf" $
  string "multipleOf"

key_not :: TTermDefinition String
key_not = define "key_not" $
  string "not"

key_oneOf :: TTermDefinition String
key_oneOf = define "key_oneOf" $
  string "oneOf"

key_pattern :: TTermDefinition String
key_pattern = define "key_pattern" $
  string "pattern"

key_patternProperties :: TTermDefinition String
key_patternProperties = define "key_patternProperties" $
  string "patternProperties"

key_properties :: TTermDefinition String
key_properties = define "key_properties" $
  string "properties"

key_ref :: TTermDefinition String
key_ref = define "key_ref" $
  string "$ref"

key_required :: TTermDefinition String
key_required = define "key_required" $
  string "required"

key_schema :: TTermDefinition String
key_schema = define "key_schema" $
  string "$schema"

key_title :: TTermDefinition String
key_title = define "key_title" $
  string "title"

key_type_ :: TTermDefinition String
key_type_ = define "key_type" $
  string "type"

key_uniqueItems :: TTermDefinition String
key_uniqueItems = define "key_uniqueItems" $
  string "uniqueItems"


-- Encoding functions

encodeAdditionalItems :: TTermDefinition (JS.AdditionalItems -> J.Value)
encodeAdditionalItems = define "encodeAdditionalItems" $
  doc "Encode additional items as a JSON value" $
  lambda "ai" $
    cases JS._AdditionalItems (var "ai") Nothing [
      JS._AdditionalItems_any>>: lambda "b" $ Json.valueBoolean (var "b"),
      JS._AdditionalItems_schema>>: lambda "schema" $ encodeSchema @@ var "schema"]

encodeArrayRestriction :: TTermDefinition (JS.ArrayRestriction -> (String, J.Value))
encodeArrayRestriction = define "encodeArrayRestriction" $
  doc "Encode an array restriction as a key-value pair" $
  lambda "r" $
    cases JS._ArrayRestriction (var "r") Nothing [
      JS._ArrayRestriction_items>>: lambda "items" $ encodeItems @@ var "items",
      JS._ArrayRestriction_additionalItems>>: lambda "ai" $
        pair key_additionalItems (encodeAdditionalItems @@ var "ai"),
      JS._ArrayRestriction_minItems>>: lambda "n" $
        pair key_minItems (encodeInteger @@ var "n"),
      JS._ArrayRestriction_maxItems>>: lambda "n" $
        pair key_maxItems (encodeInteger @@ var "n"),
      JS._ArrayRestriction_uniqueItems>>: lambda "b" $
        pair key_uniqueItems (Json.valueBoolean (var "b"))]

encodeInteger :: TTermDefinition (Int -> J.Value)
encodeInteger = define "encodeInteger" $
  doc "Encode an integer as a JSON number value" $
  lambda "n" $
    Json.valueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint (var "n")))

encodeItems :: TTermDefinition (JS.Items -> (String, J.Value))
encodeItems = define "encodeItems" $
  doc "Encode items as a key-value pair" $
  lambda "items" $
    pair key_items
      (cases JS._Items (var "items") Nothing [
        JS._Items_sameItems>>: lambda "schema" $ encodeSchema @@ var "schema",
        JS._Items_varItems>>: lambda "schemas" $
          Json.valueArray (Lists.map encodeSchema (var "schemas"))])

encodeKeyword :: TTermDefinition (JS.Keyword -> J.Value)
encodeKeyword = define "encodeKeyword" $
  doc "Encode a keyword as a JSON string value" $
  lambda "k" $
    Json.valueString (unwrap JS._Keyword @@ var "k")

encodeKeywordSchemaOrArray :: TTermDefinition ((JS.Keyword, JS.SchemaOrArray) -> (String, J.Value))
encodeKeywordSchemaOrArray = define "encodeKeywordSchemaOrArray" $
  doc "Encode a keyword-schema-or-array pair as a key-value pair" $
  lambda "p" $ lets [
    "k">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._Keyword @@ var "k") (encodeSchemaOrArray @@ var "s")

encodeMultipleRestriction :: TTermDefinition (JS.MultipleRestriction -> (String, J.Value))
encodeMultipleRestriction = define "encodeMultipleRestriction" $
  doc "Encode a multiple restriction as a key-value pair" $
  lambda "r" $
    cases JS._MultipleRestriction (var "r") Nothing [
      JS._MultipleRestriction_allOf>>: lambda "schemas" $
        pair key_allOf (Json.valueArray (Lists.map encodeSchema (var "schemas"))),
      JS._MultipleRestriction_anyOf>>: lambda "schemas" $
        pair key_anyOf (Json.valueArray (Lists.map encodeSchema (var "schemas"))),
      JS._MultipleRestriction_oneOf>>: lambda "schemas" $
        pair key_oneOf (Json.valueArray (Lists.map encodeSchema (var "schemas"))),
      JS._MultipleRestriction_not>>: lambda "schema" $
        pair key_not (encodeSchema @@ var "schema"),
      JS._MultipleRestriction_enum>>: lambda "values" $
        pair key_enum (Json.valueArray (var "values"))]

encodeNumericRestriction :: TTermDefinition (JS.NumericRestriction -> [(String, J.Value)])
encodeNumericRestriction = define "encodeNumericRestriction" $
  doc "Encode a numeric restriction as a list of key-value pairs" $
  lambda "r" $
    cases JS._NumericRestriction (var "r") Nothing [
      JS._NumericRestriction_minimum>>: lambda "lim" $ lets [
        "value">: project JS._Limit JS._Limit_value @@ var "lim",
        "excl">: project JS._Limit JS._Limit_exclusive @@ var "lim"] $
        Lists.concat $ list [
          list [pair key_minimum (encodeInteger @@ var "value")],
          Logic.ifElse (var "excl")
            (list [pair key_exclusiveMinimum (Json.valueBoolean true)])
            (list ([] :: [TTerm (String, J.Value)]))],
      JS._NumericRestriction_maximum>>: lambda "lim" $ lets [
        "value">: project JS._Limit JS._Limit_value @@ var "lim",
        "excl">: project JS._Limit JS._Limit_exclusive @@ var "lim"] $
        Lists.concat $ list [
          list [pair key_maximum (encodeInteger @@ var "value")],
          Logic.ifElse (var "excl")
            (list [pair key_exclusiveMaximum (Json.valueBoolean true)])
            (list ([] :: [TTerm (String, J.Value)]))],
      JS._NumericRestriction_multipleOf>>: lambda "n" $
        list [pair key_multipleOf (encodeInteger @@ var "n")]]

encodePatternProperty :: TTermDefinition ((JS.RegularExpression, JS.Schema) -> (String, J.Value))
encodePatternProperty = define "encodePatternProperty" $
  doc "Encode a pattern property pair as a key-value pair" $
  lambda "p" $ lets [
    "pat">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._RegularExpression @@ var "pat") (encodeSchema @@ var "s")

encodeProperty :: TTermDefinition ((JS.Keyword, JS.Schema) -> (String, J.Value))
encodeProperty = define "encodeProperty" $
  doc "Encode a property pair as a key-value pair" $
  lambda "p" $ lets [
    "k">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._Keyword @@ var "k") (encodeSchema @@ var "s")

encodeObjectRestriction :: TTermDefinition (JS.ObjectRestriction -> (String, J.Value))
encodeObjectRestriction = define "encodeObjectRestriction" $
  doc "Encode an object restriction as a key-value pair" $
  lambda "r" $
    cases JS._ObjectRestriction (var "r") Nothing [
      JS._ObjectRestriction_properties>>: lambda "props" $
        pair key_properties
          (Json.valueObject (Maps.fromList (Lists.map encodeProperty (Maps.toList (var "props"))))),
      JS._ObjectRestriction_additionalProperties>>: lambda "ai" $
        pair key_additionalProperties (encodeAdditionalItems @@ var "ai"),
      JS._ObjectRestriction_required>>: lambda "keys" $
        pair key_required (Json.valueArray (Lists.map encodeKeyword (var "keys"))),
      JS._ObjectRestriction_minProperties>>: lambda "n" $
        pair key_minProperties (encodeInteger @@ var "n"),
      JS._ObjectRestriction_maxProperties>>: lambda "n" $
        pair key_maxProperties (encodeInteger @@ var "n"),
      JS._ObjectRestriction_dependencies>>: lambda "deps" $
        pair key_dependencies
          (Json.valueObject (Maps.fromList (Lists.map encodeKeywordSchemaOrArray (Maps.toList (var "deps"))))),
      JS._ObjectRestriction_patternProperties>>: lambda "props" $
        pair key_patternProperties
          (Json.valueObject (Maps.fromList (Lists.map encodePatternProperty (Maps.toList (var "props")))))]

encodeRestriction :: TTermDefinition (JS.Restriction -> [(String, J.Value)])
encodeRestriction = define "encodeRestriction" $
  doc "Encode a restriction as a list of key-value pairs" $
  lambda "r" $
    cases JS._Restriction (var "r") Nothing [
      JS._Restriction_type>>: lambda "t" $
        list [pair key_type_ (encodeType @@ var "t")],
      JS._Restriction_string>>: lambda "sr" $
        list [encodeStringRestriction @@ var "sr"],
      JS._Restriction_number>>: lambda "nr" $
        encodeNumericRestriction @@ var "nr",
      JS._Restriction_array>>: lambda "ar" $
        list [encodeArrayRestriction @@ var "ar"],
      JS._Restriction_object>>: lambda "or" $
        list [encodeObjectRestriction @@ var "or"],
      JS._Restriction_multiple>>: lambda "mr" $
        list [encodeMultipleRestriction @@ var "mr"],
      JS._Restriction_reference>>: lambda "sr" $
        list [pair key_ref (encodeSchemaReference @@ var "sr")],
      JS._Restriction_title>>: lambda "s" $
        list [pair key_title (Json.valueString (var "s"))],
      JS._Restriction_description>>: lambda "s" $
        list [pair key_description_ (Json.valueString (var "s"))]]

encodeStringRestriction :: TTermDefinition (JS.StringRestriction -> (String, J.Value))
encodeStringRestriction = define "encodeStringRestriction" $
  doc "Encode a string restriction as a key-value pair" $
  lambda "r" $
    cases JS._StringRestriction (var "r") Nothing [
      JS._StringRestriction_maxLength>>: lambda "n" $
        pair key_maxLength_ (Json.valueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint (var "n")))),
      JS._StringRestriction_minLength>>: lambda "n" $
        pair key_minLength_ (Json.valueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint (var "n")))),
      JS._StringRestriction_pattern>>: lambda "re" $
        pair key_pattern (Json.valueString (unwrap JS._RegularExpression @@ var "re"))]

encodeSchema :: TTermDefinition (JS.Schema -> J.Value)
encodeSchema = define "encodeSchema" $
  doc "Encode a schema as a JSON object value" $
  lambda "s" $
    Json.valueObject (Maps.fromList (Lists.concat (Lists.map encodeRestriction (unwrap JS._Schema @@ var "s"))))

encodeSchemaOrArray :: TTermDefinition (JS.SchemaOrArray -> J.Value)
encodeSchemaOrArray = define "encodeSchemaOrArray" $
  doc "Encode a schema or array as a JSON value" $
  lambda "soa" $
    cases JS._SchemaOrArray (var "soa") Nothing [
      JS._SchemaOrArray_schema>>: lambda "s" $ encodeSchema @@ var "s",
      JS._SchemaOrArray_array>>: lambda "keys" $
        Json.valueArray (Lists.map encodeKeyword (var "keys"))]

encodeSchemaReference :: TTermDefinition (JS.SchemaReference -> J.Value)
encodeSchemaReference = define "encodeSchemaReference" $
  doc "Encode a schema reference as a JSON string value" $
  lambda "sr" $
    Json.valueString (unwrap JS._SchemaReference @@ var "sr")

encodeType :: TTermDefinition (JS.Type -> J.Value)
encodeType = define "encodeType" $
  doc "Encode a type as a JSON value" $
  lambda "t" $
    cases JS._Type (var "t") Nothing [
      JS._Type_single>>: lambda "name" $ encodeTypeName @@ var "name",
      JS._Type_multiple>>: lambda "names" $
        Json.valueArray (Lists.map encodeTypeName (var "names"))]

encodeTypeName :: TTermDefinition (JS.TypeName -> J.Value)
encodeTypeName = define "encodeTypeName" $
  doc "Encode a type name as a JSON string value" $
  lambda "t" $
    cases JS._TypeName (var "t") Nothing [
      JS._TypeName_string>>: constant $ Json.valueString (string "string"),
      JS._TypeName_integer>>: constant $ Json.valueString (string "integer"),
      JS._TypeName_number>>: constant $ Json.valueString (string "number"),
      JS._TypeName_boolean>>: constant $ Json.valueString (string "boolean"),
      JS._TypeName_null>>: constant $ Json.valueString (string "null"),
      JS._TypeName_array>>: constant $ Json.valueString (string "array"),
      JS._TypeName_object>>: constant $ Json.valueString (string "object")]

jsonSchemaDocumentToJsonValue :: TTermDefinition (JS.Document -> J.Value)
jsonSchemaDocumentToJsonValue = define "jsonSchemaDocumentToJsonValue" $
  doc "Convert a JSON Schema document to a JSON value" $
  lambda "doc" $ lets [
    "mid">: project JS._Document JS._Document_id @@ var "doc",
    "mdefs">: project JS._Document JS._Document_definitions @@ var "doc",
    "root">: project JS._Document JS._Document_root @@ var "doc",
    "schemaMap">: fromObject @@ (encodeSchema @@ var "root"),
    "restMap">: fromObject @@ (toObject @@ list [
      pair key_id (Maybes.map (lambda "i" $ Json.valueString (var "i")) (var "mid")),
      pair key_schema (Maybes.pure (Json.valueString (string "http://json-schema.org/2020-12/schema"))),
      pair key_definitions (Maybes.map
        (lambda "mp" $ Json.valueObject (Maps.fromList
          (Lists.map
            (lambda "p" $ lets [
              "k">: Pairs.first (var "p"),
              "schema">: Pairs.second (var "p")] $
              pair (unwrap JS._Keyword @@ var "k") (encodeSchema @@ var "schema"))
            (Maps.toList (var "mp")))))
        (var "mdefs"))])] $
    Json.valueObject (Maps.union (var "schemaMap") (var "restMap"))

jsonSchemaDocumentToString :: TTermDefinition (JS.Document -> String)
jsonSchemaDocumentToString = define "jsonSchemaDocumentToString" $
  doc "Convert a JSON Schema document to a JSON string" $
  lambda "doc" $
    var "hydra.json.writer.printJson" @@ (jsonSchemaDocumentToJsonValue @@ var "doc")

fromObject :: TTermDefinition (J.Value -> M.Map String J.Value)
fromObject = define "fromObject" $
  doc "Extract the map from a JSON object value" $
  lambda "v" $
    cases J._Value (var "v") Nothing [
      J._Value_object>>: lambda "mp" $ var "mp"]

toObject :: TTermDefinition ([(String, Maybe J.Value)] -> J.Value)
toObject = define "toObject" $
  doc "Construct a JSON object from a list of optional key-value pairs, filtering out Nothing values" $
  lambda "pairs" $
    Json.valueObject (Maps.fromList
      (Maybes.cat (Lists.map
        (lambda "p" $ lets [
          "k">: Pairs.first (var "p"),
          "mv">: Pairs.second (var "p")] $
          Maybes.map
            (lambda "v" $ pair (var "k") (var "v"))
            (var "mv"))
        (var "pairs"))))
