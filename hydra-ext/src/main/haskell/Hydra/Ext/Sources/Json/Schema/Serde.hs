module Hydra.Ext.Sources.Json.Schema.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
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
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
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
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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


define :: String -> TTerm a -> TBinding a
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
      toBinding key_additionalItems,
      toBinding key_additionalProperties,
      toBinding key_allOf,
      toBinding key_anyOf,
      toBinding key_definitions,
      toBinding key_dependencies,
      toBinding key_description_,
      toBinding key_enum,
      toBinding key_exclusiveMaximum,
      toBinding key_exclusiveMinimum,
      toBinding key_id,
      toBinding key_items,
      toBinding key_label,
      toBinding key_maxItems,
      toBinding key_maxLength_,
      toBinding key_maxProperties,
      toBinding key_maximum,
      toBinding key_minItems,
      toBinding key_minimum,
      toBinding key_minLength_,
      toBinding key_minProperties,
      toBinding key_multipleOf,
      toBinding key_not,
      toBinding key_oneOf,
      toBinding key_pattern,
      toBinding key_patternProperties,
      toBinding key_properties,
      toBinding key_ref,
      toBinding key_required,
      toBinding key_schema,
      toBinding key_title,
      toBinding key_type_,
      toBinding key_uniqueItems,
      toBinding encodeAdditionalItems,
      toBinding encodeArrayRestriction,
      toBinding encodeInteger,
      toBinding encodeItems,
      toBinding encodeKeyword,
      toBinding encodeKeywordSchemaOrArray,
      toBinding encodeMultipleRestriction,
      toBinding encodeNumericRestriction,
      toBinding encodePatternProperty,
      toBinding encodeProperty,
      toBinding encodeObjectRestriction,
      toBinding encodeRestriction,
      toBinding encodeStringRestriction,
      toBinding encodeSchema,
      toBinding encodeSchemaOrArray,
      toBinding encodeSchemaReference,
      toBinding encodeType,
      toBinding encodeTypeName,
      toBinding jsonSchemaDocumentToJsonValue,
      toBinding jsonSchemaDocumentToString,
      toBinding fromObject,
      toBinding toObject]


-- String constant keys

key_additionalItems :: TBinding String
key_additionalItems = define "key_additionalItems" $
  string "additionalItems"

key_additionalProperties :: TBinding String
key_additionalProperties = define "key_additionalProperties" $
  string "additionalProperties"

key_allOf :: TBinding String
key_allOf = define "key_allOf" $
  string "allOf"

key_anyOf :: TBinding String
key_anyOf = define "key_anyOf" $
  string "anyOf"

key_definitions :: TBinding String
key_definitions = define "key_definitions" $
  string "$defs"

key_dependencies :: TBinding String
key_dependencies = define "key_dependencies" $
  string "dependencies"

key_description_ :: TBinding String
key_description_ = define "key_description" $
  string "description"

key_enum :: TBinding String
key_enum = define "key_enum" $
  string "enum"

key_exclusiveMaximum :: TBinding String
key_exclusiveMaximum = define "key_exclusiveMaximum" $
  string "exclusiveMaximum"

key_exclusiveMinimum :: TBinding String
key_exclusiveMinimum = define "key_exclusiveMinimum" $
  string "exclusiveMinimum"

key_id :: TBinding String
key_id = define "key_id" $
  string "$id"

key_items :: TBinding String
key_items = define "key_items" $
  string "items"

key_label :: TBinding String
key_label = define "key_label" $
  string "label"

key_maxItems :: TBinding String
key_maxItems = define "key_maxItems" $
  string "maxItems"

key_maxLength_ :: TBinding String
key_maxLength_ = define "key_maxLength" $
  string "maxLength"

key_maxProperties :: TBinding String
key_maxProperties = define "key_maxProperties" $
  string "maxProperties"

key_maximum :: TBinding String
key_maximum = define "key_maximum" $
  string "maximum"

key_minItems :: TBinding String
key_minItems = define "key_minItems" $
  string "minItems"

key_minimum :: TBinding String
key_minimum = define "key_minimum" $
  string "minimum"

key_minLength_ :: TBinding String
key_minLength_ = define "key_minLength" $
  string "minLength"

key_minProperties :: TBinding String
key_minProperties = define "key_minProperties" $
  string "minProperties"

key_multipleOf :: TBinding String
key_multipleOf = define "key_multipleOf" $
  string "multipleOf"

key_not :: TBinding String
key_not = define "key_not" $
  string "not"

key_oneOf :: TBinding String
key_oneOf = define "key_oneOf" $
  string "oneOf"

key_pattern :: TBinding String
key_pattern = define "key_pattern" $
  string "pattern"

key_patternProperties :: TBinding String
key_patternProperties = define "key_patternProperties" $
  string "patternProperties"

key_properties :: TBinding String
key_properties = define "key_properties" $
  string "properties"

key_ref :: TBinding String
key_ref = define "key_ref" $
  string "$ref"

key_required :: TBinding String
key_required = define "key_required" $
  string "required"

key_schema :: TBinding String
key_schema = define "key_schema" $
  string "$schema"

key_title :: TBinding String
key_title = define "key_title" $
  string "title"

key_type_ :: TBinding String
key_type_ = define "key_type" $
  string "type"

key_uniqueItems :: TBinding String
key_uniqueItems = define "key_uniqueItems" $
  string "uniqueItems"


-- Encoding functions

encodeAdditionalItems :: TBinding (JS.AdditionalItems -> J.Value)
encodeAdditionalItems = define "encodeAdditionalItems" $
  doc "Encode additional items as a JSON value" $
  lambda "ai" $
    cases JS._AdditionalItems (var "ai") Nothing [
      JS._AdditionalItems_any>>: lambda "b" $ Json.valueBoolean (var "b"),
      JS._AdditionalItems_schema>>: lambda "schema" $ encodeSchema @@ var "schema"]

encodeArrayRestriction :: TBinding (JS.ArrayRestriction -> (String, J.Value))
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

encodeInteger :: TBinding (Int -> J.Value)
encodeInteger = define "encodeInteger" $
  doc "Encode an integer as a JSON number value" $
  lambda "n" $
    Json.valueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint (var "n")))

encodeItems :: TBinding (JS.Items -> (String, J.Value))
encodeItems = define "encodeItems" $
  doc "Encode items as a key-value pair" $
  lambda "items" $
    pair key_items
      (cases JS._Items (var "items") Nothing [
        JS._Items_sameItems>>: lambda "schema" $ encodeSchema @@ var "schema",
        JS._Items_varItems>>: lambda "schemas" $
          Json.valueArray (Lists.map encodeSchema (var "schemas"))])

encodeKeyword :: TBinding (JS.Keyword -> J.Value)
encodeKeyword = define "encodeKeyword" $
  doc "Encode a keyword as a JSON string value" $
  lambda "k" $
    Json.valueString (unwrap JS._Keyword @@ var "k")

encodeKeywordSchemaOrArray :: TBinding ((JS.Keyword, JS.SchemaOrArray) -> (String, J.Value))
encodeKeywordSchemaOrArray = define "encodeKeywordSchemaOrArray" $
  doc "Encode a keyword-schema-or-array pair as a key-value pair" $
  lambda "p" $ lets [
    "k">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._Keyword @@ var "k") (encodeSchemaOrArray @@ var "s")

encodeMultipleRestriction :: TBinding (JS.MultipleRestriction -> (String, J.Value))
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

encodeNumericRestriction :: TBinding (JS.NumericRestriction -> [(String, J.Value)])
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

encodePatternProperty :: TBinding ((JS.RegularExpression, JS.Schema) -> (String, J.Value))
encodePatternProperty = define "encodePatternProperty" $
  doc "Encode a pattern property pair as a key-value pair" $
  lambda "p" $ lets [
    "pat">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._RegularExpression @@ var "pat") (encodeSchema @@ var "s")

encodeProperty :: TBinding ((JS.Keyword, JS.Schema) -> (String, J.Value))
encodeProperty = define "encodeProperty" $
  doc "Encode a property pair as a key-value pair" $
  lambda "p" $ lets [
    "k">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._Keyword @@ var "k") (encodeSchema @@ var "s")

encodeObjectRestriction :: TBinding (JS.ObjectRestriction -> (String, J.Value))
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

encodeRestriction :: TBinding (JS.Restriction -> [(String, J.Value)])
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

encodeStringRestriction :: TBinding (JS.StringRestriction -> (String, J.Value))
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

encodeSchema :: TBinding (JS.Schema -> J.Value)
encodeSchema = define "encodeSchema" $
  doc "Encode a schema as a JSON object value" $
  lambda "s" $
    Json.valueObject (Maps.fromList (Lists.concat (Lists.map encodeRestriction (unwrap JS._Schema @@ var "s"))))

encodeSchemaOrArray :: TBinding (JS.SchemaOrArray -> J.Value)
encodeSchemaOrArray = define "encodeSchemaOrArray" $
  doc "Encode a schema or array as a JSON value" $
  lambda "soa" $
    cases JS._SchemaOrArray (var "soa") Nothing [
      JS._SchemaOrArray_schema>>: lambda "s" $ encodeSchema @@ var "s",
      JS._SchemaOrArray_array>>: lambda "keys" $
        Json.valueArray (Lists.map encodeKeyword (var "keys"))]

encodeSchemaReference :: TBinding (JS.SchemaReference -> J.Value)
encodeSchemaReference = define "encodeSchemaReference" $
  doc "Encode a schema reference as a JSON string value" $
  lambda "sr" $
    Json.valueString (unwrap JS._SchemaReference @@ var "sr")

encodeType :: TBinding (JS.Type -> J.Value)
encodeType = define "encodeType" $
  doc "Encode a type as a JSON value" $
  lambda "t" $
    cases JS._Type (var "t") Nothing [
      JS._Type_single>>: lambda "name" $ encodeTypeName @@ var "name",
      JS._Type_multiple>>: lambda "names" $
        Json.valueArray (Lists.map encodeTypeName (var "names"))]

encodeTypeName :: TBinding (JS.TypeName -> J.Value)
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

jsonSchemaDocumentToJsonValue :: TBinding (JS.Document -> J.Value)
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

jsonSchemaDocumentToString :: TBinding (JS.Document -> String)
jsonSchemaDocumentToString = define "jsonSchemaDocumentToString" $
  doc "Convert a JSON Schema document to a JSON string" $
  lambda "doc" $
    var "hydra.json.writer.printJson" @@ (jsonSchemaDocumentToJsonValue @@ var "doc")

fromObject :: TBinding (J.Value -> M.Map String J.Value)
fromObject = define "fromObject" $
  doc "Extract the map from a JSON object value" $
  lambda "v" $
    cases J._Value (var "v") Nothing [
      J._Value_object>>: lambda "mp" $ var "mp"]

toObject :: TBinding ([(String, Maybe J.Value)] -> J.Value)
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
