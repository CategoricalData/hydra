{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Json.Schema.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
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
import qualified Hydra.Json.Schema as JS
import qualified Hydra.Json.Model as J


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.json.schema.serde"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([jsonWriterNs] L.++ (jsonSchemaSyntaxNs:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Serialization functions for converting JSON Schema documents to JSON values")}
  where
    definitions = [
      toDefinition additionalItemsToExpr,
      toDefinition arrayRestrictionToExpr,
      toDefinition fromObject,
      toDefinition integerToExpr,
      toDefinition itemsToExpr,
      toDefinition jsonSchemaDocumentToJsonValue,
      toDefinition jsonSchemaDocumentToString,
      toDefinition keyAdditionalItems,
      toDefinition keyAdditionalProperties,
      toDefinition keyAllOf,
      toDefinition keyAnyOf,
      toDefinition keyDefinitions,
      toDefinition keyDependencies,
      toDefinition keyDescription_,
      toDefinition keyEnum,
      toDefinition keyExclusiveMaximum,
      toDefinition keyExclusiveMinimum,
      toDefinition keyId,
      toDefinition keyItems,
      toDefinition keyLabel,
      toDefinition keyMaxItems,
      toDefinition keyMaxLength_,
      toDefinition keyMaxProperties,
      toDefinition keyMaximum,
      toDefinition keyMinItems,
      toDefinition keyMinLength_,
      toDefinition keyMinProperties,
      toDefinition keyMinimum,
      toDefinition keyMultipleOf,
      toDefinition keyNot,
      toDefinition keyOneOf,
      toDefinition keyPattern,
      toDefinition keyPatternProperties,
      toDefinition keyProperties,
      toDefinition keyRef,
      toDefinition keyRequired,
      toDefinition keySchema,
      toDefinition keyTitle,
      toDefinition keyType_,
      toDefinition keyUniqueItems,
      toDefinition keywordSchemaOrArrayToExpr,
      toDefinition keywordToExpr,
      toDefinition multipleRestrictionToExpr,
      toDefinition numericRestrictionToExpr,
      toDefinition objectRestrictionToExpr,
      toDefinition patternPropertyToExpr,
      toDefinition propertyToExpr,
      toDefinition restrictionToExpr,
      toDefinition schemaOrArrayToExpr,
      toDefinition schemaReferenceToExpr,
      toDefinition schemaToExpr,
      toDefinition stringRestrictionToExpr,
      toDefinition toObject,
      toDefinition typeNameToExpr,
      toDefinition typeToExpr]


jsonSchemaSyntaxNs :: ModuleName
jsonSchemaSyntaxNs = ModuleName "hydra.json.schema"

jsonWriterNs :: ModuleName
jsonWriterNs = ModuleName "hydra.json.writer"

-- String constant keys

additionalItemsToExpr :: TypedTermDefinition (JS.AdditionalItems -> J.Value)
additionalItemsToExpr = define "additionalItemsToExpr" $
  doc "Encode additional items as a JSON value" $
  lambda "ai" $
    cases JS._AdditionalItems (var "ai") Nothing [
      JS._AdditionalItems_any>>: lambda "b" $ Json.valueBoolean (var "b"),
      JS._AdditionalItems_schema>>: lambda "schema" $ schemaToExpr @@ var "schema"]

arrayRestrictionToExpr :: TypedTermDefinition (JS.ArrayRestriction -> (String, J.Value))
arrayRestrictionToExpr = define "arrayRestrictionToExpr" $
  doc "Encode an array restriction as a key-value pair" $
  lambda "r" $
    cases JS._ArrayRestriction (var "r") Nothing [
      JS._ArrayRestriction_items>>: lambda "items" $ itemsToExpr @@ var "items",
      JS._ArrayRestriction_additionalItems>>: lambda "ai" $
        pair keyAdditionalItems (additionalItemsToExpr @@ var "ai"),
      JS._ArrayRestriction_minItems>>: lambda "n" $
        pair keyMinItems (integerToExpr @@ var "n"),
      JS._ArrayRestriction_maxItems>>: lambda "n" $
        pair keyMaxItems (integerToExpr @@ var "n"),
      JS._ArrayRestriction_uniqueItems>>: lambda "b" $
        pair keyUniqueItems (Json.valueBoolean (var "b"))]

fromObject :: TypedTermDefinition (J.Value -> M.Map String J.Value)
fromObject = define "fromObject" $
  doc "Extract a name-keyed map from a JSON object value (field order is dropped)" $
  lambda "v" $
    cases J._Value (var "v") Nothing [
      J._Value_object>>: lambda "mp" $ (Maps.fromList (var "mp") :: TypedTerm (M.Map String J.Value))]

integerToExpr :: TypedTermDefinition (Int -> J.Value)
integerToExpr = define "integerToExpr" $
  doc "Encode an integer as a JSON number value" $
  lambda "n" $
    Json.valueNumber (Literals.bigintToDecimal (Literals.int32ToBigint (var "n")))

itemsToExpr :: TypedTermDefinition (JS.Items -> (String, J.Value))
itemsToExpr = define "itemsToExpr" $
  doc "Encode items as a key-value pair" $
  lambda "items" $
    pair keyItems
      (cases JS._Items (var "items") Nothing [
        JS._Items_sameItems>>: lambda "schema" $ schemaToExpr @@ var "schema",
        JS._Items_varItems>>: lambda "schemas" $
          Json.valueArray (Lists.map (asTerm schemaToExpr) (var "schemas"))])

jsonSchemaDocumentToJsonValue :: TypedTermDefinition (JS.Document -> J.Value)
jsonSchemaDocumentToJsonValue = define "jsonSchemaDocumentToJsonValue" $
  doc "Convert a JSON Schema document to a JSON value" $
  lambda "doc" $ lets [
    "mid">: project JS._Document JS._Document_id @@ var "doc",
    "mdefs">: project JS._Document JS._Document_definitions @@ var "doc",
    "root">: project JS._Document JS._Document_root @@ var "doc",
    "schemaMap">: fromObject @@ (schemaToExpr @@ var "root"),
    "restMap">: fromObject @@ (toObject @@ list [
      pair keyId (Optionals.map (lambda "i" $ Json.valueString (var "i")) (var "mid")),
      pair keySchema (Optionals.pure (Json.valueString (string "http://json-schema.org/2020-12/schema"))),
      pair keyDefinitions (Optionals.map
        (lambda "mp" $ Json.valueObject
          (Lists.map
            (lambda "p" $ lets [
              "k">: Pairs.first (var "p"),
              "schema">: Pairs.second (var "p")] $
              pair (unwrap JS._Keyword @@ var "k") (schemaToExpr @@ var "schema"))
            (Maps.toList (var "mp" :: TypedTerm (M.Map Term Term)))))
        (var "mdefs"))])] $
    Json.valueObject (Maps.toList (Maps.union (var "schemaMap") (var "restMap")))

jsonSchemaDocumentToString :: TypedTermDefinition (JS.Document -> String)
jsonSchemaDocumentToString = define "jsonSchemaDocumentToString" $
  doc "Convert a JSON Schema document to a JSON string" $
  lambda "doc" $
    var "hydra.json.writer.printJson" @@ (jsonSchemaDocumentToJsonValue @@ var "doc")

keyAdditionalItems :: TypedTermDefinition String
keyAdditionalItems = define "keyAdditionalItems" $
  doc "The JSON Schema \"additionalItems\" keyword" $
  string "additionalItems"

keyAdditionalProperties :: TypedTermDefinition String
keyAdditionalProperties = define "keyAdditionalProperties" $
  doc "The JSON Schema \"additionalProperties\" keyword" $
  string "additionalProperties"

keyAllOf :: TypedTermDefinition String
keyAllOf = define "keyAllOf" $
  doc "The JSON Schema \"allOf\" keyword" $
  string "allOf"

keyAnyOf :: TypedTermDefinition String
keyAnyOf = define "keyAnyOf" $
  doc "The JSON Schema \"anyOf\" keyword" $
  string "anyOf"

keyDefinitions :: TypedTermDefinition String
keyDefinitions = define "keyDefinitions" $
  doc "The JSON Schema \"$defs\" keyword, used for reusable schema definitions" $
  string "$defs"

keyDependencies :: TypedTermDefinition String
keyDependencies = define "keyDependencies" $
  doc "The JSON Schema \"dependencies\" keyword" $
  string "dependencies"

keyDescription_ :: TypedTermDefinition String
keyDescription_ = define "keyDescription" $
  doc "The JSON Schema \"description\" keyword" $
  string "description"

keyEnum :: TypedTermDefinition String
keyEnum = define "keyEnum" $
  doc "The JSON Schema \"enum\" keyword" $
  string "enum"

keyExclusiveMaximum :: TypedTermDefinition String
keyExclusiveMaximum = define "keyExclusiveMaximum" $
  doc "The JSON Schema \"exclusiveMaximum\" keyword" $
  string "exclusiveMaximum"

keyExclusiveMinimum :: TypedTermDefinition String
keyExclusiveMinimum = define "keyExclusiveMinimum" $
  doc "The JSON Schema \"exclusiveMinimum\" keyword" $
  string "exclusiveMinimum"

keyId :: TypedTermDefinition String
keyId = define "keyId" $
  doc "The JSON Schema \"$id\" keyword, identifying the schema" $
  string "$id"

keyItems :: TypedTermDefinition String
keyItems = define "keyItems" $
  doc "The JSON Schema \"items\" keyword" $
  string "items"

keyLabel :: TypedTermDefinition String
keyLabel = define "keyLabel" $
  doc "The JSON Schema \"label\" keyword" $
  string "label"

keyMaxItems :: TypedTermDefinition String
keyMaxItems = define "keyMaxItems" $
  doc "The JSON Schema \"maxItems\" keyword" $
  string "maxItems"

keyMaxLength_ :: TypedTermDefinition String
keyMaxLength_ = define "keyMaxLength" $
  doc "The JSON Schema \"maxLength\" keyword" $
  string "maxLength"

keyMaxProperties :: TypedTermDefinition String
keyMaxProperties = define "keyMaxProperties" $
  doc "The JSON Schema \"maxProperties\" keyword" $
  string "maxProperties"

keyMaximum :: TypedTermDefinition String
keyMaximum = define "keyMaximum" $
  doc "The JSON Schema \"maximum\" keyword" $
  string "maximum"

keyMinItems :: TypedTermDefinition String
keyMinItems = define "keyMinItems" $
  doc "The JSON Schema \"minItems\" keyword" $
  string "minItems"

keyMinLength_ :: TypedTermDefinition String
keyMinLength_ = define "keyMinLength" $
  doc "The JSON Schema \"minLength\" keyword" $
  string "minLength"

keyMinProperties :: TypedTermDefinition String
keyMinProperties = define "keyMinProperties" $
  doc "The JSON Schema \"minProperties\" keyword" $
  string "minProperties"

keyMinimum :: TypedTermDefinition String
keyMinimum = define "keyMinimum" $
  doc "The JSON Schema \"minimum\" keyword" $
  string "minimum"

keyMultipleOf :: TypedTermDefinition String
keyMultipleOf = define "keyMultipleOf" $
  doc "The JSON Schema \"multipleOf\" keyword" $
  string "multipleOf"

keyNot :: TypedTermDefinition String
keyNot = define "keyNot" $
  doc "The JSON Schema \"not\" keyword" $
  string "not"

keyOneOf :: TypedTermDefinition String
keyOneOf = define "keyOneOf" $
  doc "The JSON Schema \"oneOf\" keyword" $
  string "oneOf"

keyPattern :: TypedTermDefinition String
keyPattern = define "keyPattern" $
  doc "The JSON Schema \"pattern\" keyword" $
  string "pattern"

keyPatternProperties :: TypedTermDefinition String
keyPatternProperties = define "keyPatternProperties" $
  doc "The JSON Schema \"patternProperties\" keyword" $
  string "patternProperties"

keyProperties :: TypedTermDefinition String
keyProperties = define "keyProperties" $
  doc "The JSON Schema \"properties\" keyword" $
  string "properties"

keyRef :: TypedTermDefinition String
keyRef = define "keyRef" $
  doc "The JSON Schema \"$ref\" keyword, used for schema references" $
  string "$ref"

keyRequired :: TypedTermDefinition String
keyRequired = define "keyRequired" $
  doc "The JSON Schema \"required\" keyword" $
  string "required"

keySchema :: TypedTermDefinition String
keySchema = define "keySchema" $
  doc "The JSON Schema \"$schema\" keyword, identifying the schema dialect" $
  string "$schema"

keyTitle :: TypedTermDefinition String
keyTitle = define "keyTitle" $
  doc "The JSON Schema \"title\" keyword" $
  string "title"

keyType_ :: TypedTermDefinition String
keyType_ = define "keyType" $
  doc "The JSON Schema \"type\" keyword" $
  string "type"

keyUniqueItems :: TypedTermDefinition String
keyUniqueItems = define "keyUniqueItems" $
  doc "The JSON Schema \"uniqueItems\" keyword" $
  string "uniqueItems"


keywordSchemaOrArrayToExpr :: TypedTermDefinition ((JS.Keyword, JS.SchemaOrArray) -> (String, J.Value))
keywordSchemaOrArrayToExpr = define "keywordSchemaOrArrayToExpr" $
  doc "Encode a keyword-schema-or-array pair as a key-value pair" $
  lambda "p" $ lets [
    "k">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._Keyword @@ var "k") (schemaOrArrayToExpr @@ var "s")

keywordToExpr :: TypedTermDefinition (JS.Keyword -> J.Value)
keywordToExpr = define "keywordToExpr" $
  doc "Encode a keyword as a JSON string value" $
  lambda "k" $
    Json.valueString (unwrap JS._Keyword @@ var "k")

multipleRestrictionToExpr :: TypedTermDefinition (JS.MultipleRestriction -> (String, J.Value))
multipleRestrictionToExpr = define "multipleRestrictionToExpr" $
  doc "Encode a multiple restriction as a key-value pair" $
  lambda "r" $
    cases JS._MultipleRestriction (var "r") Nothing [
      JS._MultipleRestriction_allOf>>: lambda "schemas" $
        pair keyAllOf (Json.valueArray (Lists.map (asTerm schemaToExpr) (var "schemas"))),
      JS._MultipleRestriction_anyOf>>: lambda "schemas" $
        pair keyAnyOf (Json.valueArray (Lists.map (asTerm schemaToExpr) (var "schemas"))),
      JS._MultipleRestriction_oneOf>>: lambda "schemas" $
        pair keyOneOf (Json.valueArray (Lists.map (asTerm schemaToExpr) (var "schemas"))),
      JS._MultipleRestriction_not>>: lambda "schema" $
        pair keyNot (schemaToExpr @@ var "schema"),
      JS._MultipleRestriction_enum>>: lambda "values" $
        pair keyEnum (Json.valueArray (var "values"))]

numericRestrictionToExpr :: TypedTermDefinition (JS.NumericRestriction -> [(String, J.Value)])
numericRestrictionToExpr = define "numericRestrictionToExpr" $
  doc "Encode a numeric restriction as a list of key-value pairs" $
  lambda "r" $
    cases JS._NumericRestriction (var "r") Nothing [
      JS._NumericRestriction_minimum>>: lambda "lim" $ lets [
        "value">: project JS._Limit JS._Limit_value @@ var "lim",
        "excl">: project JS._Limit JS._Limit_exclusive @@ var "lim"] $
        Lists.concat $ list [
          list [pair keyMinimum (integerToExpr @@ var "value")],
          Logic.ifElse (var "excl")
            (list [pair keyExclusiveMinimum (Json.valueBoolean true)])
            (list ([] :: [TypedTerm (String, J.Value)]))],
      JS._NumericRestriction_maximum>>: lambda "lim" $ lets [
        "value">: project JS._Limit JS._Limit_value @@ var "lim",
        "excl">: project JS._Limit JS._Limit_exclusive @@ var "lim"] $
        Lists.concat $ list [
          list [pair keyMaximum (integerToExpr @@ var "value")],
          Logic.ifElse (var "excl")
            (list [pair keyExclusiveMaximum (Json.valueBoolean true)])
            (list ([] :: [TypedTerm (String, J.Value)]))],
      JS._NumericRestriction_multipleOf>>: lambda "n" $
        list [pair keyMultipleOf (integerToExpr @@ var "n")]]

objectRestrictionToExpr :: TypedTermDefinition (JS.ObjectRestriction -> (String, J.Value))
objectRestrictionToExpr = define "objectRestrictionToExpr" $
  doc "Encode an object restriction as a key-value pair" $
  lambda "r" $
    cases JS._ObjectRestriction (var "r") Nothing [
      JS._ObjectRestriction_properties>>: lambda "props" $
        pair keyProperties
          (Json.valueObject (Lists.map (asTerm propertyToExpr) (Maps.toList (var "props")))),
      JS._ObjectRestriction_additionalProperties>>: lambda "ai" $
        pair keyAdditionalProperties (additionalItemsToExpr @@ var "ai"),
      JS._ObjectRestriction_required>>: lambda "keys" $
        pair keyRequired (Json.valueArray (Lists.map (asTerm keywordToExpr) (var "keys"))),
      JS._ObjectRestriction_minProperties>>: lambda "n" $
        pair keyMinProperties (integerToExpr @@ var "n"),
      JS._ObjectRestriction_maxProperties>>: lambda "n" $
        pair keyMaxProperties (integerToExpr @@ var "n"),
      JS._ObjectRestriction_dependencies>>: lambda "deps" $
        pair keyDependencies
          (Json.valueObject (Lists.map (asTerm keywordSchemaOrArrayToExpr) (Maps.toList (var "deps")))),
      JS._ObjectRestriction_patternProperties>>: lambda "props" $
        pair keyPatternProperties
          (Json.valueObject (Lists.map (asTerm patternPropertyToExpr) (Maps.toList (var "props"))))]

patternPropertyToExpr :: TypedTermDefinition ((JS.RegularExpression, JS.Schema) -> (String, J.Value))
patternPropertyToExpr = define "patternPropertyToExpr" $
  doc "Encode a pattern property pair as a key-value pair" $
  lambda "p" $ lets [
    "pat">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._RegularExpression @@ var "pat") (schemaToExpr @@ var "s")

propertyToExpr :: TypedTermDefinition ((JS.Keyword, JS.Schema) -> (String, J.Value))
propertyToExpr = define "propertyToExpr" $
  doc "Encode a property pair as a key-value pair" $
  lambda "p" $ lets [
    "k">: Pairs.first (var "p"),
    "s">: Pairs.second (var "p")] $
    pair (unwrap JS._Keyword @@ var "k") (schemaToExpr @@ var "s")

restrictionToExpr :: TypedTermDefinition (JS.Restriction -> [(String, J.Value)])
restrictionToExpr = define "restrictionToExpr" $
  doc "Encode a restriction as a list of key-value pairs" $
  lambda "r" $
    cases JS._Restriction (var "r") Nothing [
      JS._Restriction_type>>: lambda "t" $
        list [pair keyType_ (typeToExpr @@ var "t")],
      JS._Restriction_string>>: lambda "sr" $
        list [stringRestrictionToExpr @@ var "sr"],
      JS._Restriction_number>>: lambda "nr" $
        numericRestrictionToExpr @@ var "nr",
      JS._Restriction_array>>: lambda "ar" $
        list [arrayRestrictionToExpr @@ var "ar"],
      JS._Restriction_object>>: lambda "or" $
        list [objectRestrictionToExpr @@ var "or"],
      JS._Restriction_multiple>>: lambda "mr" $
        list [multipleRestrictionToExpr @@ var "mr"],
      JS._Restriction_reference>>: lambda "sr" $
        list [pair keyRef (schemaReferenceToExpr @@ var "sr")],
      JS._Restriction_title>>: lambda "s" $
        list [pair keyTitle (Json.valueString (var "s"))],
      JS._Restriction_description>>: lambda "s" $
        list [pair keyDescription_ (Json.valueString (var "s"))]]

schemaOrArrayToExpr :: TypedTermDefinition (JS.SchemaOrArray -> J.Value)
schemaOrArrayToExpr = define "schemaOrArrayToExpr" $
  doc "Encode a schema or array as a JSON value" $
  lambda "soa" $
    cases JS._SchemaOrArray (var "soa") Nothing [
      JS._SchemaOrArray_schema>>: lambda "s" $ schemaToExpr @@ var "s",
      JS._SchemaOrArray_array>>: lambda "keys" $
        Json.valueArray (Lists.map (asTerm keywordToExpr) (var "keys"))]

schemaReferenceToExpr :: TypedTermDefinition (JS.SchemaReference -> J.Value)
schemaReferenceToExpr = define "schemaReferenceToExpr" $
  doc "Encode a schema reference as a JSON string value" $
  lambda "sr" $
    Json.valueString (unwrap JS._SchemaReference @@ var "sr")

schemaToExpr :: TypedTermDefinition (JS.Schema -> J.Value)
schemaToExpr = define "schemaToExpr" $
  doc "Encode a schema as a JSON object value" $
  lambda "s" $
    Json.valueObject (Lists.concat (Lists.map (asTerm restrictionToExpr) (unwrap JS._Schema @@ var "s")))

stringRestrictionToExpr :: TypedTermDefinition (JS.StringRestriction -> (String, J.Value))
stringRestrictionToExpr = define "stringRestrictionToExpr" $
  doc "Encode a string restriction as a key-value pair" $
  lambda "r" $
    cases JS._StringRestriction (var "r") Nothing [
      JS._StringRestriction_maxLength>>: lambda "n" $
        pair keyMaxLength_ (Json.valueNumber (Literals.bigintToDecimal (Literals.int32ToBigint (var "n")))),
      JS._StringRestriction_minLength>>: lambda "n" $
        pair keyMinLength_ (Json.valueNumber (Literals.bigintToDecimal (Literals.int32ToBigint (var "n")))),
      JS._StringRestriction_pattern>>: lambda "re" $
        pair keyPattern (Json.valueString (unwrap JS._RegularExpression @@ var "re"))]

typeNameToExpr :: TypedTermDefinition (JS.TypeName -> J.Value)
typeNameToExpr = define "typeNameToExpr" $
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

typeToExpr :: TypedTermDefinition (JS.Type -> J.Value)
typeToExpr = define "typeToExpr" $
  doc "Encode a type as a JSON value" $
  lambda "t" $
    cases JS._Type (var "t") Nothing [
      JS._Type_single>>: lambda "name" $ typeNameToExpr @@ var "name",
      JS._Type_multiple>>: lambda "names" $
        Json.valueArray (Lists.map (asTerm typeNameToExpr) (var "names"))]

-- Encoding functions

toObject :: TypedTermDefinition ([(String, Maybe J.Value)] -> J.Value)
toObject = define "toObject" $
  doc "Construct a JSON object from a list of optional key-value pairs, filtering out Nothing values" $
  lambda "pairs" $
    Json.valueObject
      (Optionals.cat (Lists.map
        (lambda "p" $ lets [
          "k">: Pairs.first (var "p"),
          "mv">: Pairs.second (var "p")] $
          Optionals.map
            (lambda "v" $ pair (var "k") (var "v"))
            (var "mv"))
        (var "pairs")))
