module Hydra.Sources.Json.Schema where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Sources.Json.Model        as JsonModel


ns :: ModuleName
ns = ModuleName "hydra.json.schema"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [JsonModel.ns, Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A model for JSON Schema. Based on https://cswr.github.io/JsonSchema/spec/grammar"))}
  where
    def = datatype ns
    js = typeref ns
    json = typeref $ JsonModel.ns

    keywordSchemaMap = T.map (js "Keyword") (js "Schema")
    keywordSchemaOrArrayMap = T.map (js "Keyword") (js "SchemaOrArray")
    regexSchemaMap = T.map (js "RegularExpression") (js "Schema")

    -- Alphabetical order by local type name, per the definition-ordering style guide
    -- (Validate.Packaging.checkDefinitionOrdering has no section-boundary awareness).
    -- Grammar production comments (from https://cswr.github.io/JsonSchema/spec/grammar) are kept
    -- next to their definitions as documentation only; they do not reflect list order.
    definitions = [
--  additems :=  "additionalItems": (bool | { JSch })
      def "AdditionalItems" $
        doc "Whether additional array items beyond those explicitly typed are permitted, and if so, their schema" $
        T.union [
          "any">: T.boolean,
          "schema">: js "Schema"],

--  arrRes := items | additems | minitems | maxitems  | unique
      def "ArrayRestriction" $
        doc "A restriction on array-typed values: items, additionalItems, minItems, maxItems, or uniqueItems" $
        T.union [
          "items">: js "Items",
          "additionalItems">: js "AdditionalItems",
          "minItems">: T.nonNegativeInt32,
          "maxItems">: T.nonNegativeInt32,
          "uniqueItems">: T.boolean],

-- JSDoc := { ( id, )? ( defs, )? JSch }
      def "Document" $
        doc "A JSON Schema document: an optional id, optional local definitions, and a root schema" $
        T.record [
        "id">: T.optional T.string,
        "definitions">: T.optional keywordSchemaMap,
        "root">: js "Schema"],

--  items := ( sameitems |  varitems )
      def "Items" $
        doc "The schema(s) for array items: either a single schema shared by all items, or one schema per item" $
        T.union [
          "sameItems">: js "Schema",
          "varItems">: nonemptyList $ js "Schema"],

-- kSch := kword: { JSch }
      def "Keyword" $
        doc "A property name used as a key in a JSON Schema keyword-to-schema map" $
        T.wrap T.string,

-- min := "minimum": r (,exMin)?
      def "Limit" $
        doc "A numeric bound with a flag indicating whether the bound itself is excluded" $
       T.record [
         "value">: T.int32,
         "exclusive">: T.boolean],

-- multRes := allOf | anyOf| oneOf | not | enum
      def "MultipleRestriction" $
        doc "A restriction combining multiple schemas: allOf, anyOf, oneOf, not, or enum" $
        T.union [
          "allOf">: nonemptyList $ js "Schema",
          "anyOf">: nonemptyList $ js "Schema",
          "oneOf">: nonemptyList $ js "Schema",
          "not">: js "Schema",
          "enum">: nonemptyList $ json "Value"],

-- numRes := min | max | multiple
      def "NumericRestriction" $
        doc "A restriction on numeric-typed values: a minimum, a maximum, or a multipleOf constraint" $
        T.union [
          "minimum">: js "Limit",
          "maximum">: js "Limit",
          "multipleOf">: T.nonNegativeInt32],

-- objRes := prop | addprop | req | minprop | maxprop | dep | pattprop
      def "ObjectRestriction" $
        doc "A restriction on object-typed values: properties, additionalProperties, required, and related keywords" $
        T.union [
          "properties">: keywordSchemaMap,
          "additionalProperties">: js "AdditionalItems",
          "required">: nonemptyList $ js "Keyword",
          "minProperties">: T.nonNegativeInt32,
          "maxProperties">: T.nonNegativeInt32,
          "dependencies">: keywordSchemaOrArrayMap,
          "patternProperties">: regexSchemaMap],

-- pattern := "pattern": "regExp"
      def "RegularExpression" $
        doc "A regular expression pattern, as used by the pattern and patternProperties keywords" $
        T.wrap T.string,

-- res := type | strRes | numRes | arrRes | objRes | multRes | refSch | title | description
      def "Restriction" $
        doc "A single JSON Schema restriction: a type, value restriction, reference, title, or description" $
        T.union [
          "type">: js "Type",
          "string">: js "StringRestriction",
          "number">: js "NumericRestriction",
          "array">: js "ArrayRestriction",
          "object">: js "ObjectRestriction",
          "multiple">: js "MultipleRestriction",
          "reference">: js "SchemaReference",
          "title">: T.string,
          "description">: T.string],

-- JSch := ( res (, res)*)
      def "Schema" $
        doc "A JSON schema: a non-empty list of restrictions that a conforming value must satisfy" $
        T.wrap $ nonemptyList $ js "Restriction",

-- kDep := (kArr | kSch)
      def "SchemaOrArray" $
        doc "A dependency value: either a schema, or a list of required keyword names" $
        T.union [
          "schema">: js "Schema",
          "array">: T.list $ js "Keyword"],

-- refSch := "$ref": "uriRef"
      def "SchemaReference" $
        doc "A reference to another schema by URI" $
        T.wrap T.string,

-- strRes :=  minLen | maxLen | pattern
      def "StringRestriction" $
        doc "A restriction on string-typed values: minLength, maxLength, or pattern" $
        T.union [
          "minLength">: T.int32,
          "maxLength">: T.int32,
          "pattern">: js "RegularExpression"],

-- type := "type" : ([typename (, typename)*] | typename)
      def "Type" $
        doc "A type restriction: a single type name, or a list of alternative type names" $
        T.union [
          "single">: js "TypeName",
          "multiple">: nonemptyList $ js "TypeName"],

-- typename := "string" | "integer" | "number" | "boolean" | "null" | "array" | "object"
      def "TypeName" $
        doc "The name of a JSON Schema primitive type" $
        T.enum ["string", "integer", "number", "boolean", "null", "array", "object"]]

-- Here each res and typename must be different from each other(otherwise they would be superfluous).
-- We must also note that each kword is representing a keyword that must be unique in the nest level that is occurs.
-- Besides, string is any string to describe either the title or de description of the nested schema.
-- Finally, a uri is any possible uri as defined in the standard.
--
-- uriRef := ( address )? ( # / JPointer )?
-- JPointer := ( / path )
-- path := ( unescaped | escaped )
-- escaped := ~0 | ~1
-- Where unescaped can be any character except for / and ~. Also, address corresponds to any URI that does not use the # symbol, or more precisely to any URI-reference constructed using the following grammar, as defined in the official standard:
--
-- address = (scheme : )? hier-part (? query )


