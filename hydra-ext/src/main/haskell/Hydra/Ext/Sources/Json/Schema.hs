module Hydra.Ext.Sources.Json.Schema where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Classes     as Classes
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Variants    as Variants
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


module_ :: Module
module_ = Module ns elements [Json.ns] [Core.ns] $
    Just ("A model for JSON Schema. Based on https://cswr.github.io/JsonSchema/spec/grammar")
  where
    ns = Namespace "hydra.ext.org.json.schema"
    def = datatype ns
    js = typeref ns
    json = typeref $ Json.ns

    keywordSchemaMap = T.map (js "Keyword") (js "Schema")
    keywordSchemaOrArrayMap = T.map (js "Keyword") (js "SchemaOrArray")
    regexSchemaMap = T.map (js "RegularExpression") (js "Schema")

    elements = [
-- Json Documents and Schemas
-- Let JDOC be an arbitrary JSON Schema Document. We can define its syntax using the following grammar:
--
-- JSDoc := { ( id, )? ( defs, )? JSch }

      def "Document" $ T.record [
        "id">: T.maybe T.string,
        "definitions">: T.maybe keywordSchemaMap,
        "root">: js "Schema"],

-- id := "id": "uri"
-- defs := "definitions": { kSch (, kSch)*}
-- kSch := kword: { JSch }

      def "Keyword" $
        T.wrap T.string,

-- JSch := ( res (, res)*)
      def "Schema" $
        T.wrap $ nonemptyList $ js "Restriction",

-- res := type | strRes | numRes | arrRes | objRes | multRes | refSch | title | description

      def "Restriction" $
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

-- type := "type" : ([typename (, typename)*] | typename)

      def "Type" $
        T.union [
          "single">: js "TypeName",
          "multiple">: nonemptyList $ js "TypeName"],

-- typename := "string" | "integer" | "number" | "boolean" | "null" | "array" | "object"

      def "TypeName" $
        T.enum ["string", "integer", "number", "boolean", "null", "array", "object"],

-- title := "title":  string
-- description := "description":  string

-- Here each res and typename must be different from each other(otherwise they would be superfluous).
-- We must also note that each kword is representing a keyword that must be unique in the nest level that is occurs.
-- Besides, string is any string to describe either the title or de description of the nested schema.
-- Finally, a uri is any possible uri as defined in the standard. Next we specify the remaining restrictions:
-- strRes, numRes, arrRes, objRes and multRes, as well as referred schemas refSch.
--
-- String Restrictions
-- strRes :=  minLen | maxLen | pattern

      def "StringRestriction" $
        T.union [
          "minLength">: T.int32,
          "maxLength">: T.int32,
          "pattern">: js "RegularExpression"],

-- minLen := "minLength": n
-- maxLen := "maxLength": n
-- pattern := "pattern": "regExp"

      def "RegularExpression" $
        T.wrap T.string,

-- Here n is a natural number and r is a regular expression.
--
-- Numeric Restrictions
-- numRes := min | max | multiple

      def "NumericRestriction" $
        T.union [
          "minimum">: js "Limit",
          "maximum">: js "Limit",
          "multipleOf">: T.nonNegativeInt32],

     def "Limit" $
       T.record [
         "value">: T.int32,
         "exclusive">: T.boolean],

-- min := "minimum": r (,exMin)?
-- exMin := "exclusiveMinimum": bool
-- max := "maximum": r (,exMax)?
-- exMax := "exclusiveMaximum": bool
-- multiple := "multipleOf": r   (r >= 0)
-- Here r is a decimal number and bool is either true or false.
--
-- Array Restrictions
--  arrRes := items | additems | minitems | maxitems  | unique

      def "ArrayRestriction" $
        T.union [
          "items">: js "Items",
          "additionalItems">: js "AdditionalItems",
          "minItems">: T.nonNegativeInt32,
          "maxItems">: T.nonNegativeInt32,
          "uniqueItems">: T.boolean],

--  items := ( sameitems |  varitems )

      def "Items" $
        T.union [
          "sameItems">: js "Schema",
          "varItems">: nonemptyList $ js "Schema"],

--  sameitems := "items": { JSch }
--  varitems := "items": [{ JSch }(,{ JSch })*]
--  additems :=  "additionalItems": (bool | { JSch })

      def "AdditionalItems" $
        T.union [
          "any">: T.boolean,
          "schema">: js "Schema"],

--  minitems := "minItems": n
--  maxitems := "maxItems": n
--  unique := "uniqueItems": bool
-- Here n is a natural number and bool is either true or false.
--
-- Object Restrictions
-- objRes := prop | addprop | req | minprop | maxprop | dep | pattprop

      def "ObjectRestriction" $
        T.union [
          "properties">: keywordSchemaMap,
          "additionalProperties">: js "AdditionalItems",
          "required">: nonemptyList $ js "Keyword",
          "minProperties">: T.nonNegativeInt32,
          "maxProperties">: T.nonNegativeInt32,
          "dependencies">: keywordSchemaOrArrayMap,
          "patternProperties">: regexSchemaMap],

-- prop := "properties": { kSch (, kSch)*}
-- kSch := kword: { JSch }
-- addprop := "additionalProperties": (bool | { JSch })
-- req := "required": [ kword (, kword)*]
-- minprop := "minProperties": n
-- maxprop := "maxProperties": n
-- dep := "dependencies": { kDep (, kDep)*}
-- kDep := (kArr | kSch)

      def "SchemaOrArray" $
        T.union [
          "schema">: js "Schema",
          "array">: T.list $ js "Keyword"],

-- kArr := kword: [ kword (, kword)*]
-- pattprop := "patternProperties": { patSch (, patSch)*}
-- patSch := "regExp": { JSch }
-- Here n is a natural number, bool is either true or false and regExp is a regular expression. As above, each kword is representing a keyword that must be unique in the nest level that is occurs.
--
-- Multiple Restrictions
-- multRes := allOf | anyOf| oneOf | not | enum

      def "MultipleRestriction" $
        T.union [
          "allOf">: nonemptyList $ js "Schema",
          "anyOf">: nonemptyList $ js "Schema",
          "oneOf">: nonemptyList $ js "Schema",
          "not">: js "Schema",
          "enum">: nonemptyList $ json "Value"],

-- anyOf := "anyOf": [ { JSch } (, { JSch }) * ]
-- allOf := "allOf": [ { JSch } (, { JSch }) * ]
-- oneOf := "oneOf": [ { JSch } (, { JSch }) * ]
-- not := "not": { JSch }
-- enum := "enum": [Jval (, Jval)*]
-- Here Jval is either a string, number, array, object, bool or a null value. Moreover each Jval must be different from each other(otherwise they would be superfluous).

-- Referred Schemas
-- Note that uriRef below is the same grammar we defined earlier for URIs.
--
-- refSch := "$ref": "uriRef"

         def "SchemaReference" $
            T.wrap T.string]

-- uriRef := ( address )? ( # / JPointer )?
-- JPointer := ( / path )
-- path := ( unescaped | escaped )
-- escaped := ~0 | ~1
-- Where unescaped can be any character except for / and ~. Also, address corresponds to any URI that does not use the # symbol, or more precisely to any URI-reference constructed using the following grammar, as defined in the official standard:
--
-- address = (scheme : )? hier-part (? query )


