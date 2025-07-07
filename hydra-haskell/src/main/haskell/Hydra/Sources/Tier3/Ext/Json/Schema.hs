{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Json.Schema where

import Hydra.Kernel
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.All as Tier2
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


jsonSchemaModule :: Module
jsonSchemaModule = Module ns elements [KernelTypes.jsonModelModule] [KernelTypes.hydraCoreModule] $
    Just ("A model for JSON Schema. Based on https://cswr.github.io/JsonSchema/spec/grammar")
  where
    ns = Namespace "hydra.ext.org.json.schema"
    def = datatype ns
    js = typeref ns
    json = typeref $ moduleNamespace KernelTypes.jsonModelModule

    keywordSchemaMap = Types.map (js "Keyword") (js "Schema")
    keywordSchemaOrArrayMap = Types.map (js "Keyword") (js "SchemaOrArray")
    regexSchemaMap = Types.map (js "RegularExpression") (js "Schema")

    elements = [
-- Json Documents and Schemas
-- Let JDOC be an arbitrary JSON Schema Document. We can define its syntax using the following grammar:
--
-- JSDoc := { ( id, )? ( defs, )? JSch }

      def "Document" $ record [
        "id">: optional string,
        "definitions">: optional keywordSchemaMap,
        "root">: js "Schema"],

-- id := "id": "uri"
-- defs := "definitions": { kSch (, kSch)*}
-- kSch := kword: { JSch }

      def "Keyword" $
        wrap string,

-- JSch := ( res (, res)*)
      def "Schema" $
        wrap $ nonemptyList $ js "Restriction",

-- res := type | strRes | numRes | arrRes | objRes | multRes | refSch | title | description

      def "Restriction" $
        union [
          "type">: js "Type",
          "string">: js "StringRestriction",
          "number">: js "NumericRestriction",
          "array">: js "ArrayRestriction",
          "object">: js "ObjectRestriction",
          "multiple">: js "MultipleRestriction",
          "reference">: js "SchemaReference",
          "title">: string,
          "description">: string],

-- type := "type" : ([typename (, typename)*] | typename)

      def "Type" $
        union [
          "single">: js "TypeName",
          "multiple">: nonemptyList $ js "TypeName"],

-- typename := "string" | "integer" | "number" | "boolean" | "null" | "array" | "object"

      def "TypeName" $
        enum ["string", "integer", "number", "boolean", "null", "array", "object"],

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
        union [
          "minLength">: int32,
          "maxLength">: int32,
          "pattern">: js "RegularExpression"],

-- minLen := "minLength": n
-- maxLen := "maxLength": n
-- pattern := "pattern": "regExp"

      def "RegularExpression" $
        wrap string,

-- Here n is a natural number and r is a regular expression.
--
-- Numeric Restrictions
-- numRes := min | max | multiple

      def "NumericRestriction" $
        union [
          "minimum">: js "Limit",
          "maximum">: js "Limit",
          "multipleOf">: nonNegativeInt32],

     def "Limit" $
       record [
         "value">: int32,
         "exclusive">: boolean],

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
        union [
          "items">: js "Items",
          "additionalItems">: js "AdditionalItems",
          "minItems">: nonNegativeInt32,
          "maxItems">: nonNegativeInt32,
          "uniqueItems">: boolean],

--  items := ( sameitems |  varitems )

      def "Items" $
        union [
          "sameItems">: js "Schema",
          "varItems">: nonemptyList $ js "Schema"],

--  sameitems := "items": { JSch }
--  varitems := "items": [{ JSch }(,{ JSch })*]
--  additems :=  "additionalItems": (bool | { JSch })

      def "AdditionalItems" $
        union [
          "any">: boolean,
          "schema">: js "Schema"],

--  minitems := "minItems": n
--  maxitems := "maxItems": n
--  unique := "uniqueItems": bool
-- Here n is a natural number and bool is either true or false.
--
-- Object Restrictions
-- objRes := prop | addprop | req | minprop | maxprop | dep | pattprop

      def "ObjectRestriction" $
        union [
          "properties">: keywordSchemaMap,
          "additionalProperties">: js "AdditionalItems",
          "required">: nonemptyList $ js "Keyword",
          "minProperties">: nonNegativeInt32,
          "maxProperties">: nonNegativeInt32,
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
        union [
          "schema">: js "Schema",
          "array">: list $ js "Keyword"],

-- kArr := kword: [ kword (, kword)*]
-- pattprop := "patternProperties": { patSch (, patSch)*}
-- patSch := "regExp": { JSch }
-- Here n is a natural number, bool is either true or false and regExp is a regular expression. As above, each kword is representing a keyword that must be unique in the nest level that is occurs.
--
-- Multiple Restrictions
-- multRes := allOf | anyOf| oneOf | not | enum

      def "MultipleRestriction" $
        union [
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
            wrap string]

-- uriRef := ( address )? ( # / JPointer )?
-- JPointer := ( / path )
-- path := ( unescaped | escaped )
-- escaped := ~0 | ~1
-- Where unescaped can be any character except for / and ~. Also, address corresponds to any URI that does not use the # symbol, or more precisely to any URI-reference constructed using the following grammar, as defined in the official standard:
--
-- address = (scheme : )? hier-part (? query )



