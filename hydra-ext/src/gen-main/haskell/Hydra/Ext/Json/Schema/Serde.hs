-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting JSON Schema documents to JSON values

module Hydra.Ext.Json.Schema.Serde where

import qualified Hydra.Ext.Org.Json.Schema as Schema
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

key_additionalItems :: String
key_additionalItems = "additionalItems"

key_additionalProperties :: String
key_additionalProperties = "additionalProperties"

key_allOf :: String
key_allOf = "allOf"

key_anyOf :: String
key_anyOf = "anyOf"

key_definitions :: String
key_definitions = "$defs"

key_dependencies :: String
key_dependencies = "dependencies"

key_description :: String
key_description = "description"

key_enum :: String
key_enum = "enum"

key_exclusiveMaximum :: String
key_exclusiveMaximum = "exclusiveMaximum"

key_exclusiveMinimum :: String
key_exclusiveMinimum = "exclusiveMinimum"

key_id :: String
key_id = "$id"

key_items :: String
key_items = "items"

key_label :: String
key_label = "label"

key_maxItems :: String
key_maxItems = "maxItems"

key_maxLength :: String
key_maxLength = "maxLength"

key_maxProperties :: String
key_maxProperties = "maxProperties"

key_maximum :: String
key_maximum = "maximum"

key_minItems :: String
key_minItems = "minItems"

key_minimum :: String
key_minimum = "minimum"

key_minLength :: String
key_minLength = "minLength"

key_minProperties :: String
key_minProperties = "minProperties"

key_multipleOf :: String
key_multipleOf = "multipleOf"

key_not :: String
key_not = "not"

key_oneOf :: String
key_oneOf = "oneOf"

key_pattern :: String
key_pattern = "pattern"

key_patternProperties :: String
key_patternProperties = "patternProperties"

key_properties :: String
key_properties = "properties"

key_ref :: String
key_ref = "$ref"

key_required :: String
key_required = "required"

key_schema :: String
key_schema = "$schema"

key_title :: String
key_title = "title"

key_type :: String
key_type = "type"

key_uniqueItems :: String
key_uniqueItems = "uniqueItems"

-- | Encode additional items as a JSON value
encodeAdditionalItems :: (Schema.AdditionalItems -> Model.Value)
encodeAdditionalItems ai = ((\x -> case x of
  Schema.AdditionalItemsAny v0 -> (Model.ValueBoolean v0)
  Schema.AdditionalItemsSchema v0 -> (encodeSchema v0)) ai)

-- | Encode an array restriction as a key-value pair
encodeArrayRestriction :: (Schema.ArrayRestriction -> (String, Model.Value))
encodeArrayRestriction r = ((\x -> case x of
  Schema.ArrayRestrictionItems v0 -> (encodeItems v0)
  Schema.ArrayRestrictionAdditionalItems v0 -> (key_additionalItems, (encodeAdditionalItems v0))
  Schema.ArrayRestrictionMinItems v0 -> (key_minItems, (encodeInteger v0))
  Schema.ArrayRestrictionMaxItems v0 -> (key_maxItems, (encodeInteger v0))
  Schema.ArrayRestrictionUniqueItems v0 -> (key_uniqueItems, (Model.ValueBoolean v0))) r)

-- | Encode an integer as a JSON number value
encodeInteger :: (Int -> Model.Value)
encodeInteger n = (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint n)))

-- | Encode items as a key-value pair
encodeItems :: (Schema.Items -> (String, Model.Value))
encodeItems items = (key_items, ((\x -> case x of
  Schema.ItemsSameItems v0 -> (encodeSchema v0)
  Schema.ItemsVarItems v0 -> (Model.ValueArray (Lists.map encodeSchema v0))) items))

-- | Encode a keyword as a JSON string value
encodeKeyword :: (Schema.Keyword -> Model.Value)
encodeKeyword k = (Model.ValueString (Schema.unKeyword k))

-- | Encode a keyword-schema-or-array pair as a key-value pair
encodeKeywordSchemaOrArray :: ((Schema.Keyword, Schema.SchemaOrArray) -> (String, Model.Value))
encodeKeywordSchemaOrArray p =  
  let k = (Pairs.first p) 
      s = (Pairs.second p)
  in (Schema.unKeyword k, (encodeSchemaOrArray s))

-- | Encode a multiple restriction as a key-value pair
encodeMultipleRestriction :: (Schema.MultipleRestriction -> (String, Model.Value))
encodeMultipleRestriction r = ((\x -> case x of
  Schema.MultipleRestrictionAllOf v0 -> (key_allOf, (Model.ValueArray (Lists.map encodeSchema v0)))
  Schema.MultipleRestrictionAnyOf v0 -> (key_anyOf, (Model.ValueArray (Lists.map encodeSchema v0)))
  Schema.MultipleRestrictionOneOf v0 -> (key_oneOf, (Model.ValueArray (Lists.map encodeSchema v0)))
  Schema.MultipleRestrictionNot v0 -> (key_not, (encodeSchema v0))
  Schema.MultipleRestrictionEnum v0 -> (key_enum, (Model.ValueArray v0))) r)

-- | Encode a numeric restriction as a list of key-value pairs
encodeNumericRestriction :: (Schema.NumericRestriction -> [(String, Model.Value)])
encodeNumericRestriction r = ((\x -> case x of
  Schema.NumericRestrictionMinimum v0 ->  
    let value = (Schema.limitValue v0) 
        excl = (Schema.limitExclusive v0)
    in (Lists.concat [
      [
        (key_minimum, (encodeInteger value))],
      (Logic.ifElse excl [
        (key_exclusiveMinimum, (Model.ValueBoolean True))] [])])
  Schema.NumericRestrictionMaximum v0 ->  
    let value = (Schema.limitValue v0) 
        excl = (Schema.limitExclusive v0)
    in (Lists.concat [
      [
        (key_maximum, (encodeInteger value))],
      (Logic.ifElse excl [
        (key_exclusiveMaximum, (Model.ValueBoolean True))] [])])
  Schema.NumericRestrictionMultipleOf v0 -> [
    (key_multipleOf, (encodeInteger v0))]) r)

-- | Encode a pattern property pair as a key-value pair
encodePatternProperty :: ((Schema.RegularExpression, Schema.Schema) -> (String, Model.Value))
encodePatternProperty p =  
  let pat = (Pairs.first p) 
      s = (Pairs.second p)
  in (Schema.unRegularExpression pat, (encodeSchema s))

-- | Encode a property pair as a key-value pair
encodeProperty :: ((Schema.Keyword, Schema.Schema) -> (String, Model.Value))
encodeProperty p =  
  let k = (Pairs.first p) 
      s = (Pairs.second p)
  in (Schema.unKeyword k, (encodeSchema s))

-- | Encode an object restriction as a key-value pair
encodeObjectRestriction :: (Schema.ObjectRestriction -> (String, Model.Value))
encodeObjectRestriction r = ((\x -> case x of
  Schema.ObjectRestrictionProperties v0 -> (key_properties, (Model.ValueObject (Maps.fromList (Lists.map encodeProperty (Maps.toList v0)))))
  Schema.ObjectRestrictionAdditionalProperties v0 -> (key_additionalProperties, (encodeAdditionalItems v0))
  Schema.ObjectRestrictionRequired v0 -> (key_required, (Model.ValueArray (Lists.map encodeKeyword v0)))
  Schema.ObjectRestrictionMinProperties v0 -> (key_minProperties, (encodeInteger v0))
  Schema.ObjectRestrictionMaxProperties v0 -> (key_maxProperties, (encodeInteger v0))
  Schema.ObjectRestrictionDependencies v0 -> (key_dependencies, (Model.ValueObject (Maps.fromList (Lists.map encodeKeywordSchemaOrArray (Maps.toList v0)))))
  Schema.ObjectRestrictionPatternProperties v0 -> (key_patternProperties, (Model.ValueObject (Maps.fromList (Lists.map encodePatternProperty (Maps.toList v0)))))) r)

-- | Encode a restriction as a list of key-value pairs
encodeRestriction :: (Schema.Restriction -> [(String, Model.Value)])
encodeRestriction r = ((\x -> case x of
  Schema.RestrictionType v0 -> [
    (key_type, (encodeType v0))]
  Schema.RestrictionString v0 -> [
    encodeStringRestriction v0]
  Schema.RestrictionNumber v0 -> (encodeNumericRestriction v0)
  Schema.RestrictionArray v0 -> [
    encodeArrayRestriction v0]
  Schema.RestrictionObject v0 -> [
    encodeObjectRestriction v0]
  Schema.RestrictionMultiple v0 -> [
    encodeMultipleRestriction v0]
  Schema.RestrictionReference v0 -> [
    (key_ref, (encodeSchemaReference v0))]
  Schema.RestrictionTitle v0 -> [
    (key_title, (Model.ValueString v0))]
  Schema.RestrictionDescription v0 -> [
    (key_description, (Model.ValueString v0))]) r)

-- | Encode a string restriction as a key-value pair
encodeStringRestriction :: (Schema.StringRestriction -> (String, Model.Value))
encodeStringRestriction r = ((\x -> case x of
  Schema.StringRestrictionMaxLength v0 -> (key_maxLength, (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint v0))))
  Schema.StringRestrictionMinLength v0 -> (key_minLength, (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint v0))))
  Schema.StringRestrictionPattern v0 -> (key_pattern, (Model.ValueString (Schema.unRegularExpression v0)))) r)

-- | Encode a schema as a JSON object value
encodeSchema :: (Schema.Schema -> Model.Value)
encodeSchema s = (Model.ValueObject (Maps.fromList (Lists.concat (Lists.map encodeRestriction (Schema.unSchema s)))))

-- | Encode a schema or array as a JSON value
encodeSchemaOrArray :: (Schema.SchemaOrArray -> Model.Value)
encodeSchemaOrArray soa = ((\x -> case x of
  Schema.SchemaOrArraySchema v0 -> (encodeSchema v0)
  Schema.SchemaOrArrayArray v0 -> (Model.ValueArray (Lists.map encodeKeyword v0))) soa)

-- | Encode a schema reference as a JSON string value
encodeSchemaReference :: (Schema.SchemaReference -> Model.Value)
encodeSchemaReference sr = (Model.ValueString (Schema.unSchemaReference sr))

-- | Encode a type as a JSON value
encodeType :: (Schema.Type -> Model.Value)
encodeType t = ((\x -> case x of
  Schema.TypeSingle v0 -> (encodeTypeName v0)
  Schema.TypeMultiple v0 -> (Model.ValueArray (Lists.map encodeTypeName v0))) t)

-- | Encode a type name as a JSON string value
encodeTypeName :: (Schema.TypeName -> Model.Value)
encodeTypeName t = ((\x -> case x of
  Schema.TypeNameString -> (Model.ValueString "string")
  Schema.TypeNameInteger -> (Model.ValueString "integer")
  Schema.TypeNameNumber -> (Model.ValueString "number")
  Schema.TypeNameBoolean -> (Model.ValueString "boolean")
  Schema.TypeNameNull -> (Model.ValueString "null")
  Schema.TypeNameArray -> (Model.ValueString "array")
  Schema.TypeNameObject -> (Model.ValueString "object")) t)

-- | Convert a JSON Schema document to a JSON value
jsonSchemaDocumentToJsonValue :: (Schema.Document -> Model.Value)
jsonSchemaDocumentToJsonValue doc =  
  let mid = (Schema.documentId doc) 
      mdefs = (Schema.documentDefinitions doc)
      root = (Schema.documentRoot doc)
      schemaMap = (fromObject (encodeSchema root))
      restMap = (fromObject (toObject [
              (key_id, (Maybes.map (\i -> Model.ValueString i) mid)),
              (key_schema, (Maybes.pure (Model.ValueString "http://json-schema.org/2020-12/schema"))),
              (key_definitions, (Maybes.map (\mp -> Model.ValueObject (Maps.fromList (Lists.map (\p ->  
                let k = (Pairs.first p) 
                    schema = (Pairs.second p)
                in (Schema.unKeyword k, (encodeSchema schema))) (Maps.toList mp)))) mdefs))]))
  in (Model.ValueObject (Maps.union schemaMap restMap))

-- | Convert a JSON Schema document to a JSON string
jsonSchemaDocumentToString :: (Schema.Document -> String)
jsonSchemaDocumentToString doc = (Writer.printJson (jsonSchemaDocumentToJsonValue doc))

-- | Extract the map from a JSON object value
fromObject :: (Model.Value -> M.Map String Model.Value)
fromObject v = ((\x -> case x of
  Model.ValueObject v0 -> v0) v)

-- | Construct a JSON object from a list of optional key-value pairs, filtering out Nothing values
toObject :: ([(String, (Maybe Model.Value))] -> Model.Value)
toObject pairs = (Model.ValueObject (Maps.fromList (Maybes.cat (Lists.map (\p ->  
  let k = (Pairs.first p) 
      mv = (Pairs.second p)
  in (Maybes.map (\v -> (k, v)) mv)) pairs))))
