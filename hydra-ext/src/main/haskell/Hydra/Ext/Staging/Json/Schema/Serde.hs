module Hydra.Ext.Staging.Json.Schema.Serde(
  jsonSchemaDocumentToJsonValue,
  jsonSchemaDocumentToString
) where

import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json as J
import qualified Hydra.Staging.Json.Serde as JsonSerde

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


key_additionalItems = "additionalItems"
key_additionalProperties = "additionalProperties"
key_allOf = "allOf"
key_anyOf = "anyOf"
key_definitions = "$defs"
key_dependencies = "dependencies"
key_description = "description"
key_enum = "enum"
key_exclusiveMaximum = "exclusiveMaximum"
key_exclusiveMinimum = "exclusiveMinimum"
key_id = "$id"
key_items = "items"
key_label = "label"
key_maxItems = "maxItems"
key_maxLength = "maxLength"
key_maxProperties = "maxProperties"
key_maximum = "maximum"
key_minItems = "minItems"
key_minimum = "minimum"
key_minLength = "minLength"
key_minProperties = "minProperties"
key_multipleOf = "multipleOf"
key_not = "not"
key_oneOf = "oneOf"
key_pattern = "pattern"
key_patternProperties = "patternProperties"
key_properties = "properties"
key_ref = "$ref"
key_required = "required"
key_schema = "$schema"
key_title = "title"
key_type = "type"
key_uniqueItems = "uniqueItems"

encodeAdditionalItems :: JS.AdditionalItems -> J.Value
encodeAdditionalItems ai = case ai of
  JS.AdditionalItemsAny b -> J.ValueBoolean b
  JS.AdditionalItemsSchema schema -> encodeSchema schema

encodeArrayRestriction :: JS.ArrayRestriction -> (String, J.Value)
encodeArrayRestriction r = case r of
  JS.ArrayRestrictionItems items -> encodeItems items
  JS.ArrayRestrictionAdditionalItems ai -> (key_additionalItems, encodeAdditionalItems ai)
  JS.ArrayRestrictionMinItems n -> (key_minItems, encodeInteger n)
  JS.ArrayRestrictionMaxItems n -> (key_maxItems, encodeInteger n)
  JS.ArrayRestrictionUniqueItems b -> (key_uniqueItems, J.ValueBoolean b)

encodeInteger :: Int -> J.Value
encodeInteger = J.ValueNumber . fromIntegral

encodeItems :: JS.Items -> (String, J.Value)
encodeItems items = (key_items, case items of
  JS.ItemsSameItems schema -> encodeSchema schema
  JS.ItemsVarItems schemas -> J.ValueArray $ fmap encodeSchema schemas)

encodeKeyword :: JS.Keyword -> J.Value
encodeKeyword (JS.Keyword k) = J.ValueString k

encodeKeywordSchemaOrArray :: (JS.Keyword, JS.SchemaOrArray) -> (String, J.Value)
encodeKeywordSchemaOrArray (JS.Keyword k, s) = (k, encodeSchemaOrArray s)

encodeMultipleRestriction :: JS.MultipleRestriction -> (String, J.Value)
encodeMultipleRestriction r = case r of
  JS.MultipleRestrictionAllOf schemas -> (key_allOf, J.ValueArray $ fmap encodeSchema schemas)
  JS.MultipleRestrictionAnyOf schemas -> (key_anyOf, J.ValueArray $ fmap encodeSchema schemas)
  JS.MultipleRestrictionOneOf schemas -> (key_oneOf, J.ValueArray $ fmap encodeSchema schemas)
  JS.MultipleRestrictionNot schema -> (key_not, encodeSchema schema)
  JS.MultipleRestrictionEnum values -> (key_enum, J.ValueArray values)

encodeNumericRestriction :: JS.NumericRestriction -> [(String, J.Value)]
encodeNumericRestriction r = case r of
  JS.NumericRestrictionMinimum (JS.Limit value excl) -> [(key_minimum, encodeInteger value)]
    ++ if excl then [(key_exclusiveMinimum, J.ValueBoolean True)] else []
  JS.NumericRestrictionMaximum (JS.Limit value excl) -> [(key_maximum, encodeInteger value)]
    ++ if excl then [(key_exclusiveMaximum, J.ValueBoolean True)] else []
  JS.NumericRestrictionMultipleOf n -> [(key_multipleOf, encodeInteger n)]

encodePatternProperty :: (JS.RegularExpression, JS.Schema) -> (String, J.Value)
encodePatternProperty (JS.RegularExpression p, s) = (p, encodeSchema s)

encodeProperty :: (JS.Keyword, JS.Schema) -> (String, J.Value)
encodeProperty (JS.Keyword k, s) = (k, encodeSchema s)

encodeObjectRestriction :: JS.ObjectRestriction -> (String, J.Value)
encodeObjectRestriction r = case r of
  JS.ObjectRestrictionProperties props -> (key_properties, J.ValueObject $ M.fromList (encodeProperty <$> M.toList props))
  JS.ObjectRestrictionAdditionalProperties ai -> (key_additionalProperties, encodeAdditionalItems ai)
  JS.ObjectRestrictionRequired keys -> (key_required, J.ValueArray $ fmap encodeKeyword keys)
  JS.ObjectRestrictionMinProperties n -> (key_minProperties, encodeInteger n)
  JS.ObjectRestrictionMaxProperties n -> (key_maxProperties, encodeInteger n)
  JS.ObjectRestrictionDependencies deps -> (key_dependencies, J.ValueObject $ M.fromList (encodeKeywordSchemaOrArray <$> M.toList deps))
  JS.ObjectRestrictionPatternProperties props -> (key_patternProperties, J.ValueObject $ M.fromList (encodePatternProperty <$> M.toList props))

encodeRestriction :: JS.Restriction -> [(String, J.Value)]
encodeRestriction r = case r of
    JS.RestrictionType t -> [(key_type, encodeType t)]
    JS.RestrictionString sr -> [encodeStringRestriction sr]
    JS.RestrictionNumber nr -> encodeNumericRestriction nr
    JS.RestrictionArray ar -> [encodeArrayRestriction ar]
    JS.RestrictionObject or -> [encodeObjectRestriction or]
    JS.RestrictionMultiple mr -> [encodeMultipleRestriction mr]
    JS.RestrictionReference sr -> [(key_ref, encodeSchemaReference sr)]
    JS.RestrictionTitle s -> [(key_title, J.ValueString s)]
    JS.RestrictionDescription s -> [(key_description, J.ValueString s)]

encodeStringRestriction :: JS.StringRestriction -> (String, J.Value)
encodeStringRestriction r = case r of
    JS.StringRestrictionMaxLength n -> (key_maxLength, J.ValueNumber $ fromIntegral n)
    JS.StringRestrictionMinLength n -> (key_minLength, J.ValueNumber $ fromIntegral n)
    JS.StringRestrictionPattern (JS.RegularExpression s) -> (key_pattern, J.ValueString s)

encodeSchema :: JS.Schema -> J.Value
encodeSchema (JS.Schema rs) = J.ValueObject $ M.fromList $ L.concat $ L.map encodeRestriction rs

encodeSchemaOrArray :: JS.SchemaOrArray -> J.Value
encodeSchemaOrArray soa = case soa of
  JS.SchemaOrArraySchema s -> encodeSchema s
  JS.SchemaOrArrayArray keys -> J.ValueArray $ fmap encodeKeyword keys

encodeSchemaReference :: JS.SchemaReference -> J.Value
encodeSchemaReference (JS.SchemaReference s) = J.ValueString s

encodeType :: JS.Type -> J.Value
encodeType t = case t of
  JS.TypeSingle name -> encodeTypeName name
  JS.TypeMultiple names -> J.ValueArray $ L.map encodeTypeName names

encodeTypeName :: JS.TypeName -> J.Value
encodeTypeName t = case t of
  JS.TypeNameString -> J.ValueString "string"
  JS.TypeNameInteger -> J.ValueString "integer"
  JS.TypeNameNumber -> J.ValueString "number"
  JS.TypeNameBoolean -> J.ValueString "boolean"
  JS.TypeNameNull -> J.ValueString "null"
  JS.TypeNameArray -> J.ValueString "array"
  JS.TypeNameObject -> J.ValueString "object"

jsonSchemaDocumentToJsonValue :: JS.Document -> J.Value
jsonSchemaDocumentToJsonValue (JS.Document mid mdefs root) = J.ValueObject $ M.union schemaMap restMap
  where
    schemaMap = fromObject $ encodeSchema root
    restMap = fromObject $ toObject [
      (key_id, J.ValueString <$> mid),
      (key_schema, Just $ J.ValueString "http://json-schema.org/2020-12/schema"),
      (key_definitions, encodeDefs <$> mdefs)]
    encodeDef (JS.Keyword k, schema) = (k, encodeSchema schema)
    encodeDefs mp = J.ValueObject $ M.fromList $ L.map encodeDef $ M.toList mp

jsonSchemaDocumentToString :: JS.Document -> String
jsonSchemaDocumentToString = JsonSerde.jsonValueToString . jsonSchemaDocumentToJsonValue

fromObject :: J.Value -> M.Map String J.Value
fromObject (J.ValueObject mp) = mp

toObject :: [(String, Maybe J.Value)] -> J.Value
toObject = J.ValueObject . M.fromList . Y.catMaybes . L.map helper
  where
    helper (k, mv) = case mv of
      Just v -> Just (k, v)
      Nothing -> Nothing
