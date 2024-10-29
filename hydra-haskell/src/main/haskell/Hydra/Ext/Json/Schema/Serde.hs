module Hydra.Ext.Json.Schema.Serde(
  jsonSchemaDocumentToJsonValue,
  jsonSchemaDocumentToString
) where

import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json as J
import Hydra.Ext.Json.Serde

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


key_definitions = "definitions"
key_description = "description"
key_exclusiveMaximum = "exclusiveMaximum"
key_exclusiveMinimum = "exclusiveMinimum"
key_id = "$id"
key_items = "items"
key_label = "label"
key_maxLength = "maxLength"
key_maximum = "maximum"
key_minimum = "minimum"
key_minLength = "minLength"
key_multipleOf = "multipleOf"
key_pattern = "pattern"
key_type = "type"

encodeArrayRestriction :: JS.ArrayRestriction -> (String, J.Value)
encodeArrayRestriction r = case r of
  JS.ArrayRestrictionItems items -> encodeItems items
--  JS.ArrayRestrictionAdditionalItems ai -> (key_additionalItems, encodeAdditionalItems ai)
--  JS.ArrayRestrictionMinItems n -> (key_minItems, encodeInteger n)
--  JS.ArrayRestrictionMaxItems n -> (key_maxItems, encodeInteger n)
--  JS.ArrayRestrictionUniqueItems b -> (key_uniqueItems, J.ValueBoolean b)

encodeInteger :: Int -> J.Value
encodeInteger = J.ValueNumber . fromIntegral

encodeItems :: JS.Items -> (String, J.Value)
encodeItems items = (key_items, case items of
  JS.ItemsSameItems schema -> encodeSchema schema
  JS.ItemsVarItems schemas -> J.ValueArray $ fmap encodeSchema schemas)

encodeNumericRestriction :: JS.NumericRestriction -> [(String, J.Value)]
encodeNumericRestriction r = case r of
  JS.NumericRestrictionMinimum (JS.Limit value excl) -> [(key_minimum, encodeInteger value)]
    ++ if excl then [(key_exclusiveMinimum, J.ValueBoolean True)] else []
  JS.NumericRestrictionMaximum (JS.Limit value excl) -> [(key_maximum, encodeInteger value)]
    ++ if excl then [(key_exclusiveMaximum, J.ValueBoolean True)] else []
  JS.NumericRestrictionMultipleOf n -> [(key_multipleOf, encodeInteger n)]

encodeRestriction :: JS.Restriction -> [(String, J.Value)]
encodeRestriction r = case r of
    JS.RestrictionType t -> [(key_type, encodeType t)]
    JS.RestrictionString sr -> [encodeStringRestriction sr]
    JS.RestrictionNumber nr -> encodeNumericRestriction nr
    JS.RestrictionArray ar -> [encodeArrayRestriction ar]
--    JS.RestrictionObject or -> (key_object, encodeObjectRestriction or)
--    JS.RestrictionMultiple mr -> (key_multiple, encodeMultipleRestriction mr)
--    JS.RestrictionReference rr -> (key_reference, encodeSchemaReference rr)
--    JS.RestrictionTitle s -> (key_title, J.ValueString s)
--    JS.RestrictionDescription s -> (key_description, J.ValueString s)

encodeStringRestriction :: JS.StringRestriction -> (String, J.Value)
encodeStringRestriction r = case r of
    JS.StringRestrictionMaxLength n -> (key_maxLength, J.ValueNumber $ fromIntegral n)
    JS.StringRestrictionMinLength n -> (key_minLength, J.ValueNumber $ fromIntegral n)
    JS.StringRestrictionPattern (JS.RegularExpression s) -> (key_pattern, J.ValueString s)

{-
  RestrictionType Type |
  RestrictionString StringRestriction |
  RestrictionNumber NumericRestriction |
  RestrictionArray ArrayRestriction |
  RestrictionObject ObjectRestriction |
  RestrictionMultiple MultipleRestriction |
  RestrictionReference SchemaReference |
  RestrictionTitle String |
  RestrictionDescription String
-}

encodeSchema :: JS.Schema -> J.Value
encodeSchema (JS.Schema rs) = J.ValueObject $ M.fromList $ L.concat $ L.map encodeRestriction rs

encodeType :: JS.Type -> J.Value
encodeType (JS.Type names) = J.ValueArray $ L.map encodeTypeName names

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
      (key_definitions, encodeDefs <$> mdefs)]
    encodeDef (JS.Keyword k, schema) = (k, encodeSchema schema)
    encodeDefs mp = J.ValueObject $ M.fromList $ L.map encodeDef $ M.toList mp

jsonSchemaDocumentToString :: JS.Document -> String
jsonSchemaDocumentToString = jsonValueToString . jsonSchemaDocumentToJsonValue

fromObject :: J.Value -> M.Map String J.Value
fromObject (J.ValueObject mp) = mp

toObject :: [(String, Maybe J.Value)] -> J.Value
toObject = J.ValueObject . M.fromList . Y.catMaybes . L.map helper
  where
    helper (k, mv) = case mv of
      Just v -> Just (k, v)
      Nothing -> Nothing