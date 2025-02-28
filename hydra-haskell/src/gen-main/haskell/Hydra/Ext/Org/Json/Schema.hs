-- | A model for JSON Schema. Based on https://cswr.github.io/JsonSchema/spec/grammar

module Hydra.Ext.Org.Json.Schema where

import qualified Hydra.Core as Core
import qualified Hydra.Json as Json
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data Document = 
  Document {
    documentId :: (Maybe String),
    documentDefinitions :: (Maybe (M.Map Keyword Schema)),
    documentRoot :: Schema}
  deriving (Eq, Ord, Read, Show)

_Document = (Core.Name "hydra.ext.org.json.schema.Document")

_Document_id = (Core.Name "id")

_Document_definitions = (Core.Name "definitions")

_Document_root = (Core.Name "root")

newtype Keyword = 
  Keyword {
    unKeyword :: String}
  deriving (Eq, Ord, Read, Show)

_Keyword = (Core.Name "hydra.ext.org.json.schema.Keyword")

newtype Schema = 
  Schema {
    unSchema :: [Restriction]}
  deriving (Eq, Ord, Read, Show)

_Schema = (Core.Name "hydra.ext.org.json.schema.Schema")

data Restriction = 
  RestrictionType Type |
  RestrictionString StringRestriction |
  RestrictionNumber NumericRestriction |
  RestrictionArray ArrayRestriction |
  RestrictionObject ObjectRestriction |
  RestrictionMultiple MultipleRestriction |
  RestrictionReference SchemaReference |
  RestrictionTitle String |
  RestrictionDescription String
  deriving (Eq, Ord, Read, Show)

_Restriction = (Core.Name "hydra.ext.org.json.schema.Restriction")

_Restriction_type = (Core.Name "type")

_Restriction_string = (Core.Name "string")

_Restriction_number = (Core.Name "number")

_Restriction_array = (Core.Name "array")

_Restriction_object = (Core.Name "object")

_Restriction_multiple = (Core.Name "multiple")

_Restriction_reference = (Core.Name "reference")

_Restriction_title = (Core.Name "title")

_Restriction_description = (Core.Name "description")

data Type = 
  TypeSingle TypeName |
  TypeMultiple [TypeName]
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra.ext.org.json.schema.Type")

_Type_single = (Core.Name "single")

_Type_multiple = (Core.Name "multiple")

data TypeName = 
  TypeNameString  |
  TypeNameInteger  |
  TypeNameNumber  |
  TypeNameBoolean  |
  TypeNameNull  |
  TypeNameArray  |
  TypeNameObject 
  deriving (Eq, Ord, Read, Show)

_TypeName = (Core.Name "hydra.ext.org.json.schema.TypeName")

_TypeName_string = (Core.Name "string")

_TypeName_integer = (Core.Name "integer")

_TypeName_number = (Core.Name "number")

_TypeName_boolean = (Core.Name "boolean")

_TypeName_null = (Core.Name "null")

_TypeName_array = (Core.Name "array")

_TypeName_object = (Core.Name "object")

data StringRestriction = 
  StringRestrictionMinLength Int |
  StringRestrictionMaxLength Int |
  StringRestrictionPattern RegularExpression
  deriving (Eq, Ord, Read, Show)

_StringRestriction = (Core.Name "hydra.ext.org.json.schema.StringRestriction")

_StringRestriction_minLength = (Core.Name "minLength")

_StringRestriction_maxLength = (Core.Name "maxLength")

_StringRestriction_pattern = (Core.Name "pattern")

newtype RegularExpression = 
  RegularExpression {
    unRegularExpression :: String}
  deriving (Eq, Ord, Read, Show)

_RegularExpression = (Core.Name "hydra.ext.org.json.schema.RegularExpression")

data NumericRestriction = 
  NumericRestrictionMinimum Limit |
  NumericRestrictionMaximum Limit |
  NumericRestrictionMultipleOf Int
  deriving (Eq, Ord, Read, Show)

_NumericRestriction = (Core.Name "hydra.ext.org.json.schema.NumericRestriction")

_NumericRestriction_minimum = (Core.Name "minimum")

_NumericRestriction_maximum = (Core.Name "maximum")

_NumericRestriction_multipleOf = (Core.Name "multipleOf")

data Limit = 
  Limit {
    limitValue :: Int,
    limitExclusive :: Bool}
  deriving (Eq, Ord, Read, Show)

_Limit = (Core.Name "hydra.ext.org.json.schema.Limit")

_Limit_value = (Core.Name "value")

_Limit_exclusive = (Core.Name "exclusive")

data ArrayRestriction = 
  ArrayRestrictionItems Items |
  ArrayRestrictionAdditionalItems AdditionalItems |
  ArrayRestrictionMinItems Int |
  ArrayRestrictionMaxItems Int |
  ArrayRestrictionUniqueItems Bool
  deriving (Eq, Ord, Read, Show)

_ArrayRestriction = (Core.Name "hydra.ext.org.json.schema.ArrayRestriction")

_ArrayRestriction_items = (Core.Name "items")

_ArrayRestriction_additionalItems = (Core.Name "additionalItems")

_ArrayRestriction_minItems = (Core.Name "minItems")

_ArrayRestriction_maxItems = (Core.Name "maxItems")

_ArrayRestriction_uniqueItems = (Core.Name "uniqueItems")

data Items = 
  ItemsSameItems Schema |
  ItemsVarItems [Schema]
  deriving (Eq, Ord, Read, Show)

_Items = (Core.Name "hydra.ext.org.json.schema.Items")

_Items_sameItems = (Core.Name "sameItems")

_Items_varItems = (Core.Name "varItems")

data AdditionalItems = 
  AdditionalItemsAny Bool |
  AdditionalItemsSchema Schema
  deriving (Eq, Ord, Read, Show)

_AdditionalItems = (Core.Name "hydra.ext.org.json.schema.AdditionalItems")

_AdditionalItems_any = (Core.Name "any")

_AdditionalItems_schema = (Core.Name "schema")

data ObjectRestriction = 
  ObjectRestrictionProperties (M.Map Keyword Schema) |
  ObjectRestrictionAdditionalProperties AdditionalItems |
  ObjectRestrictionRequired [Keyword] |
  ObjectRestrictionMinProperties Int |
  ObjectRestrictionMaxProperties Int |
  ObjectRestrictionDependencies (M.Map Keyword SchemaOrArray) |
  ObjectRestrictionPatternProperties (M.Map RegularExpression Schema)
  deriving (Eq, Ord, Read, Show)

_ObjectRestriction = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction")

_ObjectRestriction_properties = (Core.Name "properties")

_ObjectRestriction_additionalProperties = (Core.Name "additionalProperties")

_ObjectRestriction_required = (Core.Name "required")

_ObjectRestriction_minProperties = (Core.Name "minProperties")

_ObjectRestriction_maxProperties = (Core.Name "maxProperties")

_ObjectRestriction_dependencies = (Core.Name "dependencies")

_ObjectRestriction_patternProperties = (Core.Name "patternProperties")

data SchemaOrArray = 
  SchemaOrArraySchema Schema |
  SchemaOrArrayArray [Keyword]
  deriving (Eq, Ord, Read, Show)

_SchemaOrArray = (Core.Name "hydra.ext.org.json.schema.SchemaOrArray")

_SchemaOrArray_schema = (Core.Name "schema")

_SchemaOrArray_array = (Core.Name "array")

data MultipleRestriction = 
  MultipleRestrictionAllOf [Schema] |
  MultipleRestrictionAnyOf [Schema] |
  MultipleRestrictionOneOf [Schema] |
  MultipleRestrictionNot Schema |
  MultipleRestrictionEnum [Json.Value]
  deriving (Eq, Ord, Read, Show)

_MultipleRestriction = (Core.Name "hydra.ext.org.json.schema.MultipleRestriction")

_MultipleRestriction_allOf = (Core.Name "allOf")

_MultipleRestriction_anyOf = (Core.Name "anyOf")

_MultipleRestriction_oneOf = (Core.Name "oneOf")

_MultipleRestriction_not = (Core.Name "not")

_MultipleRestriction_enum = (Core.Name "enum")

newtype SchemaReference = 
  SchemaReference {
    unSchemaReference :: String}
  deriving (Eq, Ord, Read, Show)

_SchemaReference = (Core.Name "hydra.ext.org.json.schema.SchemaReference")