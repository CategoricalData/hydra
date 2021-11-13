{-# LANGUAGE DeriveGeneric #-}
-- | Model for PDL (Pegasus Data Language) schemas
module Hydra.Ext.Pegasus.Pdl
  ( Annotations(..)
  , EnumField(..)
  , EnumFieldName
  , EnumSchema(..)
  , FieldName
  , Name
  , NamedSchema(..)
  , Namespace
  , Package
  , PrimitiveType
  , Property(..)
  , PropertyKey
  , QualifiedName(..)
  , RecordField(..)
  , RecordSchema(..)
  , Schema(..)
  , SchemaFile(..)
  , UnionMember(..)
  , UnionSchema
  , _Annotations
  , _Annotations_deprecated
  , _Annotations_doc
  , _EnumField
  , _EnumFieldName
  , _EnumField_annotations
  , _EnumField_name
  , _EnumSchema
  , _EnumSchema_fields
  , _FieldName
  , _Name
  , _NamedSchema
  , _NamedSchema_annotations
  , _NamedSchema_qualifiedName
  , _NamedSchema_type
  , _Namespace
  , _Package
  , _PrimitiveType
  , _Property
  , _PropertyKey
  , _Property_key
  , _Property_value
  , _QualifiedName
  , _QualifiedName_name
  , _QualifiedName_namespace
  , _RecordField
  , _RecordField_annotations
  , _RecordField_defaultEsc
  , _RecordField_name
  , _RecordField_optional
  , _RecordField_value
  , _RecordSchema
  , _RecordSchema_fields
  , _RecordSchema_includes
  , _Schema
  , _SchemaFile
  , _SchemaFile_imports
  , _SchemaFile_namespace
  , _SchemaFile_package
  , _SchemaFile_schemas
  , _Schema_array
  , _Schema_fixed
  , _Schema_inline
  , _Schema_map
  , _Schema_named
  , _Schema_primitive
  , _Schema_union
  , _UnionMember
  , _UnionMember_alias
  , _UnionMember_annotations
  , _UnionMember_value
  , _UnionSchema
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set
import Hydra.Ext.Json.Json

{-| Annotations which can be applied to record fields, aliased union members,
    enum symbols, or named schemas -}
data Annotations
  = Annotations
    -- | @type optional: string
    { annotationsDoc :: Maybe String
    -- | @type boolean
    , annotationsDeprecated :: Bool } deriving (Eq, Generic, Ord, Read, Show)

data EnumField
  = EnumField
    -- | @type hydra/ext/pegasus/pdl.EnumFieldName
    { enumFieldName :: EnumFieldName
    -- | @type hydra/ext/pegasus/pdl.Annotations
    , enumFieldAnnotations :: Annotations } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type EnumFieldName = String

data EnumSchema
  = EnumSchema
    -- | @type list: hydra/ext/pegasus/pdl.EnumField
    { enumSchemaFields :: [EnumField] } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type FieldName = String

-- | @type string
type Name = String

data NamedSchema
  = NamedSchema
    -- | @type hydra/ext/pegasus/pdl.QualifiedName
    { namedSchemaQualifiedName :: QualifiedName
    , namedSchemaType :: ()
    -- | @type hydra/ext/pegasus/pdl.Annotations
    , namedSchemaAnnotations :: Annotations } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type Namespace = String

-- | @type string
type Package = String

type PrimitiveType = ()

data Property
  = Property
    -- | @type hydra/ext/pegasus/pdl.PropertyKey
    { propertyKey :: PropertyKey
    -- | @type optional: hydra/ext/json/json.Value
    , propertyValue :: Maybe Value } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type PropertyKey = String

data QualifiedName
  = QualifiedName
    -- | @type hydra/ext/pegasus/pdl.Name
    { qualifiedNameName :: Name
    -- | @type optional: hydra/ext/pegasus/pdl.Namespace
    , qualifiedNameNamespace :: Maybe Namespace } deriving (Eq, Generic, Ord, Read, Show)

data RecordField
  = RecordField
    -- | @type hydra/ext/pegasus/pdl.FieldName
    { recordFieldName :: FieldName
    -- | @type hydra/ext/pegasus/pdl.Schema
    , recordFieldValue :: Schema
    -- | @type boolean
    , recordFieldOptional :: Bool
    {-| @comments The default value for an enum-valued field must be one of the
        enumerated string symbols
        @type optional: hydra/ext/json/json.Value -}
    , recordFieldDefaultEsc :: Maybe Value
    -- | @type hydra/ext/pegasus/pdl.Annotations
    , recordFieldAnnotations :: Annotations } deriving (Eq, Generic, Ord, Read, Show)

data RecordSchema
  = RecordSchema
    -- | @type list: hydra/ext/pegasus/pdl.RecordField
    { recordSchemaFields :: [RecordField]
    {-| @comments All included schemas must be record schemas
        @type list: hydra/ext/pegasus/pdl.NamedSchema -}
    , recordSchemaIncludes :: [NamedSchema] } deriving (Eq, Generic, Ord, Read, Show)

data Schema
  -- | @type hydra/ext/pegasus/pdl.Schema
  = SchemaArray Schema
  -- | @type integer
  | SchemaFixed Int
  -- | @type hydra/ext/pegasus/pdl.NamedSchema
  | SchemaInline NamedSchema
  -- | @type hydra/ext/pegasus/pdl.Schema
  | SchemaMap Schema
  -- | @type hydra/ext/pegasus/pdl.QualifiedName
  | SchemaNamed QualifiedName
  -- | @type hydra/ext/pegasus/pdl.PrimitiveType
  | SchemaPrimitive PrimitiveType
  -- | @type hydra/ext/pegasus/pdl.UnionSchema
  | SchemaUnion UnionSchema deriving (Eq, Generic, Ord, Read, Show)

data SchemaFile
  = SchemaFile
    -- | @type hydra/ext/pegasus/pdl.Namespace
    { schemaFileNamespace :: Namespace
    -- | @type optional: hydra/ext/pegasus/pdl.Package
    , schemaFilePackage :: Maybe Package
    -- | @type list: hydra/ext/pegasus/pdl.QualifiedName
    , schemaFileImports :: [QualifiedName]
    -- | @type list: hydra/ext/pegasus/pdl.NamedSchema
    , schemaFileSchemas :: [NamedSchema] } deriving (Eq, Generic, Ord, Read, Show)

data UnionMember
  = UnionMember
    -- | @type optional: hydra/ext/pegasus/pdl.FieldName
    { unionMemberAlias :: Maybe FieldName
    -- | @type hydra/ext/pegasus/pdl.Schema
    , unionMemberValue :: Schema
    {-| @comments Annotations are only available for aliased members
        @type hydra/ext/pegasus/pdl.Annotations -}
    , unionMemberAnnotations :: Annotations } deriving (Eq, Generic, Ord, Read, Show)

{-| @comments Unions are not allowed as member types of other unions
    @type list: hydra/ext/pegasus/pdl.UnionMember -}
type UnionSchema = [UnionMember]

_Annotations = "hydra/ext/pegasus/pdl.Annotations" :: String
_Annotations_deprecated = "deprecated" :: String
_Annotations_doc = "doc" :: String
_EnumField = "hydra/ext/pegasus/pdl.EnumField" :: String
_EnumFieldName = "hydra/ext/pegasus/pdl.EnumFieldName" :: String
_EnumField_annotations = "annotations" :: String
_EnumField_name = "name" :: String
_EnumSchema = "hydra/ext/pegasus/pdl.EnumSchema" :: String
_EnumSchema_fields = "fields" :: String
_FieldName = "hydra/ext/pegasus/pdl.FieldName" :: String
_Name = "hydra/ext/pegasus/pdl.Name" :: String
_NamedSchema = "hydra/ext/pegasus/pdl.NamedSchema" :: String
_NamedSchema_annotations = "annotations" :: String
_NamedSchema_qualifiedName = "qualifiedName" :: String
_NamedSchema_type = "type" :: String
_Namespace = "hydra/ext/pegasus/pdl.Namespace" :: String
_Package = "hydra/ext/pegasus/pdl.Package" :: String
_PrimitiveType = "hydra/ext/pegasus/pdl.PrimitiveType" :: String
_Property = "hydra/ext/pegasus/pdl.Property" :: String
_PropertyKey = "hydra/ext/pegasus/pdl.PropertyKey" :: String
_Property_key = "key" :: String
_Property_value = "value" :: String
_QualifiedName = "hydra/ext/pegasus/pdl.QualifiedName" :: String
_QualifiedName_name = "name" :: String
_QualifiedName_namespace = "namespace" :: String
_RecordField = "hydra/ext/pegasus/pdl.RecordField" :: String
_RecordField_annotations = "annotations" :: String
_RecordField_defaultEsc = "defaultEsc" :: String
_RecordField_name = "name" :: String
_RecordField_optional = "optional" :: String
_RecordField_value = "value" :: String
_RecordSchema = "hydra/ext/pegasus/pdl.RecordSchema" :: String
_RecordSchema_fields = "fields" :: String
_RecordSchema_includes = "includes" :: String
_Schema = "hydra/ext/pegasus/pdl.Schema" :: String
_SchemaFile = "hydra/ext/pegasus/pdl.SchemaFile" :: String
_SchemaFile_imports = "imports" :: String
_SchemaFile_namespace = "namespace" :: String
_SchemaFile_package = "package" :: String
_SchemaFile_schemas = "schemas" :: String
_Schema_array = "array" :: String
_Schema_fixed = "fixed" :: String
_Schema_inline = "inline" :: String
_Schema_map = "map" :: String
_Schema_named = "named" :: String
_Schema_primitive = "primitive" :: String
_Schema_union = "union" :: String
_UnionMember = "hydra/ext/pegasus/pdl.UnionMember" :: String
_UnionMember_alias = "alias" :: String
_UnionMember_annotations = "annotations" :: String
_UnionMember_value = "value" :: String
_UnionSchema = "hydra/ext/pegasus/pdl.UnionSchema" :: String
