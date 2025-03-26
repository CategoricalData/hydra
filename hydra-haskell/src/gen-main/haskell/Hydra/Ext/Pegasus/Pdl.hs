-- | A model for PDL (Pegasus Data Language) schemas. Based on the specification at:
-- |   https://linkedin.github.io/rest.li/pdl_schema

module Hydra.Ext.Pegasus.Pdl where

import qualified Hydra.Core as Core
import qualified Hydra.Json as Json
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas
data Annotations = 
  Annotations {
    annotationsDoc :: (Maybe String),
    annotationsDeprecated :: Bool}
  deriving (Eq, Ord, Read, Show)

_Annotations = (Core.Name "hydra.ext.pegasus.pdl.Annotations")

_Annotations_doc = (Core.Name "doc")

_Annotations_deprecated = (Core.Name "deprecated")

data EnumField = 
  EnumField {
    enumFieldName :: EnumFieldName,
    enumFieldAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_EnumField = (Core.Name "hydra.ext.pegasus.pdl.EnumField")

_EnumField_name = (Core.Name "name")

_EnumField_annotations = (Core.Name "annotations")

newtype EnumFieldName = 
  EnumFieldName {
    unEnumFieldName :: String}
  deriving (Eq, Ord, Read, Show)

_EnumFieldName = (Core.Name "hydra.ext.pegasus.pdl.EnumFieldName")

data EnumSchema = 
  EnumSchema {
    enumSchemaFields :: [EnumField]}
  deriving (Eq, Ord, Read, Show)

_EnumSchema = (Core.Name "hydra.ext.pegasus.pdl.EnumSchema")

_EnumSchema_fields = (Core.Name "fields")

newtype FieldName = 
  FieldName {
    unFieldName :: String}
  deriving (Eq, Ord, Read, Show)

_FieldName = (Core.Name "hydra.ext.pegasus.pdl.FieldName")

data NamedSchema = 
  NamedSchema {
    namedSchemaQualifiedName :: QualifiedName,
    namedSchemaType :: NamedSchemaType,
    namedSchemaAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_NamedSchema = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema")

_NamedSchema_qualifiedName = (Core.Name "qualifiedName")

_NamedSchema_type = (Core.Name "type")

_NamedSchema_annotations = (Core.Name "annotations")

data NamedSchemaType = 
  NamedSchemaTypeRecord RecordSchema |
  NamedSchemaTypeEnum EnumSchema |
  NamedSchemaTypeTyperef Schema
  deriving (Eq, Ord, Read, Show)

_NamedSchemaType = (Core.Name "hydra.ext.pegasus.pdl.NamedSchemaType")

_NamedSchemaType_record = (Core.Name "record")

_NamedSchemaType_enum = (Core.Name "enum")

_NamedSchemaType_typeref = (Core.Name "typeref")

newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra.ext.pegasus.pdl.Name")

newtype Namespace = 
  Namespace {
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra.ext.pegasus.pdl.Namespace")

newtype Package = 
  Package {
    unPackage :: String}
  deriving (Eq, Ord, Read, Show)

_Package = (Core.Name "hydra.ext.pegasus.pdl.Package")

data PrimitiveType = 
  PrimitiveTypeBoolean  |
  PrimitiveTypeBytes  |
  PrimitiveTypeDouble  |
  PrimitiveTypeFloat  |
  PrimitiveTypeInt  |
  PrimitiveTypeLong  |
  PrimitiveTypeString 
  deriving (Eq, Ord, Read, Show)

_PrimitiveType = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType")

_PrimitiveType_boolean = (Core.Name "boolean")

_PrimitiveType_bytes = (Core.Name "bytes")

_PrimitiveType_double = (Core.Name "double")

_PrimitiveType_float = (Core.Name "float")

_PrimitiveType_int = (Core.Name "int")

_PrimitiveType_long = (Core.Name "long")

_PrimitiveType_string = (Core.Name "string")

newtype PropertyKey = 
  PropertyKey {
    unPropertyKey :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKey = (Core.Name "hydra.ext.pegasus.pdl.PropertyKey")

data Property = 
  Property {
    propertyKey :: PropertyKey,
    propertyValue :: (Maybe Json.Value)}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra.ext.pegasus.pdl.Property")

_Property_key = (Core.Name "key")

_Property_value = (Core.Name "value")

data QualifiedName = 
  QualifiedName {
    qualifiedNameName :: Name,
    qualifiedNameNamespace :: (Maybe Namespace)}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName")

_QualifiedName_name = (Core.Name "name")

_QualifiedName_namespace = (Core.Name "namespace")

data RecordField = 
  RecordField {
    recordFieldName :: FieldName,
    recordFieldValue :: Schema,
    recordFieldOptional :: Bool,
    recordFieldDefault :: (Maybe Json.Value),
    recordFieldAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_RecordField = (Core.Name "hydra.ext.pegasus.pdl.RecordField")

_RecordField_name = (Core.Name "name")

_RecordField_value = (Core.Name "value")

_RecordField_optional = (Core.Name "optional")

_RecordField_default = (Core.Name "default")

_RecordField_annotations = (Core.Name "annotations")

data RecordSchema = 
  RecordSchema {
    recordSchemaFields :: [RecordField],
    recordSchemaIncludes :: [NamedSchema]}
  deriving (Eq, Ord, Read, Show)

_RecordSchema = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema")

_RecordSchema_fields = (Core.Name "fields")

_RecordSchema_includes = (Core.Name "includes")

data Schema = 
  SchemaArray Schema |
  SchemaFixed Int |
  SchemaInline NamedSchema |
  SchemaMap Schema |
  SchemaNamed QualifiedName |
  SchemaNull  |
  SchemaPrimitive PrimitiveType |
  SchemaUnion UnionSchema
  deriving (Eq, Ord, Read, Show)

_Schema = (Core.Name "hydra.ext.pegasus.pdl.Schema")

_Schema_array = (Core.Name "array")

_Schema_fixed = (Core.Name "fixed")

_Schema_inline = (Core.Name "inline")

_Schema_map = (Core.Name "map")

_Schema_named = (Core.Name "named")

_Schema_null = (Core.Name "null")

_Schema_primitive = (Core.Name "primitive")

_Schema_union = (Core.Name "union")

data SchemaFile = 
  SchemaFile {
    schemaFileNamespace :: Namespace,
    schemaFilePackage :: (Maybe Package),
    schemaFileImports :: [QualifiedName],
    schemaFileSchemas :: [NamedSchema]}
  deriving (Eq, Ord, Read, Show)

_SchemaFile = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile")

_SchemaFile_namespace = (Core.Name "namespace")

_SchemaFile_package = (Core.Name "package")

_SchemaFile_imports = (Core.Name "imports")

_SchemaFile_schemas = (Core.Name "schemas")

data UnionMember = 
  UnionMember {
    unionMemberAlias :: (Maybe FieldName),
    unionMemberValue :: Schema,
    unionMemberAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_UnionMember = (Core.Name "hydra.ext.pegasus.pdl.UnionMember")

_UnionMember_alias = (Core.Name "alias")

_UnionMember_value = (Core.Name "value")

_UnionMember_annotations = (Core.Name "annotations")

newtype UnionSchema = 
  UnionSchema {
    unUnionSchema :: [UnionMember]}
  deriving (Eq, Ord, Read, Show)

_UnionSchema = (Core.Name "hydra.ext.pegasus.pdl.UnionSchema")
