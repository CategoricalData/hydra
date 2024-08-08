-- | A model for PDL (Pegasus Data Language) schemas. Based on the specification at:
-- |   https://linkedin.github.io/rest.li/pdl_schema

module Hydra.Langs.Pegasus.Pdl where

import qualified Hydra.Core as Core
import qualified Hydra.Json as Json
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas
data Annotations = 
  Annotations {
    annotationsDoc :: (Maybe String),
    annotationsDeprecated :: Bool}
  deriving (Eq, Ord, Read, Show)

_Annotations = (Core.Name "hydra/langs/pegasus/pdl.Annotations")

_Annotations_doc = (Core.Name "doc")

_Annotations_deprecated = (Core.Name "deprecated")

_Annotations_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.Annotations"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "doc"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "deprecated"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

data EnumField = 
  EnumField {
    enumFieldName :: EnumFieldName,
    enumFieldAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_EnumField = (Core.Name "hydra/langs/pegasus/pdl.EnumField")

_EnumField_name = (Core.Name "name")

_EnumField_annotations = (Core.Name "annotations")

_EnumField_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.EnumField"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _EnumFieldName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = _Annotations_type_}]}))

newtype EnumFieldName = 
  EnumFieldName {
    unEnumFieldName :: String}
  deriving (Eq, Ord, Read, Show)

_EnumFieldName = (Core.Name "hydra/langs/pegasus/pdl.EnumFieldName")

_EnumFieldName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data EnumSchema = 
  EnumSchema {
    enumSchemaFields :: [EnumField]}
  deriving (Eq, Ord, Read, Show)

_EnumSchema = (Core.Name "hydra/langs/pegasus/pdl.EnumSchema")

_EnumSchema_fields = (Core.Name "fields")

_EnumSchema_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.EnumSchema"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _EnumField_type_)}]}))

newtype FieldName = 
  FieldName {
    unFieldName :: String}
  deriving (Eq, Ord, Read, Show)

_FieldName = (Core.Name "hydra/langs/pegasus/pdl.FieldName")

_FieldName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data NamedSchema = 
  NamedSchema {
    namedSchemaQualifiedName :: QualifiedName,
    namedSchemaType :: NamedSchema_Type,
    namedSchemaAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_NamedSchema = (Core.Name "hydra/langs/pegasus/pdl.NamedSchema")

_NamedSchema_qualifiedName = (Core.Name "qualifiedName")

_NamedSchema_type = (Core.Name "type")

_NamedSchema_annotations = (Core.Name "annotations")

_NamedSchema_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.NamedSchema"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiedName"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _NamedSchema_Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = _Annotations_type_}]}))

data NamedSchema_Type = 
  NamedSchema_TypeRecord RecordSchema |
  NamedSchema_TypeEnum EnumSchema |
  NamedSchema_TypeTyperef Schema
  deriving (Eq, Ord, Read, Show)

_NamedSchema_Type = (Core.Name "hydra/langs/pegasus/pdl.NamedSchema.Type")

_NamedSchema_Type_record = (Core.Name "record")

_NamedSchema_Type_enum = (Core.Name "enum")

_NamedSchema_Type_typeref = (Core.Name "typeref")

_NamedSchema_Type_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.NamedSchema.Type"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "record"),
      Core.fieldTypeType = _RecordSchema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enum"),
      Core.fieldTypeType = _EnumSchema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeref"),
      Core.fieldTypeType = _Schema_type_}]}))

newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/langs/pegasus/pdl.Name")

_Name_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Namespace = 
  Namespace {
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra/langs/pegasus/pdl.Namespace")

_Namespace_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Package = 
  Package {
    unPackage :: String}
  deriving (Eq, Ord, Read, Show)

_Package = (Core.Name "hydra/langs/pegasus/pdl.Package")

_Package_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data PrimitiveType = 
  PrimitiveTypeBoolean  |
  PrimitiveTypeBytes  |
  PrimitiveTypeDouble  |
  PrimitiveTypeFloat  |
  PrimitiveTypeInt  |
  PrimitiveTypeLong  |
  PrimitiveTypeString 
  deriving (Eq, Ord, Read, Show)

_PrimitiveType = (Core.Name "hydra/langs/pegasus/pdl.PrimitiveType")

_PrimitiveType_boolean = (Core.Name "boolean")

_PrimitiveType_bytes = (Core.Name "bytes")

_PrimitiveType_double = (Core.Name "double")

_PrimitiveType_float = (Core.Name "float")

_PrimitiveType_int = (Core.Name "int")

_PrimitiveType_long = (Core.Name "long")

_PrimitiveType_string = (Core.Name "string")

_PrimitiveType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.PrimitiveType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bytes"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "long"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype PropertyKey = 
  PropertyKey {
    unPropertyKey :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKey = (Core.Name "hydra/langs/pegasus/pdl.PropertyKey")

_PropertyKey_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Property = 
  Property {
    propertyKey :: PropertyKey,
    propertyValue :: (Maybe Json.Value)}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/langs/pegasus/pdl.Property")

_Property_key = (Core.Name "key")

_Property_value = (Core.Name "value")

_Property_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.Property"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = _PropertyKey_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeOptional Json._Value_type_)}]}))

data QualifiedName = 
  QualifiedName {
    qualifiedNameName :: Name,
    qualifiedNameNamespace :: (Maybe Namespace)}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra/langs/pegasus/pdl.QualifiedName")

_QualifiedName_name = (Core.Name "name")

_QualifiedName_namespace = (Core.Name "namespace")

_QualifiedName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.QualifiedName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namespace"),
      Core.fieldTypeType = (Core.TypeOptional _Namespace_type_)}]}))

data RecordField = 
  RecordField {
    recordFieldName :: FieldName,
    recordFieldValue :: Schema,
    recordFieldOptional :: Bool,
    recordFieldDefault :: (Maybe Json.Value),
    recordFieldAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_RecordField = (Core.Name "hydra/langs/pegasus/pdl.RecordField")

_RecordField_name = (Core.Name "name")

_RecordField_value = (Core.Name "value")

_RecordField_optional = (Core.Name "optional")

_RecordField_default = (Core.Name "default")

_RecordField_annotations = (Core.Name "annotations")

_RecordField_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.RecordField"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _FieldName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Schema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "optional"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "default"),
      Core.fieldTypeType = (Core.TypeOptional Json._Value_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = _Annotations_type_}]}))

data RecordSchema = 
  RecordSchema {
    recordSchemaFields :: [RecordField],
    recordSchemaIncludes :: [NamedSchema]}
  deriving (Eq, Ord, Read, Show)

_RecordSchema = (Core.Name "hydra/langs/pegasus/pdl.RecordSchema")

_RecordSchema_fields = (Core.Name "fields")

_RecordSchema_includes = (Core.Name "includes")

_RecordSchema_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.RecordSchema"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _RecordField_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "includes"),
      Core.fieldTypeType = (Core.TypeList _NamedSchema_type_)}]}))

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

_Schema = (Core.Name "hydra/langs/pegasus/pdl.Schema")

_Schema_array = (Core.Name "array")

_Schema_fixed = (Core.Name "fixed")

_Schema_inline = (Core.Name "inline")

_Schema_map = (Core.Name "map")

_Schema_named = (Core.Name "named")

_Schema_null = (Core.Name "null")

_Schema_primitive = (Core.Name "primitive")

_Schema_union = (Core.Name "union")

_Schema_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.Schema"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _Schema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fixed"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inline"),
      Core.fieldTypeType = _NamedSchema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _Schema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "named"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitive"),
      Core.fieldTypeType = _PrimitiveType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = _UnionSchema_type_}]}))

data SchemaFile = 
  SchemaFile {
    schemaFileNamespace :: Namespace,
    schemaFilePackage :: (Maybe Package),
    schemaFileImports :: [QualifiedName],
    schemaFileSchemas :: [NamedSchema]}
  deriving (Eq, Ord, Read, Show)

_SchemaFile = (Core.Name "hydra/langs/pegasus/pdl.SchemaFile")

_SchemaFile_namespace = (Core.Name "namespace")

_SchemaFile_package = (Core.Name "package")

_SchemaFile_imports = (Core.Name "imports")

_SchemaFile_schemas = (Core.Name "schemas")

_SchemaFile_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.SchemaFile"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namespace"),
      Core.fieldTypeType = _Namespace_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "package"),
      Core.fieldTypeType = (Core.TypeOptional _Package_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "imports"),
      Core.fieldTypeType = (Core.TypeList _QualifiedName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "schemas"),
      Core.fieldTypeType = (Core.TypeList _NamedSchema_type_)}]}))

data UnionMember = 
  UnionMember {
    unionMemberAlias :: (Maybe FieldName),
    unionMemberValue :: Schema,
    unionMemberAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_UnionMember = (Core.Name "hydra/langs/pegasus/pdl.UnionMember")

_UnionMember_alias = (Core.Name "alias")

_UnionMember_value = (Core.Name "value")

_UnionMember_annotations = (Core.Name "annotations")

_UnionMember_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/pegasus/pdl.UnionMember"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alias"),
      Core.fieldTypeType = (Core.TypeOptional _FieldName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Schema_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = _Annotations_type_}]}))

newtype UnionSchema = 
  UnionSchema {
    unUnionSchema :: [UnionMember]}
  deriving (Eq, Ord, Read, Show)

_UnionSchema = (Core.Name "hydra/langs/pegasus/pdl.UnionSchema")

_UnionSchema_type_ = (Core.TypeList _UnionMember_type_)