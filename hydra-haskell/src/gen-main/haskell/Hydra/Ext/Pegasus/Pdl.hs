module Hydra.Ext.Pegasus.Pdl where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Json.Json as Json
import Data.Map
import Data.Set

-- Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas
data Annotations 
  = Annotations {
    annotationsDoc :: (Maybe String),
    annotationsDeprecated :: Bool}
  deriving (Eq, Ord, Read, Show)

_Annotations = "hydra/ext/pegasus/pdl.Annotations"

_Annotations_doc = "doc"

_Annotations_deprecated = "deprecated"

data EnumField 
  = EnumField {
    enumFieldName :: EnumFieldName,
    enumFieldAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_EnumField = "hydra/ext/pegasus/pdl.EnumField"

_EnumField_name = "name"

_EnumField_annotations = "annotations"

newtype EnumFieldName 
  = EnumFieldName String
  deriving (Eq, Ord, Read, Show)

_EnumFieldName = "hydra/ext/pegasus/pdl.EnumFieldName"

data EnumSchema 
  = EnumSchema {enumSchemaFields :: [EnumField]}
  deriving (Eq, Ord, Read, Show)

_EnumSchema = "hydra/ext/pegasus/pdl.EnumSchema"

_EnumSchema_fields = "fields"

newtype FieldName 
  = FieldName String
  deriving (Eq, Ord, Read, Show)

_FieldName = "hydra/ext/pegasus/pdl.FieldName"

data NamedSchema 
  = NamedSchema {
    namedSchemaQualifiedName :: QualifiedName,
    namedSchemaType :: NamedSchema_Type,
    namedSchemaAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_NamedSchema = "hydra/ext/pegasus/pdl.NamedSchema"

_NamedSchema_qualifiedName = "qualifiedName"

_NamedSchema_type = "type"

_NamedSchema_annotations = "annotations"

data NamedSchema_Type 
  = NamedSchema_TypeRecord RecordSchema
  | NamedSchema_TypeEnum EnumSchema
  | NamedSchema_TypeTyperef Schema
  deriving (Eq, Ord, Read, Show)

_NamedSchema_Type = "hydra/ext/pegasus/pdl.NamedSchema.Type"

_NamedSchema_Type_record = "record"

_NamedSchema_Type_enum = "enum"

_NamedSchema_Type_typeref = "typeref"

newtype Name 
  = Name String
  deriving (Eq, Ord, Read, Show)

_Name = "hydra/ext/pegasus/pdl.Name"

newtype Namespace 
  = Namespace String
  deriving (Eq, Ord, Read, Show)

_Namespace = "hydra/ext/pegasus/pdl.Namespace"

newtype Package 
  = Package String
  deriving (Eq, Ord, Read, Show)

_Package = "hydra/ext/pegasus/pdl.Package"

data PrimitiveType 
  = PrimitiveTypeBoolean 
  | PrimitiveTypeBytes 
  | PrimitiveTypeDouble 
  | PrimitiveTypeFloat 
  | PrimitiveTypeInt 
  | PrimitiveTypeLong 
  | PrimitiveTypeString 
  deriving (Eq, Ord, Read, Show)

_PrimitiveType = "hydra/ext/pegasus/pdl.PrimitiveType"

_PrimitiveType_boolean = "boolean"

_PrimitiveType_bytes = "bytes"

_PrimitiveType_double = "double"

_PrimitiveType_float = "float"

_PrimitiveType_int = "int"

_PrimitiveType_long = "long"

_PrimitiveType_string = "string"

newtype PropertyKey 
  = PropertyKey String
  deriving (Eq, Ord, Read, Show)

_PropertyKey = "hydra/ext/pegasus/pdl.PropertyKey"

data Property 
  = Property {
    propertyKey :: PropertyKey,
    propertyValue :: (Maybe Json.Value)}
  deriving (Eq, Ord, Read, Show)

_Property = "hydra/ext/pegasus/pdl.Property"

_Property_key = "key"

_Property_value = "value"

data QualifiedName 
  = QualifiedName {
    qualifiedNameName :: Name,
    qualifiedNameNamespace :: (Maybe Namespace)}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = "hydra/ext/pegasus/pdl.QualifiedName"

_QualifiedName_name = "name"

_QualifiedName_namespace = "namespace"

data RecordField 
  = RecordField {
    recordFieldName :: FieldName,
    recordFieldValue :: Schema,
    recordFieldOptional :: Bool,
    recordFieldDefault :: (Maybe Json.Value),
    recordFieldAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_RecordField = "hydra/ext/pegasus/pdl.RecordField"

_RecordField_name = "name"

_RecordField_value = "value"

_RecordField_optional = "optional"

_RecordField_default = "default"

_RecordField_annotations = "annotations"

data RecordSchema 
  = RecordSchema {
    recordSchemaFields :: [RecordField],
    recordSchemaIncludes :: [NamedSchema]}
  deriving (Eq, Ord, Read, Show)

_RecordSchema = "hydra/ext/pegasus/pdl.RecordSchema"

_RecordSchema_fields = "fields"

_RecordSchema_includes = "includes"

data Schema 
  = SchemaArray Schema
  | SchemaFixed Int
  | SchemaInline NamedSchema
  | SchemaMap Schema
  | SchemaNamed QualifiedName
  | SchemaNull 
  | SchemaPrimitive PrimitiveType
  | SchemaUnion UnionSchema
  deriving (Eq, Ord, Read, Show)

_Schema = "hydra/ext/pegasus/pdl.Schema"

_Schema_array = "array"

_Schema_fixed = "fixed"

_Schema_inline = "inline"

_Schema_map = "map"

_Schema_named = "named"

_Schema_null = "null"

_Schema_primitive = "primitive"

_Schema_union = "union"

data SchemaFile 
  = SchemaFile {
    schemaFileNamespace :: Namespace,
    schemaFilePackage :: (Maybe Package),
    schemaFileImports :: [QualifiedName],
    schemaFileSchemas :: [NamedSchema]}
  deriving (Eq, Ord, Read, Show)

_SchemaFile = "hydra/ext/pegasus/pdl.SchemaFile"

_SchemaFile_namespace = "namespace"

_SchemaFile_package = "package"

_SchemaFile_imports = "imports"

_SchemaFile_schemas = "schemas"

data UnionMember 
  = UnionMember {
    unionMemberAlias :: (Maybe FieldName),
    unionMemberValue :: Schema,
    unionMemberAnnotations :: Annotations}
  deriving (Eq, Ord, Read, Show)

_UnionMember = "hydra/ext/pegasus/pdl.UnionMember"

_UnionMember_alias = "alias"

_UnionMember_value = "value"

_UnionMember_annotations = "annotations"

newtype UnionSchema 
  = UnionSchema [UnionMember]
  deriving (Eq, Ord, Read, Show)

_UnionSchema = "hydra/ext/pegasus/pdl.UnionSchema"