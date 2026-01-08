module Hydra.Ext.Sources.Pegasus.Pdl where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Json.Model               as JsonModel


ns :: Namespace
ns = Namespace "hydra.ext.pegasus.pdl"

define :: String -> Type -> Binding
define = defineType ns

pdl :: String -> Type
pdl = typeref ns

json :: String -> Type
json = typeref $ JsonModel.ns

module_ :: Module
module_ = Module ns elements [JsonModel.ns] [Core.ns] $
    Just ("A model for PDL (Pegasus Data Language) schemas. Based on the specification at:\n" ++
      "  https://linkedin.github.io/rest.li/pdl_schema")
  where
    elements = [
      annotations,
      enumField,
      enumFieldName,
      enumSchema,
      fieldName_,
      namedSchema,
      namedSchemaType,
      name_,
      namespace_,
      package_,
      primitiveType_,
      propertyKey,
      property_,
      qualifiedName,
      recordField,
      recordSchema,
      schema,
      schemaFile,
      unionMember,
      unionSchema]

annotations :: Binding
annotations = define "Annotations" $
  doc "Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas" $
  T.record [
    "doc">: T.maybe T.string,
    "deprecated">: T.boolean]

enumField :: Binding
enumField = define "EnumField" $
  T.record [
    "name">: pdl "EnumFieldName",
    "annotations">: pdl "Annotations"]

enumFieldName :: Binding
enumFieldName = define "EnumFieldName" $
  T.wrap T.string

enumSchema :: Binding
enumSchema = define "EnumSchema" $
  T.record [
    "fields">: T.list $ pdl "EnumField"]

fieldName_ :: Binding
fieldName_ = define "FieldName" $
  T.wrap T.string

namedSchema :: Binding
namedSchema = define "NamedSchema" $
  T.record [
    "qualifiedName">: pdl "QualifiedName",
    "type">: pdl "NamedSchemaType",
    "annotations">: pdl "Annotations"]

namedSchemaType :: Binding
namedSchemaType = define "NamedSchemaType" $
  T.union [
    "record">: pdl "RecordSchema",
    "enum">: pdl "EnumSchema",
    "typeref">: pdl "Schema"]

name_ :: Binding
name_ = define "Name" $
  T.wrap T.string

namespace_ :: Binding
namespace_ = define "Namespace" $
  T.wrap T.string

package_ :: Binding
package_ = define "Package" $
  T.wrap T.string

primitiveType_ :: Binding
primitiveType_ = define "PrimitiveType" $
  T.enum [
    "boolean",
    "bytes",
    "double",
    "float",
    "int",
    "long",
    "string"]

propertyKey :: Binding
propertyKey = define "PropertyKey" $
  T.wrap T.string

property_ :: Binding
property_ = define "Property" $
  T.record [
    "key">: pdl "PropertyKey",
    "value">: T.maybe $ json "Value"]

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  T.record [
    "name">: pdl "Name",
    "namespace">: T.maybe $ pdl "Namespace"]

recordField :: Binding
recordField = define "RecordField" $
  doc "Note: the default value for an enum-valued must be one of the enumerated string symbols" $
  T.record [
    "name">: pdl "FieldName",
    "value">: pdl "Schema",
    "optional">: T.boolean,
    "default">: T.maybe $ json "Value",
    "annotations">: pdl "Annotations"]

recordSchema :: Binding
recordSchema = define "RecordSchema" $
  doc "Note: all included schemas must be record schemas" $
  T.record [
    "fields">: T.list $ pdl "RecordField",
    "includes">: T.list $ pdl "NamedSchema"]

schema :: Binding
schema = define "Schema" $
  T.union [
    "array">: pdl "Schema",
    "fixed">: T.int32,
    "inline">: pdl "NamedSchema",
    "map">: pdl "Schema",
    "named">: pdl "QualifiedName",
    "null">: T.unit,
    "primitive">: pdl "PrimitiveType",
    "union">: pdl "UnionSchema"]

schemaFile :: Binding
schemaFile = define "SchemaFile" $
  T.record [
    "namespace">: pdl "Namespace",
    "package">: T.maybe $ pdl "Package",
    "imports">: T.list $ pdl "QualifiedName",
    "schemas">: T.list $ pdl "NamedSchema"]

unionMember :: Binding
unionMember = define "UnionMember" $
  doc "Note: annotations are only available for aliased members" $
  T.record [
    "alias">: T.maybe $ pdl "FieldName",
    "value">: pdl "Schema",
    "annotations">: pdl "Annotations"]

unionSchema :: Binding
unionSchema = define "UnionSchema" $
  doc "Note: unions are not allowed as member types of other unions" $
  T.wrap $ T.list $ pdl "UnionMember"
