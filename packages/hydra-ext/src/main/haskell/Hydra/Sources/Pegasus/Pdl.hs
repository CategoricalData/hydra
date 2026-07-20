module Hydra.Sources.Pegasus.Pdl where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Sources.Json.Model        as JsonModel


ns :: ModuleName
ns = ModuleName "hydra.pegasus.pdl"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [JsonModel.ns, Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A model for PDL (Pegasus Data Language) schemas. Based on the specification at:\n" ++
      "  https://linkedin.github.io/rest.li/pdl_schema"))}
  where
    definitions = [
      annotations,
      enumField,
      enumFieldName,
      enumSchema,
      fieldName_,
      name_,
      namedSchema,
      namedSchemaType,
      namespace_,
      package_,
      primitiveType_,
      property_,
      propertyKey,
      qualifiedName,
      recordField,
      recordSchema,
      schema,
      schemaFile,
      unionMember,
      unionSchema]

annotations :: TypeDefinition
annotations = define "Annotations" $
  doc "Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas" $
  T.record [
    "doc">: T.optional T.string,
    "deprecated">: T.boolean]

enumField :: TypeDefinition
enumField = define "EnumField" $
  doc "A single symbol of a PDL enum schema, with its optional annotations" $
  T.record [
    "name">: pdl "EnumFieldName",
    "annotations">: pdl "Annotations"]

enumFieldName :: TypeDefinition
enumFieldName = define "EnumFieldName" $
  doc "The name of a PDL enum symbol, conventionally in upper snake case" $
  T.wrap T.string

enumSchema :: TypeDefinition
enumSchema = define "EnumSchema" $
  doc "A PDL enum schema, defined as a fixed set of enumerated symbols" $
  T.record [
    "fields">: T.list $ pdl "EnumField"]

fieldName_ :: TypeDefinition
fieldName_ = define "FieldName" $
  doc "The name of a record field or an aliased union member" $
  T.wrap T.string

json :: String -> Type
json = typeref $ JsonModel.ns

name_ :: TypeDefinition
name_ = define "Name" $
  doc "The unqualified (non-namespaced) name of a PDL named schema" $
  T.wrap T.string

namedSchema :: TypeDefinition
namedSchema = define "NamedSchema" $
  doc "A PDL schema that has a qualified name: a record, enum, or typeref" $
  T.record [
    "qualifiedName">: pdl "QualifiedName",
    "type">: pdl "NamedSchemaType",
    "annotations">: pdl "Annotations"]

namedSchemaType :: TypeDefinition
namedSchemaType = define "NamedSchemaType" $
  doc "The variety of a PDL named schema: record, enum, or typeref" $
  T.union [
    "record">: pdl "RecordSchema",
    "enum">: pdl "EnumSchema",
    "typeref">: pdl "Schema"]

namespace_ :: TypeDefinition
namespace_ = define "Namespace" $
  doc "A dot-separated PDL namespace, analogous to a Java package name" $
  T.wrap T.string

package_ :: TypeDefinition
package_ = define "Package" $
  doc "The Java package under which generated bindings for a schema file are placed" $
  T.wrap T.string

pdl :: String -> Type
pdl = typeref ns

primitiveType_ :: TypeDefinition
primitiveType_ = define "PrimitiveType" $
  doc "One of the built-in PDL primitive types" $
  T.enum [
    "boolean",
    "bytes",
    "double",
    "float",
    "int",
    "long",
    "string"]

propertyKey :: TypeDefinition
propertyKey = define "PropertyKey" $
  doc "The key of a custom PDL schema property, e.g. a dot-separated path" $
  T.wrap T.string

property_ :: TypeDefinition
property_ = define "Property" $
  doc "A custom key/value property attached to a PDL schema, field, or member" $
  T.record [
    "key">: pdl "PropertyKey",
    "value">: T.optional $ json "Value"]

qualifiedName :: TypeDefinition
qualifiedName = define "QualifiedName" $
  doc "A PDL name together with the optional namespace that qualifies it" $
  T.record [
    "name">: pdl "Name",
    "namespace">: T.optional $ pdl "Namespace"]

recordField :: TypeDefinition
recordField = define "RecordField" $
  doc "Note: the default value for an enum-valued must be one of the enumerated string symbols" $
  T.record [
    "name">: pdl "FieldName",
    "value">: pdl "Schema",
    "optional">: T.boolean,
    "default">: T.optional $ json "Value",
    "annotations">: pdl "Annotations"]

recordSchema :: TypeDefinition
recordSchema = define "RecordSchema" $
  doc "Note: all included schemas must be record schemas" $
  T.record [
    "fields">: T.list $ pdl "RecordField",
    "includes">: T.list $ pdl "NamedSchema"]

schema :: TypeDefinition
schema = define "Schema" $
  doc "A PDL data schema: either a primitive, a reference by name, or a complex type" $
  T.union [
    "array">: pdl "Schema",
    "fixed">: T.int32,
    "inline">: pdl "NamedSchema",
    "map">: pdl "Schema",
    "named">: pdl "QualifiedName",
    "null">: T.unit,
    "primitive">: pdl "PrimitiveType",
    "union">: pdl "UnionSchema"]

schemaFile :: TypeDefinition
schemaFile = define "SchemaFile" $
  doc "A .pdl source file: a namespace declaration, imports, and one or more named schemas" $
  T.record [
    "namespace">: pdl "Namespace",
    "package">: T.optional $ pdl "Package",
    "imports">: T.list $ pdl "QualifiedName",
    "schemas">: T.list $ pdl "NamedSchema"]

unionMember :: TypeDefinition
unionMember = define "UnionMember" $
  doc "Note: annotations are only available for aliased members" $
  T.record [
    "alias">: T.optional $ pdl "FieldName",
    "value">: pdl "Schema",
    "annotations">: pdl "Annotations"]

unionSchema :: TypeDefinition
unionSchema = define "UnionSchema" $
  doc "Note: unions are not allowed as member types of other unions" $
  T.wrap $ T.list $ pdl "UnionMember"
