-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.pegasus.pdl

module Hydra.Dsl.Ext.Pegasus.Pdl where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Pegasus.Pdl as Pdl
import qualified Hydra.Json.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotations :: (Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Bool -> Phantoms.TTerm Pdl.Annotations)
annotations doc deprecated = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "doc"),
      Core.fieldTerm = (Phantoms.unTTerm doc)},
    Core.Field {
      Core.fieldName = (Core.Name "deprecated"),
      Core.fieldTerm = (Phantoms.unTTerm deprecated)}]})))

annotationsDoc :: (Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm (Maybe String))
annotationsDoc x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
    Core.projectionField = (Core.Name "doc")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

annotationsDeprecated :: (Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Bool)
annotationsDeprecated x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
    Core.projectionField = (Core.Name "deprecated")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

annotationsWithDoc :: (Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Pdl.Annotations)
annotationsWithDoc original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "doc"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "deprecated"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
          Core.projectionField = (Core.Name "deprecated")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

annotationsWithDeprecated :: (Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Bool -> Phantoms.TTerm Pdl.Annotations)
annotationsWithDeprecated original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "doc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Annotations"),
          Core.projectionField = (Core.Name "doc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "deprecated"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

enumField :: (Phantoms.TTerm Pdl.EnumFieldName -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.EnumField)
enumField name annotations = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm annotations)}]})))

enumFieldName :: (Phantoms.TTerm Pdl.EnumField -> Phantoms.TTerm Pdl.EnumFieldName)
enumFieldName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumFieldAnnotations :: (Phantoms.TTerm Pdl.EnumField -> Phantoms.TTerm Pdl.Annotations)
enumFieldAnnotations x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
    Core.projectionField = (Core.Name "annotations")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumFieldWithName :: (Phantoms.TTerm Pdl.EnumField -> Phantoms.TTerm Pdl.EnumFieldName -> Phantoms.TTerm Pdl.EnumField)
enumFieldWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

enumFieldWithAnnotations :: (Phantoms.TTerm Pdl.EnumField -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.EnumField)
enumFieldWithAnnotations original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

enumFieldName_ :: (Phantoms.TTerm String -> Phantoms.TTerm Pdl.EnumFieldName)
enumFieldName_ x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumFieldName"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unEnumFieldName :: (Phantoms.TTerm Pdl.EnumFieldName -> Phantoms.TTerm String)
unEnumFieldName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.EnumFieldName")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumSchema :: (Phantoms.TTerm [Pdl.EnumField] -> Phantoms.TTerm Pdl.EnumSchema)
enumSchema fields = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Phantoms.unTTerm fields)}]})))

enumSchemaFields :: (Phantoms.TTerm Pdl.EnumSchema -> Phantoms.TTerm [Pdl.EnumField])
enumSchemaFields x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumSchema"),
    Core.projectionField = (Core.Name "fields")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumSchemaWithFields :: (Phantoms.TTerm Pdl.EnumSchema -> Phantoms.TTerm [Pdl.EnumField] -> Phantoms.TTerm Pdl.EnumSchema)
enumSchemaWithFields original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.EnumSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

fieldName :: (Phantoms.TTerm String -> Phantoms.TTerm Pdl.FieldName)
fieldName x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.FieldName"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFieldName :: (Phantoms.TTerm Pdl.FieldName -> Phantoms.TTerm String)
unFieldName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.FieldName")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedSchema :: (Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm Pdl.NamedSchemaType -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.NamedSchema)
namedSchema qualifiedName type_ annotations = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qualifiedName"),
      Core.fieldTerm = (Phantoms.unTTerm qualifiedName)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm annotations)}]})))

namedSchemaQualifiedName :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.QualifiedName)
namedSchemaQualifiedName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
    Core.projectionField = (Core.Name "qualifiedName")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedSchemaType :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.NamedSchemaType)
namedSchemaType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedSchemaAnnotations :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.Annotations)
namedSchemaAnnotations x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
    Core.projectionField = (Core.Name "annotations")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedSchemaWithQualifiedName :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm Pdl.NamedSchema)
namedSchemaWithQualifiedName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qualifiedName"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

namedSchemaWithType :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.NamedSchemaType -> Phantoms.TTerm Pdl.NamedSchema)
namedSchemaWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qualifiedName"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
          Core.projectionField = (Core.Name "qualifiedName")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

namedSchemaWithAnnotations :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.NamedSchema)
namedSchemaWithAnnotations original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qualifiedName"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
          Core.projectionField = (Core.Name "qualifiedName")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchema"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

namedSchemaTypeRecord :: (Phantoms.TTerm Pdl.RecordSchema -> Phantoms.TTerm Pdl.NamedSchemaType)
namedSchemaTypeRecord x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchemaType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "record"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

namedSchemaTypeEnum :: (Phantoms.TTerm Pdl.EnumSchema -> Phantoms.TTerm Pdl.NamedSchemaType)
namedSchemaTypeEnum x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchemaType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "enum"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

namedSchemaTypeTyperef :: (Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Pdl.NamedSchemaType)
namedSchemaTypeTyperef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.NamedSchemaType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeref"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

name :: (Phantoms.TTerm String -> Phantoms.TTerm Pdl.Name)
name x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.Name"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unName :: (Phantoms.TTerm Pdl.Name -> Phantoms.TTerm String)
unName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.Name")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namespace :: (Phantoms.TTerm String -> Phantoms.TTerm Pdl.Namespace)
namespace x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.Namespace"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unNamespace :: (Phantoms.TTerm Pdl.Namespace -> Phantoms.TTerm String)
unNamespace x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.Namespace")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

package :: (Phantoms.TTerm String -> Phantoms.TTerm Pdl.Package)
package x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.Package"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPackage :: (Phantoms.TTerm Pdl.Package -> Phantoms.TTerm String)
unPackage x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.Package")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

primitiveTypeBoolean :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeBoolean = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "boolean"),
    Core.fieldTerm = Core.TermUnit}})))

primitiveTypeBytes :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeBytes = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bytes"),
    Core.fieldTerm = Core.TermUnit}})))

primitiveTypeDouble :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeDouble = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "double"),
    Core.fieldTerm = Core.TermUnit}})))

primitiveTypeFloat :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeFloat = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "float"),
    Core.fieldTerm = Core.TermUnit}})))

primitiveTypeInt :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeInt = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "int"),
    Core.fieldTerm = Core.TermUnit}})))

primitiveTypeLong :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeLong = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "long"),
    Core.fieldTerm = Core.TermUnit}})))

primitiveTypeString :: (Phantoms.TTerm Pdl.PrimitiveType)
primitiveTypeString = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.PrimitiveType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "string"),
    Core.fieldTerm = Core.TermUnit}})))

propertyKey :: (Phantoms.TTerm String -> Phantoms.TTerm Pdl.PropertyKey)
propertyKey x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.PropertyKey"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPropertyKey :: (Phantoms.TTerm Pdl.PropertyKey -> Phantoms.TTerm String)
unPropertyKey x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.PropertyKey")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

property :: (Phantoms.TTerm Pdl.PropertyKey -> Phantoms.TTerm (Maybe Model.Value) -> Phantoms.TTerm Pdl.Property)
property key value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm key)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

propertyKey_ :: (Phantoms.TTerm Pdl.Property -> Phantoms.TTerm Pdl.PropertyKey)
propertyKey_ x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
    Core.projectionField = (Core.Name "key")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

propertyValue :: (Phantoms.TTerm Pdl.Property -> Phantoms.TTerm (Maybe Model.Value))
propertyValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

propertyWithKey :: (Phantoms.TTerm Pdl.Property -> Phantoms.TTerm Pdl.PropertyKey -> Phantoms.TTerm Pdl.Property)
propertyWithKey original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

propertyWithValue :: (Phantoms.TTerm Pdl.Property -> Phantoms.TTerm (Maybe Model.Value) -> Phantoms.TTerm Pdl.Property)
propertyWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Property"),
          Core.projectionField = (Core.Name "key")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

qualifiedName :: (Phantoms.TTerm Pdl.Name -> Phantoms.TTerm (Maybe Pdl.Namespace) -> Phantoms.TTerm Pdl.QualifiedName)
qualifiedName name namespace = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Phantoms.unTTerm namespace)}]})))

qualifiedNameName :: (Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm Pdl.Name)
qualifiedNameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

qualifiedNameNamespace :: (Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm (Maybe Pdl.Namespace))
qualifiedNameNamespace x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
    Core.projectionField = (Core.Name "namespace")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

qualifiedNameWithName :: (Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm Pdl.Name -> Phantoms.TTerm Pdl.QualifiedName)
qualifiedNameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
          Core.projectionField = (Core.Name "namespace")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

qualifiedNameWithNamespace :: (Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm (Maybe Pdl.Namespace) -> Phantoms.TTerm Pdl.QualifiedName)
qualifiedNameWithNamespace original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.QualifiedName"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

recordField :: (Phantoms.TTerm Pdl.FieldName -> Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Model.Value) -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.RecordField)
recordField name value optional default_ annotations = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)},
    Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Phantoms.unTTerm optional)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm annotations)}]})))

recordFieldName :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Pdl.FieldName)
recordFieldName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordFieldValue :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Pdl.Schema)
recordFieldValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordFieldOptional :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Bool)
recordFieldOptional x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
    Core.projectionField = (Core.Name "optional")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordFieldDefault :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm (Maybe Model.Value))
recordFieldDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordFieldAnnotations :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Pdl.Annotations)
recordFieldAnnotations x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
    Core.projectionField = (Core.Name "annotations")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordFieldWithName :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Pdl.FieldName -> Phantoms.TTerm Pdl.RecordField)
recordFieldWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "optional")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

recordFieldWithValue :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Pdl.RecordField)
recordFieldWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "optional")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

recordFieldWithOptional :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Bool -> Phantoms.TTerm Pdl.RecordField)
recordFieldWithOptional original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

recordFieldWithDefault :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm (Maybe Model.Value) -> Phantoms.TTerm Pdl.RecordField)
recordFieldWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "optional")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

recordFieldWithAnnotations :: (Phantoms.TTerm Pdl.RecordField -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.RecordField)
recordFieldWithAnnotations original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "optional"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "optional")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordField"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

recordSchema :: (Phantoms.TTerm [Pdl.RecordField] -> Phantoms.TTerm [Pdl.NamedSchema] -> Phantoms.TTerm Pdl.RecordSchema)
recordSchema fields includes = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Phantoms.unTTerm fields)},
    Core.Field {
      Core.fieldName = (Core.Name "includes"),
      Core.fieldTerm = (Phantoms.unTTerm includes)}]})))

recordSchemaFields :: (Phantoms.TTerm Pdl.RecordSchema -> Phantoms.TTerm [Pdl.RecordField])
recordSchemaFields x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
    Core.projectionField = (Core.Name "fields")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordSchemaIncludes :: (Phantoms.TTerm Pdl.RecordSchema -> Phantoms.TTerm [Pdl.NamedSchema])
recordSchemaIncludes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
    Core.projectionField = (Core.Name "includes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

recordSchemaWithFields :: (Phantoms.TTerm Pdl.RecordSchema -> Phantoms.TTerm [Pdl.RecordField] -> Phantoms.TTerm Pdl.RecordSchema)
recordSchemaWithFields original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "includes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
          Core.projectionField = (Core.Name "includes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

recordSchemaWithIncludes :: (Phantoms.TTerm Pdl.RecordSchema -> Phantoms.TTerm [Pdl.NamedSchema] -> Phantoms.TTerm Pdl.RecordSchema)
recordSchemaWithIncludes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fields"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.RecordSchema"),
          Core.projectionField = (Core.Name "fields")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "includes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

schemaArray :: (Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Pdl.Schema)
schemaArray x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "array"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaFixed :: (Phantoms.TTerm Int -> Phantoms.TTerm Pdl.Schema)
schemaFixed x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "fixed"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaInline :: (Phantoms.TTerm Pdl.NamedSchema -> Phantoms.TTerm Pdl.Schema)
schemaInline x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "inline"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaMap :: (Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Pdl.Schema)
schemaMap x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "map"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaNamed :: (Phantoms.TTerm Pdl.QualifiedName -> Phantoms.TTerm Pdl.Schema)
schemaNamed x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "named"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaNull :: (Phantoms.TTerm Pdl.Schema)
schemaNull = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "null"),
    Core.fieldTerm = Core.TermUnit}})))

schemaPrimitive :: (Phantoms.TTerm Pdl.PrimitiveType -> Phantoms.TTerm Pdl.Schema)
schemaPrimitive x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primitive"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaUnion :: (Phantoms.TTerm Pdl.UnionSchema -> Phantoms.TTerm Pdl.Schema)
schemaUnion x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.Schema"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "union"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

schemaFile :: (Phantoms.TTerm Pdl.Namespace -> Phantoms.TTerm (Maybe Pdl.Package) -> Phantoms.TTerm [Pdl.QualifiedName] -> Phantoms.TTerm [Pdl.NamedSchema] -> Phantoms.TTerm Pdl.SchemaFile)
schemaFile namespace package imports schemas = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Phantoms.unTTerm namespace)},
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm package)},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Phantoms.unTTerm imports)},
    Core.Field {
      Core.fieldName = (Core.Name "schemas"),
      Core.fieldTerm = (Phantoms.unTTerm schemas)}]})))

schemaFileNamespace :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm Pdl.Namespace)
schemaFileNamespace x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
    Core.projectionField = (Core.Name "namespace")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

schemaFilePackage :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm (Maybe Pdl.Package))
schemaFilePackage x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
    Core.projectionField = (Core.Name "package")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

schemaFileImports :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm [Pdl.QualifiedName])
schemaFileImports x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
    Core.projectionField = (Core.Name "imports")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

schemaFileSchemas :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm [Pdl.NamedSchema])
schemaFileSchemas x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
    Core.projectionField = (Core.Name "schemas")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

schemaFileWithNamespace :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm Pdl.Namespace -> Phantoms.TTerm Pdl.SchemaFile)
schemaFileWithNamespace original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemas"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "schemas")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

schemaFileWithPackage :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm (Maybe Pdl.Package) -> Phantoms.TTerm Pdl.SchemaFile)
schemaFileWithPackage original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "namespace")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemas"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "schemas")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

schemaFileWithImports :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm [Pdl.QualifiedName] -> Phantoms.TTerm Pdl.SchemaFile)
schemaFileWithImports original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "namespace")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "schemas"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "schemas")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

schemaFileWithSchemas :: (Phantoms.TTerm Pdl.SchemaFile -> Phantoms.TTerm [Pdl.NamedSchema] -> Phantoms.TTerm Pdl.SchemaFile)
schemaFileWithSchemas original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "namespace"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "namespace")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.SchemaFile"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemas"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unionMember :: (Phantoms.TTerm (Maybe Pdl.FieldName) -> Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.UnionMember)
unionMember alias value annotations = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Phantoms.unTTerm alias)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm annotations)}]})))

unionMemberAlias :: (Phantoms.TTerm Pdl.UnionMember -> Phantoms.TTerm (Maybe Pdl.FieldName))
unionMemberAlias x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
    Core.projectionField = (Core.Name "alias")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unionMemberValue :: (Phantoms.TTerm Pdl.UnionMember -> Phantoms.TTerm Pdl.Schema)
unionMemberValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unionMemberAnnotations :: (Phantoms.TTerm Pdl.UnionMember -> Phantoms.TTerm Pdl.Annotations)
unionMemberAnnotations x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
    Core.projectionField = (Core.Name "annotations")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unionMemberWithAlias :: (Phantoms.TTerm Pdl.UnionMember -> Phantoms.TTerm (Maybe Pdl.FieldName) -> Phantoms.TTerm Pdl.UnionMember)
unionMemberWithAlias original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unionMemberWithValue :: (Phantoms.TTerm Pdl.UnionMember -> Phantoms.TTerm Pdl.Schema -> Phantoms.TTerm Pdl.UnionMember)
unionMemberWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
          Core.projectionField = (Core.Name "alias")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
          Core.projectionField = (Core.Name "annotations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unionMemberWithAnnotations :: (Phantoms.TTerm Pdl.UnionMember -> Phantoms.TTerm Pdl.Annotations -> Phantoms.TTerm Pdl.UnionMember)
unionMemberWithAnnotations original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
          Core.projectionField = (Core.Name "alias")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionMember"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotations"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

unionSchema :: (Phantoms.TTerm [Pdl.UnionMember] -> Phantoms.TTerm Pdl.UnionSchema)
unionSchema x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.pegasus.pdl.UnionSchema"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unUnionSchema :: (Phantoms.TTerm Pdl.UnionSchema -> Phantoms.TTerm [Pdl.UnionMember])
unUnionSchema x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.pegasus.pdl.UnionSchema")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))
