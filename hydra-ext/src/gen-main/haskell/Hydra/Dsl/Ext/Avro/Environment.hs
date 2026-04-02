-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.avro.environment

module Hydra.Dsl.Ext.Avro.Environment where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Avro.Environment as Environment
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Schema
import qualified Hydra.Json.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

avroEnvironment :: Phantoms.TTerm (M.Map Environment.AvroQualifiedName (Coders.Adapter Schema.Schema Core.Type Model.Value Core.Term)) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (M.Map Core.Name Core.Binding) -> Phantoms.TTerm Environment.AvroEnvironment
avroEnvironment namedAdapters namespace elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namedAdapters"),
          Core.fieldTerm = (Phantoms.unTTerm namedAdapters)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

avroEnvironmentElements :: Phantoms.TTerm Environment.AvroEnvironment -> Phantoms.TTerm (M.Map Core.Name Core.Binding)
avroEnvironmentElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroEnvironmentNamedAdapters :: Phantoms.TTerm Environment.AvroEnvironment -> Phantoms.TTerm (M.Map Environment.AvroQualifiedName (Coders.Adapter Schema.Schema Core.Type Model.Value Core.Term))
avroEnvironmentNamedAdapters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
        Core.projectionField = (Core.Name "namedAdapters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroEnvironmentNamespace :: Phantoms.TTerm Environment.AvroEnvironment -> Phantoms.TTerm (Maybe String)
avroEnvironmentNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroEnvironmentWithElements :: Phantoms.TTerm Environment.AvroEnvironment -> Phantoms.TTerm (M.Map Core.Name Core.Binding) -> Phantoms.TTerm Environment.AvroEnvironment
avroEnvironmentWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namedAdapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
              Core.projectionField = (Core.Name "namedAdapters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

avroEnvironmentWithNamedAdapters :: Phantoms.TTerm Environment.AvroEnvironment -> Phantoms.TTerm (M.Map Environment.AvroQualifiedName (Coders.Adapter Schema.Schema Core.Type Model.Value Core.Term)) -> Phantoms.TTerm Environment.AvroEnvironment
avroEnvironmentWithNamedAdapters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namedAdapters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

avroEnvironmentWithNamespace :: Phantoms.TTerm Environment.AvroEnvironment -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Environment.AvroEnvironment
avroEnvironmentWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namedAdapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
              Core.projectionField = (Core.Name "namedAdapters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroEnvironment"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

avroForeignKey :: Phantoms.TTerm Core.Name -> Phantoms.TTerm (String -> Core.Name) -> Phantoms.TTerm Environment.AvroForeignKey
avroForeignKey typeName constructor =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm constructor)}]}))

avroForeignKeyConstructor :: Phantoms.TTerm Environment.AvroForeignKey -> Phantoms.TTerm (String -> Core.Name)
avroForeignKeyConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
        Core.projectionField = (Core.Name "constructor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroForeignKeyTypeName :: Phantoms.TTerm Environment.AvroForeignKey -> Phantoms.TTerm Core.Name
avroForeignKeyTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroForeignKeyWithConstructor :: Phantoms.TTerm Environment.AvroForeignKey -> Phantoms.TTerm (String -> Core.Name) -> Phantoms.TTerm Environment.AvroForeignKey
avroForeignKeyWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

avroForeignKeyWithTypeName :: Phantoms.TTerm Environment.AvroForeignKey -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Environment.AvroForeignKey
avroForeignKeyWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroForeignKey"),
              Core.projectionField = (Core.Name "constructor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

avroPrimaryKey :: Phantoms.TTerm Core.Name -> Phantoms.TTerm (String -> Core.Name) -> Phantoms.TTerm Environment.AvroPrimaryKey
avroPrimaryKey fieldName constructor =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm constructor)}]}))

avroPrimaryKeyConstructor :: Phantoms.TTerm Environment.AvroPrimaryKey -> Phantoms.TTerm (String -> Core.Name)
avroPrimaryKeyConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
        Core.projectionField = (Core.Name "constructor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroPrimaryKeyFieldName :: Phantoms.TTerm Environment.AvroPrimaryKey -> Phantoms.TTerm Core.Name
avroPrimaryKeyFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
        Core.projectionField = (Core.Name "fieldName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroPrimaryKeyWithConstructor :: Phantoms.TTerm Environment.AvroPrimaryKey -> Phantoms.TTerm (String -> Core.Name) -> Phantoms.TTerm Environment.AvroPrimaryKey
avroPrimaryKeyWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
              Core.projectionField = (Core.Name "fieldName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

avroPrimaryKeyWithFieldName :: Phantoms.TTerm Environment.AvroPrimaryKey -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Environment.AvroPrimaryKey
avroPrimaryKeyWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey"),
              Core.projectionField = (Core.Name "constructor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

avroQualifiedName :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm String -> Phantoms.TTerm Environment.AvroQualifiedName
avroQualifiedName namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

avroQualifiedNameName :: Phantoms.TTerm Environment.AvroQualifiedName -> Phantoms.TTerm String
avroQualifiedNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroQualifiedNameNamespace :: Phantoms.TTerm Environment.AvroQualifiedName -> Phantoms.TTerm (Maybe String)
avroQualifiedNameNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

avroQualifiedNameWithName :: Phantoms.TTerm Environment.AvroQualifiedName -> Phantoms.TTerm String -> Phantoms.TTerm Environment.AvroQualifiedName
avroQualifiedNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

avroQualifiedNameWithNamespace :: Phantoms.TTerm Environment.AvroQualifiedName -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Environment.AvroQualifiedName
avroQualifiedNameWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

encodeEnvironment :: Phantoms.TTerm (M.Map Core.Name Core.Type) -> Phantoms.TTerm (M.Map Core.Name (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value)) -> Phantoms.TTerm Environment.EncodeEnvironment
encodeEnvironment typeMap emitted =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeMap"),
          Core.fieldTerm = (Phantoms.unTTerm typeMap)},
        Core.Field {
          Core.fieldName = (Core.Name "emitted"),
          Core.fieldTerm = (Phantoms.unTTerm emitted)}]}))

encodeEnvironmentEmitted :: Phantoms.TTerm Environment.EncodeEnvironment -> Phantoms.TTerm (M.Map Core.Name (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value))
encodeEnvironmentEmitted x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
        Core.projectionField = (Core.Name "emitted")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

encodeEnvironmentTypeMap :: Phantoms.TTerm Environment.EncodeEnvironment -> Phantoms.TTerm (M.Map Core.Name Core.Type)
encodeEnvironmentTypeMap x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
        Core.projectionField = (Core.Name "typeMap")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

encodeEnvironmentWithEmitted :: Phantoms.TTerm Environment.EncodeEnvironment -> Phantoms.TTerm (M.Map Core.Name (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value)) -> Phantoms.TTerm Environment.EncodeEnvironment
encodeEnvironmentWithEmitted original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
              Core.projectionField = (Core.Name "typeMap")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "emitted"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

encodeEnvironmentWithTypeMap :: Phantoms.TTerm Environment.EncodeEnvironment -> Phantoms.TTerm (M.Map Core.Name Core.Type) -> Phantoms.TTerm Environment.EncodeEnvironment
encodeEnvironmentWithTypeMap original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeMap"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "emitted"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.avro.environment.EncodeEnvironment"),
              Core.projectionField = (Core.Name "emitted")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
