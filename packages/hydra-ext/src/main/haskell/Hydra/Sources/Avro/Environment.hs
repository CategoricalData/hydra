-- | Type definitions for the Avro code generation environment.

module Hydra.Sources.Avro.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.Sources.Kernel.Types.Coders as CodersTypes

ns :: Namespace
ns = Namespace "hydra.avro.environment"

computeType :: String -> Type
computeType = typeref CodersTypes.ns

avroSchemaType :: Type
avroSchemaType = typeref (Namespace "hydra.avro.schema") "Schema"

jsonValueType :: Type
jsonValueType = typeref (Namespace "hydra.json.model") "Value"

-- | AvroHydraAdapter = Adapter Avro.Schema Type Json.Value Term
avroHydraAdapterType :: Type
avroHydraAdapterType = T.apply (T.apply (T.apply (T.apply (computeType "Adapter") avroSchemaType) (coreType "Type")) jsonValueType) (coreType "Term")

-- | HydraAvroAdapter = Adapter Type Avro.Schema Term Json.Value (reverse direction)
hydraAvroAdapterType :: Type
hydraAvroAdapterType = T.apply (T.apply (T.apply (T.apply (computeType "Adapter") (coreType "Type")) avroSchemaType) (coreType "Term")) jsonValueType

define :: String -> Type -> Binding
define = defineType ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

localType :: String -> Type
localType = typeref ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [],
            moduleTypeDependencies = [CoreTypes.ns, CodersTypes.ns, Namespace "hydra.avro.schema", Namespace "hydra.json.model"],
            moduleDescription = Just "Type definitions for the Avro code generation environment"}
  where
    definitions = [
      avroQualifiedNameType,
      avroForeignKeyType,
      avroPrimaryKeyType,
      avroEnvironmentType,
      encodeEnvironmentType]

-- | An Avro qualified name with optional namespace
avroQualifiedNameType :: Binding
avroQualifiedNameType = define "AvroQualifiedName" $
  doc "An Avro qualified name with optional namespace" $
  T.record [
    "namespace" >:
      doc "The optional namespace" $
      T.optional T.string,
    "name" >:
      doc "The local name" $
      T.string]

-- | An Avro foreign key annotation
avroForeignKeyType :: Binding
avroForeignKeyType = define "AvroForeignKey" $
  doc "An Avro foreign key annotation linking a field to another type" $
  T.record [
    "typeName" >:
      doc "The Hydra type name referenced by this foreign key" $
      coreType "Name",
    "constructor" >:
      doc "A function which constructs element names from string values" $
      T.function T.string (coreType "Name")]

-- | An Avro primary key annotation
avroPrimaryKeyType :: Binding
avroPrimaryKeyType = define "AvroPrimaryKey" $
  doc "An Avro primary key annotation identifying the element name field" $
  T.record [
    "fieldName" >:
      doc "The name of the primary key field" $
      coreType "Name",
    "constructor" >:
      doc "A function which constructs element names from string values" $
      T.function T.string (coreType "Name")]

-- | The environment for Avro-to-Hydra code generation
avroEnvironmentType :: Binding
avroEnvironmentType = define "AvroEnvironment" $
  doc "Environment for Avro-to-Hydra code generation" $
  T.record [
    "namedAdapters" >:
      doc "Named adapters for previously processed schemas" $
      T.map (localType "AvroQualifiedName") avroHydraAdapterType,
    "namespace" >:
      doc "The current Avro namespace" $
      T.optional T.string,
    "elements" >:
      doc "Generated Hydra elements" $
      T.map (coreType "Name") (coreType "Binding")]

-- | Environment for encoding Hydra types to Avro schemas
encodeEnvironmentType :: Binding
encodeEnvironmentType = define "EncodeEnvironment" $
  doc "Environment for Hydra-to-Avro encoding, tracking which named types have been emitted" $
  T.record [
    "typeMap" >:
      doc "All named types available for reference" $
      T.map (coreType "Name") (coreType "Type"),
    "emitted" >:
      doc "Adapters for types that have already been fully emitted (emit references for these)" $
      T.map (coreType "Name") hydraAvroAdapterType]
