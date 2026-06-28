-- | Type definitions for the Avro code generation environment.

module Hydra.Sources.Avro.Environment where

import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.Sources.Kernel.Types.Coders as Coders
import qualified Hydra.Sources.Kernel.Types.Errors as Errors

ns :: ModuleName
ns = ModuleName "hydra.avro.environment"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [CoreTypes.ns, Coders.ns, Errors.ns, ModuleName "hydra.avro.schema", ModuleName "hydra.json.model"],
            moduleMetadata = descriptionMetadata (Just "Type definitions for the Avro code generation environment")}
  where
    definitions = [
      avroQualifiedNameType,
      avroForeignKeyType,
      avroPrimaryKeyType,
      avroEnvironmentType,
      encodeEnvironmentType]

-- | The environment for Avro-to-Hydra code generation
avroEnvironmentType :: TypeDefinition
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

-- | An Avro foreign key annotation
avroForeignKeyType :: TypeDefinition
avroForeignKeyType = define "AvroForeignKey" $
  doc "An Avro foreign key annotation linking a field to another type" $
  T.record [
    "typeName" >:
      doc "The Hydra type name referenced by this foreign key" $
      coreType "Name",
    "constructor" >:
      doc "A function which constructs element names from string values" $
      T.function T.string (coreType "Name")]

-- | AvroHydraAdapter = Adapter Avro.Schema Type Json.Value Term Error
avroHydraAdapterType :: Type
avroHydraAdapterType = T.apply (T.apply (T.apply (T.apply (T.apply (computeType "Adapter") avroSchemaType) (coreType "Type")) jsonValueType) (coreType "Term")) (errorsType "Error")

-- | An Avro primary key annotation
avroPrimaryKeyType :: TypeDefinition
avroPrimaryKeyType = define "AvroPrimaryKey" $
  doc "An Avro primary key annotation identifying the element name field" $
  T.record [
    "fieldName" >:
      doc "The name of the primary key field" $
      coreType "Name",
    "constructor" >:
      doc "A function which constructs element names from string values" $
      T.function T.string (coreType "Name")]

-- | An Avro qualified name with optional namespace
avroQualifiedNameType :: TypeDefinition
avroQualifiedNameType = define "AvroQualifiedName" $
  doc "An Avro qualified name with optional namespace" $
  T.record [
    "namespace" >:
      doc "The optional namespace" $
      T.optional T.string,
    "name" >:
      doc "The local name" $
      T.string]

avroSchemaType :: Type
avroSchemaType = typeref (ModuleName "hydra.avro.schema") "Schema"

computeType :: String -> Type
computeType = typeref Coders.ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

errorsType :: String -> Type
errorsType = typeref Errors.ns

-- | Environment for encoding Hydra types to Avro schemas
encodeEnvironmentType :: TypeDefinition
encodeEnvironmentType = define "EncodeEnvironment" $
  doc "Environment for Hydra-to-Avro encoding, tracking which named types have been emitted" $
  T.record [
    "typeMap" >:
      doc "All named types available for reference" $
      T.map (coreType "Name") (coreType "Type"),
    "emitted" >:
      doc "Adapters for types that have already been fully emitted (emit references for these)" $
      T.map (coreType "Name") hydraAvroAdapterType]

-- | HydraAvroAdapter = Adapter Type Avro.Schema Term Json.Value Error (reverse direction)
hydraAvroAdapterType :: Type
hydraAvroAdapterType = T.apply (T.apply (T.apply (T.apply (T.apply (computeType "Adapter") (coreType "Type")) avroSchemaType) (coreType "Term")) jsonValueType) (errorsType "Error")

jsonValueType :: Type
jsonValueType = typeref (ModuleName "hydra.json.model") "Value"

localType :: String -> Type
localType = typeref ns
