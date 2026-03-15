-- | Type definitions for the Avro code generation environment.

module Hydra.Ext.Sources.Avro.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.Sources.Kernel.Types.Util as UtilTypes

ns :: Namespace
ns = Namespace "hydra.ext.avro.environment"

computeType :: String -> Type
computeType = typeref UtilTypes.ns

avroSchemaType :: Type
avroSchemaType = typeref (Namespace "hydra.ext.org.apache.avro.schema") "Schema"

jsonValueType :: Type
jsonValueType = typeref (Namespace "hydra.json.model") "Value"

-- | AvroHydraAdapter = Adapter Avro.Schema Type Json.Value Term
avroHydraAdapterType :: Type
avroHydraAdapterType = T.apply (T.apply (T.apply (T.apply (computeType "Adapter") avroSchemaType) (coreType "Type")) jsonValueType) (coreType "Term")

define :: String -> Type -> Binding
define = defineType ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

localType :: String -> Type
localType = typeref ns

module_ :: Module
module_ = Module ns elements []
    [CoreTypes.ns, UtilTypes.ns, Namespace "hydra.ext.org.apache.avro.schema", Namespace "hydra.json.model"] $
    Just "Type definitions for the Avro code generation environment"
  where
    elements = [
      avroQualifiedNameType,
      avroForeignKeyType,
      avroPrimaryKeyType,
      avroEnvironmentType]

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
