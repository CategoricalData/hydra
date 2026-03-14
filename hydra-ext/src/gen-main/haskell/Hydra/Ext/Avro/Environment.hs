-- Note: this is an automatically generated file. Do not edit.

-- | Type definitions for the Avro code generation environment

module Hydra.Ext.Avro.Environment where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Schema
import qualified Hydra.Json.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An Avro qualified name with optional namespace
data AvroQualifiedName = 
  AvroQualifiedName {
    -- | The optional namespace
    avroQualifiedNameNamespace :: (Maybe String),
    -- | The local name
    avroQualifiedNameName :: String}
  deriving (Eq, Ord, Read, Show)

_AvroQualifiedName = (Core.Name "hydra.ext.avro.environment.AvroQualifiedName")

_AvroQualifiedName_namespace = (Core.Name "namespace")

_AvroQualifiedName_name = (Core.Name "name")

-- | An Avro foreign key annotation linking a field to another type
data AvroForeignKey = 
  AvroForeignKey {
    -- | The Hydra type name referenced by this foreign key
    avroForeignKeyTypeName :: Core.Name,
    -- | A function which constructs element names from string values
    avroForeignKeyConstructor :: (String -> Core.Name)}

_AvroForeignKey = (Core.Name "hydra.ext.avro.environment.AvroForeignKey")

_AvroForeignKey_typeName = (Core.Name "typeName")

_AvroForeignKey_constructor = (Core.Name "constructor")

-- | An Avro primary key annotation identifying the element name field
data AvroPrimaryKey = 
  AvroPrimaryKey {
    -- | The name of the primary key field
    avroPrimaryKeyFieldName :: Core.Name,
    -- | A function which constructs element names from string values
    avroPrimaryKeyConstructor :: (String -> Core.Name)}

_AvroPrimaryKey = (Core.Name "hydra.ext.avro.environment.AvroPrimaryKey")

_AvroPrimaryKey_fieldName = (Core.Name "fieldName")

_AvroPrimaryKey_constructor = (Core.Name "constructor")

-- | Environment for Avro-to-Hydra code generation
data AvroEnvironment = 
  AvroEnvironment {
    -- | Named adapters for previously processed schemas
    avroEnvironmentNamedAdapters :: (M.Map AvroQualifiedName (Compute.Adapter Schema.Schema Core.Type Model.Value Core.Term)),
    -- | The current Avro namespace
    avroEnvironmentNamespace :: (Maybe String),
    -- | Generated Hydra elements
    avroEnvironmentElements :: (M.Map Core.Name Core.Binding)}

_AvroEnvironment = (Core.Name "hydra.ext.avro.environment.AvroEnvironment")

_AvroEnvironment_namedAdapters = (Core.Name "namedAdapters")

_AvroEnvironment_namespace = (Core.Name "namespace")

_AvroEnvironment_elements = (Core.Name "elements")
