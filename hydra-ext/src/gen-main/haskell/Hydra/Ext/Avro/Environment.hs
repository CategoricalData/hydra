-- Note: this is an automatically generated file. Do not edit.

-- | Type definitions for the Avro code generation environment

module Hydra.Ext.Avro.Environment where

import qualified Hydra.Core as Core
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

-- | Environment for Avro-to-Hydra code generation
data AvroEnvironment = 
  AvroEnvironment {
    -- | Named adapters for previously processed schemas
    avroEnvironmentNamedAdapters :: (M.Map AvroQualifiedName String),
    -- | The current Avro namespace
    avroEnvironmentNamespace :: (Maybe String),
    -- | Generated Hydra elements
    avroEnvironmentElements :: (M.Map Core.Name Core.Binding)}
  deriving (Eq, Ord, Read, Show)

_AvroEnvironment = (Core.Name "hydra.ext.avro.environment.AvroEnvironment")

_AvroEnvironment_namedAdapters = (Core.Name "namedAdapters")

_AvroEnvironment_namespace = (Core.Name "namespace")

_AvroEnvironment_elements = (Core.Name "elements")
