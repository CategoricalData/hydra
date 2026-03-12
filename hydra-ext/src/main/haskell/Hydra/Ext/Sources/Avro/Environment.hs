-- | Type definitions for the Avro code generation environment.

module Hydra.Ext.Sources.Avro.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes

ns :: Namespace
ns = Namespace "hydra.ext.avro.environment"

define :: String -> Type -> Binding
define = defineType ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

localType :: String -> Type
localType = typeref ns

module_ :: Module
module_ = Module ns elements [] [CoreTypes.ns] $
    Just "Type definitions for the Avro code generation environment"
  where
    elements = [
      avroQualifiedNameType,
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

-- | The environment for Avro-to-Hydra code generation
avroEnvironmentType :: Binding
avroEnvironmentType = define "AvroEnvironment" $
  doc "Environment for Avro-to-Hydra code generation" $
  T.record [
    "namedAdapters" >:
      doc "Named adapters for previously processed schemas" $
      T.map (localType "AvroQualifiedName") T.string,
    "namespace" >:
      doc "The current Avro namespace" $
      T.optional T.string,
    "elements" >:
      doc "Generated Hydra elements" $
      T.map (coreType "Name") (coreType "Binding")]
