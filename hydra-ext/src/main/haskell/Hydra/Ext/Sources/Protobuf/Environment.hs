-- | Type definitions for the Protobuf code generation environment.

module Hydra.Ext.Sources.Protobuf.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes

ns :: Namespace
ns = Namespace "hydra.ext.protobuf.environment"

define :: String -> Type -> Binding
define = defineType ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

module_ :: Module
module_ = Module ns (map toTypeDef elements) [] [CoreTypes.ns] $
    Just "Type definitions for the Protobuf code generation environment"
  where
    elements = [
      structuralTypeRefType]

-- | A reference to a structural type (Either or Pair) with its component types
structuralTypeRefType :: Binding
structuralTypeRefType = define "StructuralTypeRef" $
  doc "A reference to a structural type (Either or Pair) with its component types" $
  T.union [
    "either" >:
      doc "An Either type with left and right component types" $
      T.pair (coreType "Type") (coreType "Type"),
    "pair" >:
      doc "A Pair type with first and second component types" $
      T.pair (coreType "Type") (coreType "Type")]
