-- | Type definitions for the Protobuf code generation environment.

module Hydra.Sources.Protobuf.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                    ((>:))
import qualified Hydra.Dsl.Types                    as T
import qualified Hydra.Sources.Kernel.Types.Core    as CoreTypes
import qualified Hydra.Sources.Kernel.Types.Typing  as TypingTypes

typingType :: String -> Type
typingType = typeref TypingTypes.ns

ns :: ModuleName
ns = ModuleName "hydra.protobuf.environment"

define :: String -> Type -> Binding
define = defineType ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [CoreTypes.ns, TypingTypes.ns],
            moduleDescription = Just "Type definitions for the Protobuf code generation environment"}
  where
    definitions = [
      encoderStateType,
      structuralTypeRefType]

-- | State threaded through Protobuf encoding: the InferenceContext used by
-- kernel helpers (annotation reads, dependency analysis) plus a per-message
-- field-index counter.
encoderStateType :: Binding
encoderStateType = define "EncoderState" $
  doc "State threaded through Protobuf encoding: inference context plus field-index counter" $
  T.record [
    "context" >:
      doc "Inference context used by kernel utilities (annotations, dependency analysis)" $
      typingType "InferenceContext",
    "fieldIndex" >:
      doc "Counter used to assign the next Protobuf field index within the current message" $
      T.int32]

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
