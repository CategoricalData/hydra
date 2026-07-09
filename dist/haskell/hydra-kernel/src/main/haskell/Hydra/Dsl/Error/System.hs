-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error.system

module Hydra.Dsl.Error.System where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Error.System as DecodeErrorSystem
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Encode.Error.System as EncodeErrorSystem
import qualified Hydra.Error.System as System
import qualified Hydra.File as File
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL injection for the commandNotFound variant of hydra.error.system.SystemError
systemErrorCommandNotFound :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm System.SystemError
systemErrorCommandNotFound x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "commandNotFound"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))

-- | DSL injection for the interrupted variant of hydra.error.system.SystemError
systemErrorInterrupted :: Typed.TypedTerm System.SystemError
systemErrorInterrupted =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interrupted"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the invalidWorkingDirectory variant of hydra.error.system.SystemError
systemErrorInvalidWorkingDirectory :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm System.SystemError
systemErrorInvalidWorkingDirectory x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidWorkingDirectory"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))

-- | DSL injection for the other variant of hydra.error.system.SystemError
systemErrorOther :: Typed.TypedTerm String -> Typed.TypedTerm System.SystemError
systemErrorOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))

-- | DSL injection for the permissionDenied variant of hydra.error.system.SystemError
systemErrorPermissionDenied :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm System.SystemError
systemErrorPermissionDenied x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "permissionDenied"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))

-- | DSL name token for hydra.error.system.SystemError
systemErrorSystemError :: Typed.TypedName System.SystemError
systemErrorSystemError = Typed.TypedName (Core.Name "hydra.error.system.SystemError")
