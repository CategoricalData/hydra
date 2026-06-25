-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.error.system

module Hydra.Encode.Error.System where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.File as File
import qualified Hydra.Error.System as System
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.error.system.SystemError
systemError :: System.SystemError -> Core.Term
systemError x =
    case x of
      System.SystemErrorCommandNotFound v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "commandNotFound"),
          Core.fieldTerm = (File.filePath v0)}})
      System.SystemErrorPermissionDenied v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "permissionDenied"),
          Core.fieldTerm = (File.filePath v0)}})
      System.SystemErrorInvalidWorkingDirectory v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidWorkingDirectory"),
          Core.fieldTerm = (File.filePath v0)}})
      System.SystemErrorInterrupted -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "interrupted"),
          Core.fieldTerm = Core.TermUnit}})
      System.SystemErrorOther v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.system.SystemError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
