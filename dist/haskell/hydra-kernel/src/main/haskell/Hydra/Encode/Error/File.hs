-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.error.file

module Hydra.Encode.Error.File where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.File as EncodeFile
import qualified Hydra.Error.File as ErrorFile
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.error.file.FileError
fileError :: ErrorFile.FileError -> Core.Term
fileError x =
    case x of
      ErrorFile.FileErrorAlreadyExists v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "alreadyExists"),
          Core.fieldTerm = (EncodeFile.filePath v0)}})
      ErrorFile.FileErrorInvalidPath v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "invalidPath"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
      ErrorFile.FileErrorNotFound v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "notFound"),
          Core.fieldTerm = (EncodeFile.filePath v0)}})
      ErrorFile.FileErrorOther v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
      ErrorFile.FileErrorPermissionDenied v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "permissionDenied"),
          Core.fieldTerm = (EncodeFile.filePath v0)}})
