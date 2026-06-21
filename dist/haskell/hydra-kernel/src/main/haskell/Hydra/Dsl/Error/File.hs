-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.file

module Hydra.Dsl.Error.File where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.File as File
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the alreadyExists variant of hydra.error.file.FileError
fileErrorAlreadyExists :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm ErrorFile.FileError
fileErrorAlreadyExists x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alreadyExists"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidPath variant of hydra.error.file.FileError
fileErrorInvalidPath :: Typed.TypedTerm String -> Typed.TypedTerm ErrorFile.FileError
fileErrorInvalidPath x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidPath"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notFound variant of hydra.error.file.FileError
fileErrorNotFound :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm ErrorFile.FileError
fileErrorNotFound x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notFound"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.error.file.FileError
fileErrorOther :: Typed.TypedTerm String -> Typed.TypedTerm ErrorFile.FileError
fileErrorOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the permissionDenied variant of hydra.error.file.FileError
fileErrorPermissionDenied :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm ErrorFile.FileError
fileErrorPermissionDenied x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.file.FileError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "permissionDenied"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
