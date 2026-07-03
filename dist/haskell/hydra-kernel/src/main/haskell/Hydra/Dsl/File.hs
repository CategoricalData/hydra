-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.file

module Hydra.Dsl.File where

import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.File as File
import qualified Hydra.Time as Time
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I

-- | DSL constructor for the hydra.file.FileExtension wrapper
fileExtension :: Typed.TypedTerm String -> Typed.TypedTerm File.FileExtension
fileExtension x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.file.FileExtension"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))

-- | DSL constructor for the hydra.file.FilePath wrapper
filePath :: Typed.TypedTerm String -> Typed.TypedTerm File.FilePath
filePath x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.file.FilePath"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))

-- | DSL constructor for hydra.file.FileStatus
fileStatus :: Typed.TypedTerm File.FileType -> Typed.TypedTerm I.Int64 -> Typed.TypedTerm Time.Timespec -> Typed.TypedTerm (Maybe Time.Timespec) -> Typed.TypedTerm (Maybe Time.Timespec) -> Typed.TypedTerm File.FileStatus
fileStatus fileType size modificationTime accessTime statusChangeTime =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (Typed.unTypedTerm fileType)},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Typed.unTypedTerm size)},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Typed.unTypedTerm modificationTime)},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = (Typed.unTypedTerm accessTime)},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = (Typed.unTypedTerm statusChangeTime)}]}))

-- | DSL accessor for the accessTime field of hydra.file.FileStatus
fileStatusAccessTime :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm (Maybe Time.Timespec)
fileStatusAccessTime x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
        Core.projectionFieldName = (Core.Name "accessTime")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the fileType field of hydra.file.FileStatus
fileStatusFileType :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm File.FileType
fileStatusFileType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
        Core.projectionFieldName = (Core.Name "fileType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the modificationTime field of hydra.file.FileStatus
fileStatusModificationTime :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm Time.Timespec
fileStatusModificationTime x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
        Core.projectionFieldName = (Core.Name "modificationTime")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the size field of hydra.file.FileStatus
fileStatusSize :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm I.Int64
fileStatusSize x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
        Core.projectionFieldName = (Core.Name "size")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the statusChangeTime field of hydra.file.FileStatus
fileStatusStatusChangeTime :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm (Maybe Time.Timespec)
fileStatusStatusChangeTime x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
        Core.projectionFieldName = (Core.Name "statusChangeTime")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL updater for the accessTime field of hydra.file.FileStatus
fileStatusWithAccessTime :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm (Maybe Time.Timespec) -> Typed.TypedTerm File.FileStatus
fileStatusWithAccessTime original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "fileType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "size")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "modificationTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "statusChangeTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the fileType field of hydra.file.FileStatus
fileStatusWithFileType :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm File.FileType -> Typed.TypedTerm File.FileStatus
fileStatusWithFileType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "size")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "modificationTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "accessTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "statusChangeTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the modificationTime field of hydra.file.FileStatus
fileStatusWithModificationTime :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm Time.Timespec -> Typed.TypedTerm File.FileStatus
fileStatusWithModificationTime original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "fileType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "size")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "accessTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "statusChangeTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the size field of hydra.file.FileStatus
fileStatusWithSize :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm I.Int64 -> Typed.TypedTerm File.FileStatus
fileStatusWithSize original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "fileType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "modificationTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "accessTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "statusChangeTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the statusChangeTime field of hydra.file.FileStatus
fileStatusWithStatusChangeTime :: Typed.TypedTerm File.FileStatus -> Typed.TypedTerm (Maybe Time.Timespec) -> Typed.TypedTerm File.FileStatus
fileStatusWithStatusChangeTime original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "fileType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "size")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "modificationTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.file.FileStatus"),
              Core.projectionFieldName = (Core.Name "accessTime")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL injection for the block variant of hydra.file.FileType
fileTypeBlock :: Typed.TypedTerm File.FileType
fileTypeBlock =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the character variant of hydra.file.FileType
fileTypeCharacter :: Typed.TypedTerm File.FileType
fileTypeCharacter =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the directory variant of hydra.file.FileType
fileTypeDirectory :: Typed.TypedTerm File.FileType
fileTypeDirectory =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "directory"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the fifo variant of hydra.file.FileType
fileTypeFifo :: Typed.TypedTerm File.FileType
fileTypeFifo =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fifo"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the link variant of hydra.file.FileType
fileTypeLink :: Typed.TypedTerm File.FileType
fileTypeLink =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "link"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the regular variant of hydra.file.FileType
fileTypeRegular :: Typed.TypedTerm File.FileType
fileTypeRegular =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regular"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL injection for the socket variant of hydra.file.FileType
fileTypeSocket :: Typed.TypedTerm File.FileType
fileTypeSocket =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "socket"),
        Core.fieldTerm = Core.TermUnit}}))

-- | DSL accessor for the body of hydra.file.FileExtension
unFileExtension :: Typed.TypedTerm File.FileExtension -> Typed.TypedTerm String
unFileExtension x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.file.FileExtension")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the body of hydra.file.FilePath
unFilePath :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm String
unFilePath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.file.FilePath")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
