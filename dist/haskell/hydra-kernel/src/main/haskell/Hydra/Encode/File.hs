-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.file

module Hydra.Encode.File where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Time as Time
import qualified Hydra.File as File
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.file.FileExtension
fileExtension :: File.FileExtension -> Core.Term
fileExtension x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.file.FileExtension"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (File.unFileExtension x))})
-- | Encoder for hydra.file.FilePath
filePath :: File.FilePath -> Core.Term
filePath x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.file.FilePath"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (File.unFilePath x))})
-- | Encoder for hydra.file.FileStatus
fileStatus :: File.FileStatus -> Core.Term
fileStatus x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.file.FileStatus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fileType"),
          Core.fieldTerm = (fileType (File.fileStatusFileType x))},
        Core.Field {
          Core.fieldName = (Core.Name "size"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 x2))) (File.fileStatusSize x))},
        Core.Field {
          Core.fieldName = (Core.Name "modificationTime"),
          Core.fieldTerm = (Time.timespec (File.fileStatusModificationTime x))},
        Core.Field {
          Core.fieldName = (Core.Name "accessTime"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map Time.timespec opt)) (File.fileStatusAccessTime x))},
        Core.Field {
          Core.fieldName = (Core.Name "statusChangeTime"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map Time.timespec opt)) (File.fileStatusStatusChangeTime x))}]})
-- | Encoder for hydra.file.FileType
fileType :: File.FileType -> Core.Term
fileType x =
    case x of
      File.FileTypeBlock -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = Core.TermUnit}})
      File.FileTypeCharacter -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "character"),
          Core.fieldTerm = Core.TermUnit}})
      File.FileTypeDirectory -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = Core.TermUnit}})
      File.FileTypeFifo -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "fifo"),
          Core.fieldTerm = Core.TermUnit}})
      File.FileTypeRegular -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "regular"),
          Core.fieldTerm = Core.TermUnit}})
      File.FileTypeLink -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "link"),
          Core.fieldTerm = Core.TermUnit}})
      File.FileTypeSocket -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.file.FileType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "socket"),
          Core.fieldTerm = Core.TermUnit}})
