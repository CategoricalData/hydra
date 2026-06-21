-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.file

module Hydra.Decode.File where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Time as Time
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.file.FileExtension
fileExtension :: Graph.Graph -> Core.Term -> Either Errors.DecodingError File.FileExtension
fileExtension cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> File.FileExtension b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.file.FilePath
filePath :: Graph.Graph -> Core.Term -> Either Errors.DecodingError File.FilePath
filePath cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> File.FilePath b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.file.FileStatus
fileStatus :: Graph.Graph -> Core.Term -> Either Errors.DecodingError File.FileStatus
fileStatus cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "fileType" fileType fieldMap cx) (\field_fileType -> Eithers.bind (ExtractCore.requireField "size" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int64 value")
            _ -> Left (Errors.DecodingError "expected int64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_size -> Eithers.bind (ExtractCore.requireField "modificationTime" Time.timespec fieldMap cx) (\field_modificationTime -> Eithers.bind (ExtractCore.requireField "accessTime" (ExtractCore.decodeMaybe Time.timespec) fieldMap cx) (\field_accessTime -> Eithers.bind (ExtractCore.requireField "statusChangeTime" (ExtractCore.decodeMaybe Time.timespec) fieldMap cx) (\field_statusChangeTime -> Right (File.FileStatus {
          File.fileStatusFileType = field_fileType,
          File.fileStatusSize = field_size,
          File.fileStatusModificationTime = field_modificationTime,
          File.fileStatusAccessTime = field_accessTime,
          File.fileStatusStatusChangeTime = field_statusChangeTime})))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.file.FileStatus")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.file.FileType
fileType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError File.FileType
fileType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "block", (\input -> Eithers.map (\t -> File.FileTypeBlock) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "character", (\input -> Eithers.map (\t -> File.FileTypeCharacter) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "directory", (\input -> Eithers.map (\t -> File.FileTypeDirectory) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "fifo", (\input -> Eithers.map (\t -> File.FileTypeFifo) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "regular", (\input -> Eithers.map (\t -> File.FileTypeRegular) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "link", (\input -> Eithers.map (\t -> File.FileTypeLink) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "socket", (\input -> Eithers.map (\t -> File.FileTypeSocket) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
