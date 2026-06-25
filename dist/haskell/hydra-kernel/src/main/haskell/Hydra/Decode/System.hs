-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.system

module Hydra.Decode.System where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.File as File
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.System as System
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.system.Command
command :: Graph.Graph -> Core.Term -> Either Errors.DecodingError System.Command
command cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "program" File.filePath fieldMap cx) (\field_program -> Eithers.bind (ExtractCore.requireField "arguments" (ExtractCore.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_arguments -> Eithers.bind (ExtractCore.requireField "workingDirectory" (ExtractCore.decodeMaybe File.filePath) fieldMap cx) (\field_workingDirectory -> Eithers.bind (ExtractCore.requireField "environment" (ExtractCore.decodeMaybe (ExtractCore.decodeMap environmentVariable (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)))) fieldMap cx) (\field_environment -> Right (System.Command {
          System.commandProgram = field_program,
          System.commandArguments = field_arguments,
          System.commandWorkingDirectory = field_workingDirectory,
          System.commandEnvironment = field_environment}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.system.Command")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.system.EnvironmentVariable
environmentVariable :: Graph.Graph -> Core.Term -> Either Errors.DecodingError System.EnvironmentVariable
environmentVariable cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> System.EnvironmentVariable b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.system.ProcessResult
processResult :: Graph.Graph -> Core.Term -> Either Errors.DecodingError System.ProcessResult
processResult cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "exitCode" statusCode fieldMap cx) (\field_exitCode -> Eithers.bind (ExtractCore.requireField "stdout" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBinary v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected binary literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_stdout -> Eithers.bind (ExtractCore.requireField "stderr" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBinary v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected binary literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_stderr -> Right (System.ProcessResult {
          System.processResultExitCode = field_exitCode,
          System.processResultStdout = field_stdout,
          System.processResultStderr = field_stderr})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.system.ProcessResult")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.system.StatusCode
statusCode :: Graph.Graph -> Core.Term -> Either Errors.DecodingError System.StatusCode
statusCode cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> System.StatusCode b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralInteger v2 -> case v2 of
            Core.IntegerValueInt32 v3 -> Right v3
            _ -> Left (Errors.DecodingError "expected int32 value")
          _ -> Left (Errors.DecodingError "expected int32 literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
