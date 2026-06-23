-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.system

module Hydra.Encode.System where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.File as File
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.System as System
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.system.Command
command :: System.Command -> Core.Term
command x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.Command"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "program"),
          Core.fieldTerm = (File.filePath (System.commandProgram x))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) xs)) (System.commandArguments x))},
        Core.Field {
          Core.fieldName = (Core.Name "workingDirectory"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map File.filePath opt)) (System.commandWorkingDirectory x))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\m -> Core.TermMap (Maps.bimap environmentVariable (\x2 -> Core.TermLiteral (Core.LiteralString x2)) m)) opt)) (System.commandEnvironment x))}]})
-- | Encoder for hydra.system.EnvironmentVariable
environmentVariable :: System.EnvironmentVariable -> Core.Term
environmentVariable x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.system.EnvironmentVariable"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (System.unEnvironmentVariable x))})
-- | Encoder for hydra.system.ProcessResult
processResult :: System.ProcessResult -> Core.Term
processResult x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.ProcessResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exitCode"),
          Core.fieldTerm = (statusCode (System.processResultExitCode x))},
        Core.Field {
          Core.fieldName = (Core.Name "stdout"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBinary x2)) (System.processResultStdout x))},
        Core.Field {
          Core.fieldName = (Core.Name "stderr"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBinary x2)) (System.processResultStderr x))}]})
-- | Encoder for hydra.system.StatusCode
statusCode :: System.StatusCode -> Core.Term
statusCode x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.system.StatusCode"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (System.unStatusCode x))})
