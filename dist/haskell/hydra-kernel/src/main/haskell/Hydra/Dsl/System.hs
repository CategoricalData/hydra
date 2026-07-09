-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.system

module Hydra.Dsl.System where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.System as DecodeSystem
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Encode.System as EncodeSystem
import qualified Hydra.File as File
import qualified Hydra.System as System
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Map as M

-- | DSL constructor for hydra.system.Command
command :: Typed.TypedTerm File.FilePath -> Typed.TypedTerm [String] -> Typed.TypedTerm (Maybe File.FilePath) -> Typed.TypedTerm (Maybe (M.Map System.EnvironmentVariable String)) -> Typed.TypedTerm System.Command
command program arguments workingDirectory environment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.Command"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "program"),
          Core.fieldTerm = (Typed.unTypedTerm program)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "workingDirectory"),
          Core.fieldTerm = (Typed.unTypedTerm workingDirectory)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Typed.unTypedTerm environment)}]}))

-- | DSL accessor for the arguments field of hydra.system.Command
commandArguments :: Typed.TypedTerm System.Command -> Typed.TypedTerm [String]
commandArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.Command"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.system.Command
commandCommand :: Typed.TypedName System.Command
commandCommand = Typed.TypedName (Core.Name "hydra.system.Command")

-- | DSL accessor for the environment field of hydra.system.Command
commandEnvironment :: Typed.TypedTerm System.Command -> Typed.TypedTerm (Maybe (M.Map System.EnvironmentVariable String))
commandEnvironment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.Command"),
        Core.projectionFieldName = (Core.Name "environment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the program field of hydra.system.Command
commandProgram :: Typed.TypedTerm System.Command -> Typed.TypedTerm File.FilePath
commandProgram x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.Command"),
        Core.projectionFieldName = (Core.Name "program")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL updater for the arguments field of hydra.system.Command
commandWithArguments :: Typed.TypedTerm System.Command -> Typed.TypedTerm [String] -> Typed.TypedTerm System.Command
commandWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.Command"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "program"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "program")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "workingDirectory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "workingDirectory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the environment field of hydra.system.Command
commandWithEnvironment :: Typed.TypedTerm System.Command -> Typed.TypedTerm (Maybe (M.Map System.EnvironmentVariable String)) -> Typed.TypedTerm System.Command
commandWithEnvironment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.Command"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "program"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "program")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "workingDirectory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "workingDirectory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL updater for the program field of hydra.system.Command
commandWithProgram :: Typed.TypedTerm System.Command -> Typed.TypedTerm File.FilePath -> Typed.TypedTerm System.Command
commandWithProgram original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.Command"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "program"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "workingDirectory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "workingDirectory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the workingDirectory field of hydra.system.Command
commandWithWorkingDirectory :: Typed.TypedTerm System.Command -> Typed.TypedTerm (Maybe File.FilePath) -> Typed.TypedTerm System.Command
commandWithWorkingDirectory original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.Command"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "program"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "program")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "workingDirectory"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.Command"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL accessor for the workingDirectory field of hydra.system.Command
commandWorkingDirectory :: Typed.TypedTerm System.Command -> Typed.TypedTerm (Maybe File.FilePath)
commandWorkingDirectory x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.Command"),
        Core.projectionFieldName = (Core.Name "workingDirectory")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL constructor for the hydra.system.EnvironmentVariable wrapper
environmentVariable :: Typed.TypedTerm String -> Typed.TypedTerm System.EnvironmentVariable
environmentVariable x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.system.EnvironmentVariable"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.system.EnvironmentVariable
environmentVariableEnvironmentVariable :: Typed.TypedName System.EnvironmentVariable
environmentVariableEnvironmentVariable = Typed.TypedName (Core.Name "hydra.system.EnvironmentVariable")

-- | DSL constructor for hydra.system.ProcessResult
processResult :: Typed.TypedTerm System.StatusCode -> Typed.TypedTerm B.ByteString -> Typed.TypedTerm B.ByteString -> Typed.TypedTerm System.ProcessResult
processResult exitCode stdout stderr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.ProcessResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exitCode"),
          Core.fieldTerm = (Typed.unTypedTerm exitCode)},
        Core.Field {
          Core.fieldName = (Core.Name "stdout"),
          Core.fieldTerm = (Typed.unTypedTerm stdout)},
        Core.Field {
          Core.fieldName = (Core.Name "stderr"),
          Core.fieldTerm = (Typed.unTypedTerm stderr)}]}))

-- | DSL accessor for the exitCode field of hydra.system.ProcessResult
processResultExitCode :: Typed.TypedTerm System.ProcessResult -> Typed.TypedTerm System.StatusCode
processResultExitCode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
        Core.projectionFieldName = (Core.Name "exitCode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.system.ProcessResult
processResultProcessResult :: Typed.TypedName System.ProcessResult
processResultProcessResult = Typed.TypedName (Core.Name "hydra.system.ProcessResult")

-- | DSL accessor for the stderr field of hydra.system.ProcessResult
processResultStderr :: Typed.TypedTerm System.ProcessResult -> Typed.TypedTerm B.ByteString
processResultStderr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
        Core.projectionFieldName = (Core.Name "stderr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the stdout field of hydra.system.ProcessResult
processResultStdout :: Typed.TypedTerm System.ProcessResult -> Typed.TypedTerm B.ByteString
processResultStdout x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
        Core.projectionFieldName = (Core.Name "stdout")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL updater for the exitCode field of hydra.system.ProcessResult
processResultWithExitCode :: Typed.TypedTerm System.ProcessResult -> Typed.TypedTerm System.StatusCode -> Typed.TypedTerm System.ProcessResult
processResultWithExitCode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.ProcessResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exitCode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stdout"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
              Core.projectionFieldName = (Core.Name "stdout")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stderr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
              Core.projectionFieldName = (Core.Name "stderr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the stderr field of hydra.system.ProcessResult
processResultWithStderr :: Typed.TypedTerm System.ProcessResult -> Typed.TypedTerm B.ByteString -> Typed.TypedTerm System.ProcessResult
processResultWithStderr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.ProcessResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exitCode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
              Core.projectionFieldName = (Core.Name "exitCode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stdout"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
              Core.projectionFieldName = (Core.Name "stdout")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stderr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL updater for the stdout field of hydra.system.ProcessResult
processResultWithStdout :: Typed.TypedTerm System.ProcessResult -> Typed.TypedTerm B.ByteString -> Typed.TypedTerm System.ProcessResult
processResultWithStdout original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.system.ProcessResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exitCode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
              Core.projectionFieldName = (Core.Name "exitCode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stdout"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stderr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.system.ProcessResult"),
              Core.projectionFieldName = (Core.Name "stderr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL constructor for the hydra.system.StatusCode wrapper
statusCode :: Typed.TypedTerm Int -> Typed.TypedTerm System.StatusCode
statusCode x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.system.StatusCode"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.system.StatusCode
statusCodeStatusCode :: Typed.TypedName System.StatusCode
statusCodeStatusCode = Typed.TypedName (Core.Name "hydra.system.StatusCode")

-- | DSL accessor for the body of hydra.system.EnvironmentVariable
unEnvironmentVariable :: Typed.TypedTerm System.EnvironmentVariable -> Typed.TypedTerm String
unEnvironmentVariable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.system.EnvironmentVariable")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the body of hydra.system.StatusCode
unStatusCode :: Typed.TypedTerm System.StatusCode -> Typed.TypedTerm Int
unStatusCode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.system.StatusCode")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
