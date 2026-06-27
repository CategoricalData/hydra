-- Note: this is an automatically generated file. Do not edit.

-- | Effectful test cases for hydra.lib.system primitives

module Hydra.Test.Lib.System where

import qualified Hydra.Core as Core
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.File as File
import qualified Hydra.Overlay.Haskell.Lib.Effects as Effects
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Overlay.Haskell.Lib.System as LibSystem
import qualified Hydra.Overlay.Haskell.Lib.Text as Text
import qualified Hydra.System as System
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | Effectful test cases for hydra.lib.system primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.system primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "execute",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "captures stdout",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStdout r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/bin/echo"),
                  System.commandArguments = [
                    "hydra"],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "hydra\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "passes multiple arguments through to stdout",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStdout r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/bin/echo"),
                  System.commandArguments = [
                    "a",
                    "b",
                    "c"],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "a b c\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "captures stdout byte-exactly (no newline translation)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStdout r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/usr/bin/printf"),
                  System.commandArguments = [
                    "abc"],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "abc")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "reports exit status zero for a successful program",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Literals.showInt32 (System.unStatusCode (System.processResultExitCode r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/usr/bin/true"),
                  System.commandArguments = [],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "0")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "reports a non-zero exit status as right (not an error)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Literals.showBoolean (Equality.gt (System.unStatusCode (System.processResultExitCode r2)) 0)) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/usr/bin/false"),
                  System.commandArguments = [],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "captures stderr separately from stdout",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Literals.showBoolean (Logic.not (Strings.null (Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStderr r2)))))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/bin/ls"),
                  System.commandArguments = [
                    "/hydra-nonexistent-498"],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "leaves stdout empty when a program writes only to stderr",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStdout r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/bin/ls"),
                  System.commandArguments = [
                    "/hydra-nonexistent-498"],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "honors the workingDirectory field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStdout r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/bin/pwd"),
                  System.commandArguments = [],
                  System.commandWorkingDirectory = (Just (File.FilePath "/")),
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "/\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "honors the environment field (replacing the child environment)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\r2 -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 (System.processResultStdout r2))) r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/usr/bin/env"),
                  System.commandArguments = [],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = (Just (M.fromList [
                    (System.EnvironmentVariable "HYDRA_X", "hydra-value")]))}))),
                Testing.effectfulTestCaseExpected = (\_ -> "HYDRA_X=hydra-value\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "yields left(commandNotFound) for a missing program",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\e -> case e of
                  ErrorSystem.SystemErrorCommandNotFound _ -> "commandNotFound"
                  ErrorSystem.SystemErrorPermissionDenied _ -> "permissionDenied"
                  ErrorSystem.SystemErrorInvalidWorkingDirectory _ -> "invalidWorkingDirectory"
                  ErrorSystem.SystemErrorInterrupted -> "interrupted"
                  ErrorSystem.SystemErrorOther _ -> "other") (\_r -> "<ran>") r) (LibSystem.execute (System.Command {
                  System.commandProgram = (File.FilePath "/hydra/no/such/program-498"),
                  System.commandArguments = [],
                  System.commandWorkingDirectory = Nothing,
                  System.commandEnvironment = Nothing}))),
                Testing.effectfulTestCaseExpected = (\_ -> "commandNotFound")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "getEnvironment",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "does not contain a guaranteed-absent variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\m -> Literals.showBoolean (Maps.member (System.EnvironmentVariable "HYDRA_DEFINITELY_NOT_SET_498") m)) LibSystem.getEnvironment),
                Testing.effectfulTestCaseExpected = (\_ -> "false")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "getEnvironmentVariable",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "yields none for a guaranteed-absent variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\m -> Optionals.fromOptional "absent" m) (LibSystem.getEnvironmentVariable (System.EnvironmentVariable "HYDRA_DEFINITELY_NOT_SET_498"))),
                Testing.effectfulTestCaseExpected = (\_ -> "absent")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "getTime",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "returns a positive number of seconds since the epoch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\t -> Literals.showBoolean (Equality.gt (Time.timespecSeconds t) 0)) LibSystem.getTime),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "returns nanoseconds within the range [0, 1000000000)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\t -> Literals.showBoolean (Logic.and (Equality.gte (Time.timespecNanoseconds t) 0) (Equality.lt (Time.timespecNanoseconds t) 1000000000))) LibSystem.getTime),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "getWorkingDirectory",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "returns a non-empty path",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\p -> Literals.showBoolean (Logic.not (Strings.null (File.unFilePath p)))) r) LibSystem.getWorkingDirectory),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
