-- Note: this is an automatically generated file. Do not edit.

-- | Effectful test cases for hydra.lib.files primitives

module Hydra.Test.Lib.Files where

import qualified Hydra.Core as Core
import qualified Hydra.File as File
import qualified Hydra.Overlay.Haskell.Lib.Effects as Effects
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Files as Files
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals
import qualified Hydra.Overlay.Haskell.Lib.Text as Text
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals

-- | Effectful test cases for hydra.lib.files primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.files primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "appendFile",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "appendFile after writeFile preserves existing contents",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/ap.txt") (Literals.stringToBinary "QUI=")) (\_w -> Effects.bind (Files.appendFile (File.FilePath "/tmp/hydra-testing/ap.txt") (Literals.stringToBinary "Q0Q=")) (\_a -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 b)) r) (Files.readFile (File.FilePath "/tmp/hydra-testing/ap.txt"))))),
                Testing.effectfulTestCaseExpected = (\_ -> "ABCD")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "createDirectory",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "createDirectory makes a directory that then exists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/sub")) (\_c -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Literals.showBoolean b) r) (Files.exists (File.FilePath "/tmp/hydra-testing/sub")))),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "exists",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "exists is false for a missing path",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Literals.showBoolean b) r) (Files.exists (File.FilePath "/tmp/hydra-testing/nope.txt"))),
                Testing.effectfulTestCaseExpected = (\_ -> "false")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "exists is true after writeFile",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/there.txt") (Literals.stringToBinary "eA==")) (\_w -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Literals.showBoolean b) r) (Files.exists (File.FilePath "/tmp/hydra-testing/there.txt")))),
                Testing.effectfulTestCaseExpected = (\_ -> "true")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "listDirectory",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "listDirectory reports the number of entries",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/only.txt") (Literals.stringToBinary "eA==")) (\_w -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\entries -> Literals.showInt32 (Lists.length entries)) r) (Files.listDirectory (File.FilePath "/tmp/hydra-testing")))),
                Testing.effectfulTestCaseExpected = (\_ -> "1")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "readFile/writeFile",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "writeFile then readFile round-trips the contents",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/rw.txt") (Literals.stringToBinary "aGVsbG8gd29ybGQ=")) (\_w -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 b)) r) (Files.readFile (File.FilePath "/tmp/hydra-testing/rw.txt")))),
                Testing.effectfulTestCaseExpected = (\_ -> "hello world")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "removeFile",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "removeFile deletes a file so it no longer exists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/rm.txt") (Literals.stringToBinary "eA==")) (\_w -> Effects.bind (Files.removeFile (File.FilePath "/tmp/hydra-testing/rm.txt")) (\_r -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Literals.showBoolean b) r) (Files.exists (File.FilePath "/tmp/hydra-testing/rm.txt"))))),
                Testing.effectfulTestCaseExpected = (\_ -> "false")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rename",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "rename moves a file to a new path",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/old.txt") (Literals.stringToBinary "bW92ZWQ=")) (\_w -> Effects.bind (Files.rename (File.FilePath "/tmp/hydra-testing/old.txt") (File.FilePath "/tmp/hydra-testing/new.txt")) (\_r -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 b)) r) (Files.readFile (File.FilePath "/tmp/hydra-testing/new.txt"))))),
                Testing.effectfulTestCaseExpected = (\_ -> "moved")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
