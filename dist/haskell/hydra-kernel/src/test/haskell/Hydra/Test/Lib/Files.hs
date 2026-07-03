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
          Testing.testGroupName = "copy",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "copy duplicates a single file",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/cp-src.txt") (Literals.stringToBinary "Y29waWVk")) (\_w -> Effects.bind (Files.copy False (File.FilePath "/tmp/hydra-testing/cp-src.txt") (File.FilePath "/tmp/hydra-testing/cp-dst.txt")) (\_c -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 b)) r) (Files.readFile (File.FilePath "/tmp/hydra-testing/cp-dst.txt"))))),
                Testing.effectfulTestCaseExpected = (\_ -> "copied")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "copy with recursive=true duplicates a directory tree",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/cp-dir")) (\_d -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/cp-dir/inner.txt") (Literals.stringToBinary "bmVzdGVk")) (\_w -> Effects.bind (Files.copy True (File.FilePath "/tmp/hydra-testing/cp-dir") (File.FilePath "/tmp/hydra-testing/cp-dir-2")) (\_c -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 b)) r) (Files.readFile (File.FilePath "/tmp/hydra-testing/cp-dir-2/inner.txt")))))),
                Testing.effectfulTestCaseExpected = (\_ -> "nested")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "copy with recursive=false on a directory yields an error",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/cp-src-dir")) (\_d -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> "unexpected success") r) (Files.copy False (File.FilePath "/tmp/hydra-testing/cp-src-dir") (File.FilePath "/tmp/hydra-testing/cp-src-dir-copy")))),
                Testing.effectfulTestCaseExpected = (\_ -> "ERR")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "copy with recursive=true into an already-existing destination directory succeeds",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/cp-dir3")) (\_d1 -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/cp-dir3/inner.txt") (Literals.stringToBinary "YWdhaW4=")) (\_w -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/cp-dir3-dst")) (\_d2 -> Effects.bind (Files.copy True (File.FilePath "/tmp/hydra-testing/cp-dir3") (File.FilePath "/tmp/hydra-testing/cp-dir3-dst")) (\_c -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Eithers.fromRight "<decode error>" (Text.decodeUtf8 b)) r) (Files.readFile (File.FilePath "/tmp/hydra-testing/cp-dir3-dst/inner.txt"))))))),
                Testing.effectfulTestCaseExpected = (\_ -> "again")})),
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
          Testing.testGroupName = "removeDirectory",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "removeDirectory with recursive=false removes an empty directory",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/rmdir-empty")) (\_d -> Effects.bind (Files.removeDirectory False (File.FilePath "/tmp/hydra-testing/rmdir-empty")) (\_r -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Literals.showBoolean b) r) (Files.exists (File.FilePath "/tmp/hydra-testing/rmdir-empty"))))),
                Testing.effectfulTestCaseExpected = (\_ -> "false")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "removeDirectory with recursive=false on a non-empty directory yields an error",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/rmdir-nonempty")) (\_d -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/rmdir-nonempty/inner.txt") (Literals.stringToBinary "eA==")) (\_w -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> "unexpected success") r) (Files.removeDirectory False (File.FilePath "/tmp/hydra-testing/rmdir-nonempty"))))),
                Testing.effectfulTestCaseExpected = (\_ -> "ERR")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "removeDirectory with recursive=true removes a populated directory",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/rmdir-full")) (\_d -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/rmdir-full/inner.txt") (Literals.stringToBinary "eA==")) (\_w -> Effects.bind (Files.removeDirectory True (File.FilePath "/tmp/hydra-testing/rmdir-full")) (\_r -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\b -> Literals.showBoolean b) r) (Files.exists (File.FilePath "/tmp/hydra-testing/rmdir-full")))))),
                Testing.effectfulTestCaseExpected = (\_ -> "false")})),
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
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "status",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "status reports the type and size of a regular file",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.writeFile (File.FilePath "/tmp/hydra-testing/stat.txt") (Literals.stringToBinary "MTIzNDU=")) (\_w -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\s -> Literals.showInt64 (File.fileStatusSize s)) r) (Files.status (File.FilePath "/tmp/hydra-testing/stat.txt")))),
                Testing.effectfulTestCaseExpected = (\_ -> "5")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "status reports directory as the file type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.bind (Files.createDirectory False (File.FilePath "/tmp/hydra-testing/stat-dir")) (\_d -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\s -> case (File.fileStatusFileType s) of
                  File.FileTypeBlock -> "block"
                  File.FileTypeCharacter -> "character"
                  File.FileTypeDirectory -> "directory"
                  File.FileTypeFifo -> "fifo"
                  File.FileTypeRegular -> "regular"
                  File.FileTypeLink -> "link"
                  File.FileTypeSocket -> "socket") r) (Files.status (File.FilePath "/tmp/hydra-testing/stat-dir")))),
                Testing.effectfulTestCaseExpected = (\_ -> "directory")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "status on a missing path yields an error",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEffectful (Testing.EffectfulTestCase {
                Testing.effectfulTestCaseActual = (\_ -> Effects.map (\r -> Eithers.either (\_e -> "ERR") (\s -> "unexpected success") r) (Files.status (File.FilePath "/tmp/hydra-testing/stat-missing.txt"))),
                Testing.effectfulTestCaseExpected = (\_ -> "ERR")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
