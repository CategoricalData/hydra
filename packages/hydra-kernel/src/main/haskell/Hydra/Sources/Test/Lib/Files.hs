module Hydra.Sources.Test.Lib.Files where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
-- Effectful test cases use HONESTLY-TYPED builders (Phantoms + Literals), NOT the reified-Term
-- builders in Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms: effectful cases compile directly to raw target effectful code,
-- so their terms must infer at their true types (effect<...>, string, binary). For #494.
import Hydra.Core.Overlay.Haskell.Dsl.Phantoms hiding ((++))  -- (@@), primitive, lambda, var, wrap, just, nothing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Literals               (string, binary)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Data.ByteString.Char8        as BC
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Hydra.Core.File as File
import qualified Hydra.Core.Lib.Effects as DefEffects
import qualified Hydra.Core.Lib.Eithers as DefEithers
import qualified Hydra.Core.Lib.Files as DefFiles
import qualified Hydra.Core.Lib.Lists as DefLists
import qualified Hydra.Core.Lib.Literals as DefLiterals
import qualified Hydra.Core.Lib.Text as DefText


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.files"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.model", ModuleName "hydra.core.file", ModuleName "hydra.core.testing"],
            moduleMetadata = descriptionMetadata (Just "Effectful test cases for hydra.core.lib.files primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.core.lib.files primitives. Each case is an effect program operating within the
-- canonical temporary directory /tmp/hydra-testing, which the test runner guarantees is empty before
-- each case (these cases reference hydra.core.lib.files primitives, so the runner's scan triggers a clear).

testDir :: String
testDir = "/tmp/hydra-testing"

path :: String -> TypedTerm Term
path rel = wrap File._FilePath (string (testDir ++ "/" ++ rel))

bytes :: String -> TypedTerm Binary
bytes s = binary (BC.pack s)

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Effectful test cases for hydra.core.lib.files primitives" $
    supergroup "hydra.core.lib.files primitives" [
      filesAppendFile,
      filesCopy,
      filesCreateDirectory,
      filesExists,
      filesListDirectory,
      filesReadWrite,
      filesRemoveDirectory,
      filesRemoveFile,
      filesRename,
      filesStatus,
      filesSymlinks,
      filesWriteFile]

-- Fold an effect<either<FileError, T>> into an effect<string> via the eithers eliminator, with the
-- right branch passed through a (T -> string) function and the left branch rendered as "ERR".
foldEither :: TypedTerm a -> TypedTerm b -> TypedTerm c
foldEither showRight eff = retype $
  primitive DefEffects.map
    @@ (lambda "r" $ primitive DefEithers.either
         @@ (lambda "_e" $ string "ERR")
         @@ (retype showRight)
         @@ var "r")
    @@ retype eff
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

-- Decode a binary value to a string (assuming valid UTF-8 here), folding the decode either via fromRight.
decodeBytes :: TypedTerm a -> TypedTerm b
decodeBytes b = retype $
  primitive DefEithers.either @@ (lambda "_e" $ string "<decode error>") @@ (lambda "s" $ var "s")
    @@ (primitive DefText.decodeUtf8 @@ retype b)
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

-- writeFile then readFile: round-trip the contents.
filesReadWrite :: TypedTerm TestGroup
filesReadWrite = subgroup "roundTripsWithWriteFile" [
  effectfulCaseWithProvisions "writeFile then readFile round-trips the contents" ["roundTripsWithWriteFile"] DefFiles.readFile
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "rw.txt" @@ bytes "hello world")
      @@ (lambda "_w" $ foldEither
            (lambda "b" $ decodeBytes (var "b"))
            (primitive DefFiles.readFile @@ path "rw.txt")))
    (string "hello world")]

-- writeFile replaces the complete contents of an existing file, rather than appending: writing a
-- shorter string over a longer one leaves no trace of the original contents.
filesWriteFile :: TypedTerm TestGroup
filesWriteFile = subgroup "replacesCompleteFileContents" [
  effectfulCaseWithProvisions "writeFile replaces existing contents entirely, not appends" ["replacesCompleteFileContents"] DefFiles.writeFile
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "overwrite.txt" @@ bytes "a much longer original string")
      @@ (lambda "_w1" $ primitive DefEffects.bind
            @@ (primitive DefFiles.writeFile @@ path "overwrite.txt" @@ bytes "short")
            @@ (lambda "_w2" $ foldEither
                  (lambda "b" $ decodeBytes (var "b"))
                  (primitive DefFiles.readFile @@ path "overwrite.txt"))))
    (string "short")]

-- appendFile then readFile: contents are preserved and extended.
filesAppendFile :: TypedTerm TestGroup
filesAppendFile = subgroup "preservesExistingContentsAndAppends" [
  effectfulCaseWithProvisions "appendFile after writeFile preserves existing contents" ["preservesExistingContentsAndAppends"] DefFiles.appendFile
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "ap.txt" @@ bytes "AB")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.appendFile @@ path "ap.txt" @@ bytes "CD")
            @@ (lambda "_a" $ foldEither
                  (lambda "b" $ decodeBytes (var "b"))
                  (primitive DefFiles.readFile @@ path "ap.txt"))))
    (string "ABCD")]

-- exists: false before a write, true after.
filesExists :: TypedTerm TestGroup
filesExists = supergroup "exists" [
  subgroup "falseForMissingPath" [
    effectfulCaseWithProvisions "exists is false for a missing path" ["falseForMissingPath"] DefFiles.exists
      (foldEither
        (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
        (primitive DefFiles.exists @@ path "nope.txt"))
      (string "false")],
  subgroup "trueAfterCreation" [
    effectfulCaseWithProvisions "exists is true after writeFile" ["trueAfterCreation"] DefFiles.exists
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "there.txt" @@ bytes "x")
        @@ (lambda "_w" $ foldEither
              (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
              (primitive DefFiles.exists @@ path "there.txt")))
      (string "true")]]

-- copy: a file, then a directory tree.
filesCopy :: TypedTerm TestGroup
filesCopy = supergroup "copy" [
  subgroup "copiesSingleFile" [
    effectfulCaseWithProvisions "copy duplicates a single file" ["copiesSingleFile"] DefFiles.copy
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "cp-src.txt" @@ bytes "copied")
        @@ (lambda "_w" $ primitive DefEffects.bind
              @@ (primitive DefFiles.copy @@ false @@ path "cp-src.txt" @@ path "cp-dst.txt")
              @@ (lambda "_c" $ foldEither
                    (lambda "b" $ decodeBytes (var "b"))
                    (primitive DefFiles.readFile @@ path "cp-dst.txt"))))
      (string "copied")],
  subgroup "copiesDirectoryTreeWhenRecursive" [
    effectfulCaseWithProvisions "copy with recursive=true duplicates a directory tree" ["copiesDirectoryTreeWhenRecursive"] DefFiles.copy
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "cp-dir")
        @@ (lambda "_d" $ primitive DefEffects.bind
              @@ (primitive DefFiles.writeFile @@ path "cp-dir/inner.txt" @@ bytes "nested")
              @@ (lambda "_w" $ primitive DefEffects.bind
                    @@ (primitive DefFiles.copy @@ true @@ path "cp-dir" @@ path "cp-dir-2")
                    @@ (lambda "_c" $ foldEither
                          (lambda "b" $ decodeBytes (var "b"))
                          (primitive DefFiles.readFile @@ path "cp-dir-2/inner.txt")))))
      (string "nested"),
    effectfulCaseWithProvisions "copy with recursive=true into an already-existing destination directory succeeds" ["copiesDirectoryTreeWhenRecursive"] DefFiles.copy
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "cp-dir3")
        @@ (lambda "_d1" $ primitive DefEffects.bind
              @@ (primitive DefFiles.writeFile @@ path "cp-dir3/inner.txt" @@ bytes "again")
              @@ (lambda "_w" $ primitive DefEffects.bind
                    @@ (primitive DefFiles.createDirectory @@ false @@ path "cp-dir3-dst")
                    @@ (lambda "_d2" $ primitive DefEffects.bind
                          @@ (primitive DefFiles.copy @@ true @@ path "cp-dir3" @@ path "cp-dir3-dst")
                          @@ (lambda "_c" $ foldEither
                                (lambda "b" $ decodeBytes (var "b"))
                                (primitive DefFiles.readFile @@ path "cp-dir3-dst/inner.txt"))))))
      (string "again")],
  subgroup "errorsOnNonRecursiveDirectory" [
    effectfulCaseWithProvisions "copy with recursive=false on a directory yields an error" ["errorsOnNonRecursiveDirectory"] DefFiles.copy
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "cp-src-dir")
        @@ (lambda "_d" $ foldEither
              (lambda "b" $ string "unexpected success")
              (primitive DefFiles.copy @@ false @@ path "cp-src-dir" @@ path "cp-src-dir-copy")))
      (string "ERR")]]

-- createDirectory then exists.
filesCreateDirectory :: TypedTerm TestGroup
filesCreateDirectory = subgroup "createsDirectoryThatThenExists" [
  effectfulCaseWithProvisions "createDirectory makes a directory that then exists" ["createsDirectoryThatThenExists"] DefFiles.createDirectory
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "sub")
      @@ (lambda "_c" $ foldEither
            (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
            (primitive DefFiles.exists @@ path "sub")))
    (string "true")]

-- listDirectory: after writing one file, the directory lists exactly one entry.
filesListDirectory :: TypedTerm TestGroup
filesListDirectory = subgroup "listsImmediateEntries" [
  effectfulCaseWithProvisions "listDirectory reports the number of entries" ["listsImmediateEntries"] DefFiles.listDirectory
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "only.txt" @@ bytes "x")
      @@ (lambda "_w" $ foldEither
            (lambda "entries" $ primitive DefLiterals.printInt32 @@ (primitive DefLists.length @@ var "entries"))
            (primitive DefFiles.listDirectory @@ wrap File._FilePath (string testDir))))
    (string "1")]

-- removeDirectory: an empty directory with recursive=false, then a populated one with recursive=true.
filesRemoveDirectory :: TypedTerm TestGroup
filesRemoveDirectory = supergroup "removeDirectory" [
  subgroup "removesEmptyDirectoryWhenNonRecursive" [
    effectfulCaseWithProvisions "removeDirectory with recursive=false removes an empty directory" ["removesEmptyDirectoryWhenNonRecursive"] DefFiles.removeDirectory
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "rmdir-empty")
        @@ (lambda "_d" $ primitive DefEffects.bind
              @@ (primitive DefFiles.removeDirectory @@ false @@ path "rmdir-empty")
              @@ (lambda "_r" $ foldEither
                    (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
                    (primitive DefFiles.exists @@ path "rmdir-empty"))))
      (string "false")],
  subgroup "errorsOnNonEmptyDirectoryWhenNonRecursive" [
    effectfulCaseWithProvisions "removeDirectory with recursive=false on a non-empty directory yields an error" ["errorsOnNonEmptyDirectoryWhenNonRecursive"] DefFiles.removeDirectory
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "rmdir-nonempty")
        @@ (lambda "_d" $ primitive DefEffects.bind
              @@ (primitive DefFiles.writeFile @@ path "rmdir-nonempty/inner.txt" @@ bytes "x")
              @@ (lambda "_w" $ foldEither
                    (lambda "b" $ string "unexpected success")
                    (primitive DefFiles.removeDirectory @@ false @@ path "rmdir-nonempty"))))
      (string "ERR")],
  subgroup "removesPopulatedDirectoryWhenRecursive" [
    effectfulCaseWithProvisions "removeDirectory with recursive=true removes a populated directory" ["removesPopulatedDirectoryWhenRecursive"] DefFiles.removeDirectory
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "rmdir-full")
        @@ (lambda "_d" $ primitive DefEffects.bind
              @@ (primitive DefFiles.writeFile @@ path "rmdir-full/inner.txt" @@ bytes "x")
              @@ (lambda "_w" $ primitive DefEffects.bind
                    @@ (primitive DefFiles.removeDirectory @@ true @@ path "rmdir-full")
                    @@ (lambda "_r" $ foldEither
                          (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
                          (primitive DefFiles.exists @@ path "rmdir-full")))))
      (string "false")]]

-- removeFile then exists is false.
filesRemoveFile :: TypedTerm TestGroup
filesRemoveFile = subgroup "deletesFileSoItNoLongerExists" [
  effectfulCaseWithProvisions "removeFile deletes a file so it no longer exists" ["deletesFileSoItNoLongerExists"] DefFiles.removeFile
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "rm.txt" @@ bytes "x")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.removeFile @@ path "rm.txt")
            @@ (lambda "_r" $ foldEither
                  (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
                  (primitive DefFiles.exists @@ path "rm.txt"))))
    (string "false")]

-- rename then read at the new location.
filesRename :: TypedTerm TestGroup
filesRename = subgroup "movesToNewPathPreservingContents" [
  effectfulCaseWithProvisions "rename moves a file to a new path" ["movesToNewPathPreservingContents"] DefFiles.rename
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "old.txt" @@ bytes "moved")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.rename @@ path "old.txt" @@ path "new.txt")
            @@ (lambda "_r" $ foldEither
                  (lambda "b" $ decodeBytes (var "b"))
                  (primitive DefFiles.readFile @@ path "new.txt"))))
    (string "moved")]

-- Render a hydra.core.file.FileType as a short string, for comparison in status test cases.
showFileType :: TypedTerm (File.FileType -> String)
showFileType = cases File._FileType Nothing [
  File._FileType_block >>: lambda "_x" (string "block"),
  File._FileType_character >>: lambda "_x" (string "character"),
  File._FileType_directory >>: lambda "_x" (string "directory"),
  File._FileType_fifo >>: lambda "_x" (string "fifo"),
  File._FileType_regular >>: lambda "_x" (string "regular"),
  File._FileType_link >>: lambda "_x" (string "link"),
  File._FileType_socket >>: lambda "_x" (string "socket")]

-- status: file type and size for a regular file, file type for a directory.
filesStatus :: TypedTerm TestGroup
filesStatus = supergroup "status" [
  subgroup "reportsCorrectTypeAndSizeForRegularFileAndDirectory" [
    effectfulCaseWithProvisions "status reports the type and size of a regular file" ["reportsCorrectTypeAndSizeForRegularFileAndDirectory"] DefFiles.status
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "stat.txt" @@ bytes "12345")
        @@ (lambda "_w" $ foldEither
              (lambda "s" $ primitive DefLiterals.printInt64 @@
                (project File._FileStatus File._FileStatus_size @@ var "s"))
              (primitive DefFiles.status @@ true @@ path "stat.txt")))
      (string "5"),
    effectfulCaseWithProvisions "status reports directory as the file type" ["reportsCorrectTypeAndSizeForRegularFileAndDirectory"] DefFiles.status
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createDirectory @@ false @@ path "stat-dir")
        @@ (lambda "_d" $ foldEither
              (lambda "s" $ showFileType @@
                (project File._FileStatus File._FileStatus_fileType @@ var "s"))
              (primitive DefFiles.status @@ true @@ path "stat-dir")))
      (string "directory")],
  subgroup "notFoundOnMissingOrDanglingTarget" [
    effectfulCaseWithProvisions "status on a missing path yields an error" ["notFoundOnMissingOrDanglingTarget"] DefFiles.status
      (foldEither
        (lambda "s" $ string "unexpected success")
        (primitive DefFiles.status @@ true @@ path "stat-missing.txt"))
      (string "ERR")]]

-- Symbolic links: creation, inspection with and without following, dangling links, and errors.
filesSymlinks :: TypedTerm TestGroup
filesSymlinks = supergroup "symlinks" [
  subgroup "createsLinkObservableAsLinkType" [
    effectfulCaseWithProvisions "createSymlink then status(false) reports fileType link" ["createsLinkObservableAsLinkType"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "link-target.txt" @@ bytes "target contents")
        @@ (lambda "_w" $ primitive DefEffects.bind
              @@ (primitive DefFiles.createSymlink @@ path "link-target.txt" @@ path "link-to-target")
              @@ (lambda "_l" $ foldEither
                    (lambda "s" $ showFileType @@
                      (project File._FileStatus File._FileStatus_fileType @@ var "s"))
                    (primitive DefFiles.status @@ false @@ path "link-to-target"))))
      (string "link")],
  subgroup "followLinksControlsLinkResolution" [
    effectfulCaseWithProvisions "status(false) on a symlink reports fileType link, not the target's type" ["followLinksControlsLinkResolution"] DefFiles.status
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "link-target4.txt" @@ bytes "4321")
        @@ (lambda "_w" $ primitive DefEffects.bind
              @@ (primitive DefFiles.createSymlink @@ path "link-target4.txt" @@ path "link-to-target4")
              @@ (lambda "_l" $ foldEither
                    (lambda "s" $ showFileType @@
                      (project File._FileStatus File._FileStatus_fileType @@ var "s"))
                    (primitive DefFiles.status @@ false @@ path "link-to-target4"))))
      (string "link"),
    effectfulCaseWithProvisions "status(true) on a symlink reports the target's status" ["followLinksControlsLinkResolution"] DefFiles.status
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "link-target3.txt" @@ bytes "1234")
        @@ (lambda "_w" $ primitive DefEffects.bind
              @@ (primitive DefFiles.createSymlink @@ path "link-target3.txt" @@ path "link-to-target3")
              @@ (lambda "_l" $ foldEither
                    (lambda "s" $ showFileType @@
                      (project File._FileStatus File._FileStatus_fileType @@ var "s"))
                    (primitive DefFiles.status @@ true @@ path "link-to-target3"))))
      (string "regular")],
  subgroup "allowsDanglingTargets" [
    effectfulCaseWithProvisions "createSymlink to a nonexistent target then status(false) reports link, not an error" ["allowsDanglingTargets"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createSymlink @@ path "does-not-exist.txt" @@ path "dangling-link")
        @@ (lambda "_l" $ foldEither
              (lambda "s" $ showFileType @@
                (project File._FileStatus File._FileStatus_fileType @@ var "s"))
              (primitive DefFiles.status @@ false @@ path "dangling-link")))
      (string "link"),
    effectfulCaseWithProvisions "a dangling symlink is not found when followed with status(true)" ["allowsDanglingTargets"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createSymlink @@ path "still-does-not-exist.txt" @@ path "dangling-link2")
        @@ (lambda "_l" $ foldEither
              (lambda "s" $ string "unexpected success")
              (primitive DefFiles.status @@ true @@ path "dangling-link2")))
      (string "ERR"),
    effectfulCaseWithProvisions "a dangling symlink is reported absent by exists" ["allowsDanglingTargets"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createSymlink @@ path "yet-another-missing.txt" @@ path "dangling-link3")
        @@ (lambda "_l" $ foldEither
              (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
              (primitive DefFiles.exists @@ path "dangling-link3")))
      (string "false")],
  subgroup "preservesTargetVerbatim" [
    effectfulCaseWithProvisions "createSymlink preserves a relative target verbatim" ["preservesTargetVerbatim"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createSymlink @@ (wrap File._FilePath (string "relative-target.txt")) @@ path "relative-link")
        @@ (lambda "_l" $ foldEither
              (lambda "t" $ Phantoms.unwrap File._FilePath @@ var "t")
              (primitive DefFiles.readSymlink @@ path "relative-link")))
      (string "relative-target.txt")],
  subgroup "returnsStoredTargetVerbatim" [
    effectfulCaseWithProvisions "readSymlink returns the target verbatim" ["returnsStoredTargetVerbatim"] DefFiles.readSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "link-target2.txt" @@ bytes "x")
        @@ (lambda "_w" $ primitive DefEffects.bind
              @@ (primitive DefFiles.createSymlink @@ path "link-target2.txt" @@ path "link-to-target2")
              @@ (lambda "_l" $ foldEither
                    (lambda "t" $ Phantoms.unwrap File._FilePath @@ var "t")
                    (primitive DefFiles.readSymlink @@ path "link-to-target2"))))
      (string (testDir ++ "/link-target2.txt"))],
  subgroup "errorsOnExistingLinkPath" [
    effectfulCaseWithProvisions "createSymlink over an existing file yields alreadyExists" ["errorsOnExistingLinkPath"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "occupied.txt" @@ bytes "x")
        @@ (lambda "_w" $ foldEither
              (lambda "_u" $ string "unexpected success")
              (primitive DefFiles.createSymlink @@ path "some-target" @@ path "occupied.txt")))
      (string "ERR"),
    effectfulCaseWithProvisions "createSymlink over an existing dangling symlink yields alreadyExists" ["errorsOnExistingLinkPath"] DefFiles.createSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.createSymlink @@ path "missing-target" @@ path "occupied-link")
        @@ (lambda "_l" $ foldEither
              (lambda "_u" $ string "unexpected success")
              (primitive DefFiles.createSymlink @@ path "another-target" @@ path "occupied-link")))
      (string "ERR")],
  subgroup "errorsOnNonLinkOrMissingPath" [
    effectfulCaseWithProvisions "readSymlink on a regular file yields invalidPath" ["errorsOnNonLinkOrMissingPath"] DefFiles.readSymlink
      (primitive DefEffects.bind
        @@ (primitive DefFiles.writeFile @@ path "not-a-link.txt" @@ bytes "x")
        @@ (lambda "_w" $ foldEither
              (lambda "t" $ string "unexpected success")
              (primitive DefFiles.readSymlink @@ path "not-a-link.txt")))
      (string "ERR"),
    effectfulCaseWithProvisions "readSymlink on a missing path yields notFound" ["errorsOnNonLinkOrMissingPath"] DefFiles.readSymlink
      (foldEither
        (lambda "t" $ string "unexpected success")
        (primitive DefFiles.readSymlink @@ path "no-such-link"))
      (string "ERR")]]
