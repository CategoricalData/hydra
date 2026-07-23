module Hydra.Sources.Test.Lib.Files where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
-- Effectful test cases use HONESTLY-TYPED builders (Phantoms + Literals), NOT the reified-Term
-- builders in Hydra.Overlay.Haskell.Dsl.Typed.Terms: effectful cases compile directly to raw target effectful code,
-- so their terms must infer at their true types (effect<...>, string, binary). For #494.
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms hiding ((++))  -- (@@), primitive, lambda, var, wrap, just, nothing
import Hydra.Overlay.Haskell.Dsl.Typed.Literals               (string, binary)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.ByteString.Char8        as BC
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.File as File
import qualified Hydra.Lib.Effects as DefEffects
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Files as DefFiles
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Literals as DefLiterals
import qualified Hydra.Lib.Text as DefText


ns :: ModuleName
ns = ModuleName "hydra.test.lib.files"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core", ModuleName "hydra.file", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Effectful test cases for hydra.lib.files primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.files primitives. Each case is an effect program operating within the
-- canonical temporary directory /tmp/hydra-testing, which the test runner guarantees is empty before
-- each case (these cases reference hydra.lib.files primitives, so the runner's scan triggers a clear).

testDir :: String
testDir = "/tmp/hydra-testing"

path :: String -> TypedTerm Term
path rel = wrap File._FilePath (string (testDir ++ "/" ++ rel))

bytes :: String -> TypedTerm Binary
bytes s = binary (BC.pack s)

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Effectful test cases for hydra.lib.files primitives" $
    supergroup "hydra.lib.files primitives" [
      filesAppendFile,
      filesCopy,
      filesCreateDirectory,
      filesExists,
      filesListDirectory,
      filesReadWrite,
      filesRemoveDirectory,
      filesRemoveFile,
      filesRename,
      filesStatus]

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
filesReadWrite = subgroup "readFile/writeFile" [
  effectfulCase "writeFile then readFile round-trips the contents"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "rw.txt" @@ bytes "hello world")
      @@ (lambda "_w" $ foldEither
            (lambda "b" $ decodeBytes (var "b"))
            (primitive DefFiles.readFile @@ path "rw.txt")))
    (string "hello world")]

-- appendFile then readFile: contents are preserved and extended.
filesAppendFile :: TypedTerm TestGroup
filesAppendFile = subgroup "appendFile" [
  effectfulCase "appendFile after writeFile preserves existing contents"
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
filesExists = subgroup "exists" [
  effectfulCase "exists is false for a missing path"
    (foldEither
      (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
      (primitive DefFiles.exists @@ path "nope.txt"))
    (string "false"),
  effectfulCase "exists is true after writeFile"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "there.txt" @@ bytes "x")
      @@ (lambda "_w" $ foldEither
            (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
            (primitive DefFiles.exists @@ path "there.txt")))
    (string "true")]

-- copy: a file, then a directory tree.
filesCopy :: TypedTerm TestGroup
filesCopy = subgroup "copy" [
  effectfulCase "copy duplicates a single file"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "cp-src.txt" @@ bytes "copied")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.copy @@ false @@ path "cp-src.txt" @@ path "cp-dst.txt")
            @@ (lambda "_c" $ foldEither
                  (lambda "b" $ decodeBytes (var "b"))
                  (primitive DefFiles.readFile @@ path "cp-dst.txt"))))
    (string "copied"),
  effectfulCase "copy with recursive=true duplicates a directory tree"
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
  effectfulCase "copy with recursive=false on a directory yields an error"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "cp-src-dir")
      @@ (lambda "_d" $ foldEither
            (lambda "b" $ string "unexpected success")
            (primitive DefFiles.copy @@ false @@ path "cp-src-dir" @@ path "cp-src-dir-copy")))
    (string "ERR"),
  effectfulCase "copy with recursive=true into an already-existing destination directory succeeds"
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
    (string "again")]

-- createDirectory then exists.
filesCreateDirectory :: TypedTerm TestGroup
filesCreateDirectory = subgroup "createDirectory" [
  effectfulCase "createDirectory makes a directory that then exists"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "sub")
      @@ (lambda "_c" $ foldEither
            (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
            (primitive DefFiles.exists @@ path "sub")))
    (string "true")]

-- listDirectory: after writing one file, the directory lists exactly one entry.
filesListDirectory :: TypedTerm TestGroup
filesListDirectory = subgroup "listDirectory" [
  effectfulCase "listDirectory reports the number of entries"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "only.txt" @@ bytes "x")
      @@ (lambda "_w" $ foldEither
            (lambda "entries" $ primitive DefLiterals.showInt32 @@ (primitive DefLists.length @@ var "entries"))
            (primitive DefFiles.listDirectory @@ wrap File._FilePath (string testDir))))
    (string "1")]

-- removeDirectory: an empty directory with recursive=false, then a populated one with recursive=true.
filesRemoveDirectory :: TypedTerm TestGroup
filesRemoveDirectory = subgroup "removeDirectory" [
  effectfulCase "removeDirectory with recursive=false removes an empty directory"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "rmdir-empty")
      @@ (lambda "_d" $ primitive DefEffects.bind
            @@ (primitive DefFiles.removeDirectory @@ false @@ path "rmdir-empty")
            @@ (lambda "_r" $ foldEither
                  (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
                  (primitive DefFiles.exists @@ path "rmdir-empty"))))
    (string "false"),
  effectfulCase "removeDirectory with recursive=false on a non-empty directory yields an error"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "rmdir-nonempty")
      @@ (lambda "_d" $ primitive DefEffects.bind
            @@ (primitive DefFiles.writeFile @@ path "rmdir-nonempty/inner.txt" @@ bytes "x")
            @@ (lambda "_w" $ foldEither
                  (lambda "b" $ string "unexpected success")
                  (primitive DefFiles.removeDirectory @@ false @@ path "rmdir-nonempty"))))
    (string "ERR"),
  effectfulCase "removeDirectory with recursive=true removes a populated directory"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "rmdir-full")
      @@ (lambda "_d" $ primitive DefEffects.bind
            @@ (primitive DefFiles.writeFile @@ path "rmdir-full/inner.txt" @@ bytes "x")
            @@ (lambda "_w" $ primitive DefEffects.bind
                  @@ (primitive DefFiles.removeDirectory @@ true @@ path "rmdir-full")
                  @@ (lambda "_r" $ foldEither
                        (lambda "b" $ primitive DefLiterals.printBoolean @@ var "b")
                        (primitive DefFiles.exists @@ path "rmdir-full")))))
    (string "false")]

-- removeFile then exists is false.
filesRemoveFile :: TypedTerm TestGroup
filesRemoveFile = subgroup "removeFile" [
  effectfulCase "removeFile deletes a file so it no longer exists"
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
filesRename = subgroup "rename" [
  effectfulCase "rename moves a file to a new path"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "old.txt" @@ bytes "moved")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.rename @@ path "old.txt" @@ path "new.txt")
            @@ (lambda "_r" $ foldEither
                  (lambda "b" $ decodeBytes (var "b"))
                  (primitive DefFiles.readFile @@ path "new.txt"))))
    (string "moved")]

-- Render a hydra.file.FileType as a short string, for comparison in status test cases.
showFileType :: TypedTerm (File.FileType -> String)
showFileType = match File._FileType Nothing [
  File._FileType_block >>: lambda "_x" (string "block"),
  File._FileType_character >>: lambda "_x" (string "character"),
  File._FileType_directory >>: lambda "_x" (string "directory"),
  File._FileType_fifo >>: lambda "_x" (string "fifo"),
  File._FileType_regular >>: lambda "_x" (string "regular"),
  File._FileType_link >>: lambda "_x" (string "link"),
  File._FileType_socket >>: lambda "_x" (string "socket")]

-- status: file type and size for a regular file, file type for a directory.
filesStatus :: TypedTerm TestGroup
filesStatus = subgroup "status" [
  effectfulCase "status reports the type and size of a regular file"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "stat.txt" @@ bytes "12345")
      @@ (lambda "_w" $ foldEither
            (lambda "s" $ primitive DefLiterals.showInt64 @@
              (project File._FileStatus File._FileStatus_size @@ var "s"))
            (primitive DefFiles.status @@ path "stat.txt")))
    (string "5"),
  effectfulCase "status reports directory as the file type"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "stat-dir")
      @@ (lambda "_d" $ foldEither
            (lambda "s" $ showFileType @@
              (project File._FileStatus File._FileStatus_fileType @@ var "s"))
            (primitive DefFiles.status @@ path "stat-dir")))
    (string "directory"),
  effectfulCase "status on a missing path yields an error"
    (foldEither
      (lambda "s" $ string "unexpected success")
      (primitive DefFiles.status @@ path "stat-missing.txt"))
    (string "ERR")]
