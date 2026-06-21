module Hydra.Sources.Test.Lib.Files where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
-- Effectful test cases use HONESTLY-TYPED builders (Phantoms + Literals), NOT the reified-Term
-- builders in Hydra.Dsl.Meta.Terms: effectful cases compile directly to raw target effectful code,
-- so their terms must infer at their true types (effect<...>, string, binary). For #494.
import Hydra.Dsl.Meta.Phantoms hiding ((++))  -- (@@), primitive, lambda, var, wrap, just, nothing
import Hydra.Dsl.Meta.Literals               (string, binary)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
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
      filesCreateDirectory,
      filesExists,
      filesListDirectory,
      filesReadWrite,
      filesRemoveFile,
      filesRename]

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
  primitive DefEithers.fromRight @@ string "<decode error>" @@ (primitive DefText.decodeUtf8 @@ retype b)
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
      (lambda "b" $ primitive DefLiterals.showBoolean @@ var "b")
      (primitive DefFiles.exists @@ path "nope.txt"))
    (string "false"),
  effectfulCase "exists is true after writeFile"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "there.txt" @@ bytes "x")
      @@ (lambda "_w" $ foldEither
            (lambda "b" $ primitive DefLiterals.showBoolean @@ var "b")
            (primitive DefFiles.exists @@ path "there.txt")))
    (string "true")]

-- createDirectory then exists.
filesCreateDirectory :: TypedTerm TestGroup
filesCreateDirectory = subgroup "createDirectory" [
  effectfulCase "createDirectory makes a directory that then exists"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.createDirectory @@ false @@ path "sub")
      @@ (lambda "_c" $ foldEither
            (lambda "b" $ primitive DefLiterals.showBoolean @@ var "b")
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

-- removeFile then exists is false.
filesRemoveFile :: TypedTerm TestGroup
filesRemoveFile = subgroup "removeFile" [
  effectfulCase "removeFile deletes a file so it no longer exists"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "rm.txt" @@ bytes "x")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.removeFile @@ path "rm.txt")
            @@ (lambda "_r" $ foldEither
                  (lambda "b" $ primitive DefLiterals.showBoolean @@ var "b")
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
