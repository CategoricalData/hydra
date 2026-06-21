-- | Primitive declarations for the hydra.lib.files namespace.

module Hydra.Sources.Kernel.Lib.Files where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import           Hydra.Dsl.Prims         (sig)
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Dsl.Types         (effect)
import qualified Hydra.Error.File        as FileError
import qualified Hydra.File              as File
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding (readFile, writeFile)


ns :: ModuleName
ns = ModuleName "hydra.lib.files"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.files module.")}
  where
    definitions = [
      primImpure "appendFile" "Append bytes to the end of a file." appendFileSig [
        "appendFile(path, contents) describes an effectful computation which attempts to append the raw\
        \ bytes contents to the end of the file at path, creating the file if it does not exist. Unlike\
        \ writeFile, existing contents are preserved. File I/O is byte-oriented; to append text, encode\
        \ it to bytes first (e.g. via hydra.lib.text.encodeUtf8). A recoverable file-system failure is\
        \ returned as left(error); success is returned as right(unit)."],
      primImpure "copy" "Copy a file, or a directory tree, to a destination path." copySig [
        "copy(recursive, source, destination) describes an effectful computation which attempts to copy\
        \ source to destination. When recursive is false, source must be a single file. When recursive\
        \ is true, source may be a directory, whose entire tree is copied. Copying is performed by the\
        \ host operating system rather than by reading and rewriting contents, preserving binary data\
        \ and avoiding a round trip through the language runtime. A recoverable file-system failure is\
        \ returned as left(error); success is returned as right(unit)."],
      primImpure "createDirectory" "Create a directory." createDirectorySig [
        "createDirectory(recursive, path) describes an effectful computation which attempts to create a\
        \ directory at path. When recursive is false this corresponds to POSIX mkdir: it fails if path\
        \ already exists or if a parent is missing. When recursive is true, any missing parent\
        \ directories are created as well (as with mkdir -p) and an already-existing directory is not an\
        \ error. A recoverable file-system failure is returned as left(error); success is returned as\
        \ right(unit)."],
      primImpure "exists" "Test whether a path exists." existsSig [
        "exists(path) describes an effectful computation which reports whether anything exists at path,\
        \ whether a file, directory, or other type of file. It does not error when the path is absent;\
        \ a missing path is reported as right(false). Use status to obtain the type of an existing path.\
        \ A recoverable file-system failure is returned as left(error)."],
      primImpure "listDirectory" "List the immediate entries of a directory." listDirectorySig [
        "listDirectory(path) describes an effectful computation which returns the immediate entries of\
        \ the directory at path, excluding the special entries \".\" and \"..\". The result is one level\
        \ deep and is not ordered; to traverse a tree recursively, recurse using status. A recoverable\
        \ file-system failure is returned as left(error)."],
      primImpure "readFile" "Read the complete contents of a file as raw bytes." readFileSig [
        "readFile(path) describes an effectful computation which attempts to read the entire contents of\
        \ the file at path as raw bytes, with no character decoding or newline translation. To interpret\
        \ the result as text, decode it (e.g. via hydra.lib.text.decodeUtf8). A recoverable file-system\
        \ failure is returned as left(error); success is returned as right(contents)."],
      primImpure "removeDirectory" "Remove a directory." removeDirectorySig [
        "removeDirectory(recursive, path) describes an effectful computation which attempts to remove the\
        \ directory at path. When recursive is false this corresponds to POSIX rmdir: it fails unless the\
        \ directory is empty. When recursive is true, the directory and its entire contents are removed\
        \ (as with rm -r). Removal is performed by the host operating system rather than by walking and\
        \ deleting entries individually. A recoverable file-system failure is returned as left(error);\
        \ success is returned as right(unit)."],
      primImpure "removeFile" "Remove a file." removeFileSig [
        "removeFile(path) describes an effectful computation which attempts to remove the file at path\
        \ (POSIX unlink). A recoverable file-system failure is returned as left(error); success is\
        \ returned as right(unit)."],
      primImpure "rename" "Rename or move a file or directory." renameSig [
        "rename(source, destination) describes an effectful computation which attempts to rename the\
        \ file or directory at source to destination. When source and destination are on the same file\
        \ system this is atomic (POSIX rename) and cannot be reproduced by a copy followed by a delete.\
        \ A recoverable file-system failure is returned as left(error); success is returned as\
        \ right(unit)."],
      primImpure "status" "Retrieve metadata about a file or directory." statusSig [
        "status(path) describes an effectful computation which retrieves metadata about the file at path\
        \ (POSIX stat), including its type, size, and modification time. Symbolic links are followed. A\
        \ path which does not exist yields left(notFound); other recoverable file-system failures are\
        \ also returned as left(error). Use exists for a boolean presence check that does not error on\
        \ absence."],
      primImpure "writeFile" "Write raw bytes as the complete contents of a file." writeFileSig [
        "writeFile(path, contents) describes an effectful computation which attempts to replace the file\
        \ at path with the raw bytes contents, with no character encoding or newline translation. To\
        \ write text, encode it to bytes first (e.g. via hydra.lib.text.encodeUtf8). A recoverable\
        \ file-system failure is returned as left(error); success is returned as right(unit)."]]

primImpure :: String -> String -> TermSignature -> [String] -> Definition
primImpure = Phantoms.impurePrimitiveInModule ns

fileError, filePath, fileStatus :: Type
fileError = TypeVariable FileError._FileError
filePath = TypeVariable File._FilePath
fileStatus = TypeVariable File._FileStatus

-- appendFile : FilePath -> binary -> effect<either<FileError, unit>>
appendFileSig :: TermSignature
appendFileSig = sig $ TypeScheme []
  (filePath Types.~> Types.binary Types.~> effect (Types.either_ fileError Types.unit)) Nothing

-- copy : boolean -> FilePath -> FilePath -> effect<either<FileError, unit>>
copySig :: TermSignature
copySig = sig $ TypeScheme []
  (Types.boolean Types.~> filePath Types.~> filePath Types.~> effect (Types.either_ fileError Types.unit)) Nothing

-- createDirectory : boolean -> FilePath -> effect<either<FileError, unit>>
createDirectorySig :: TermSignature
createDirectorySig = sig $ TypeScheme []
  (Types.boolean Types.~> filePath Types.~> effect (Types.either_ fileError Types.unit)) Nothing

-- exists : FilePath -> effect<either<FileError, boolean>>
existsSig :: TermSignature
existsSig = sig $ TypeScheme []
  (filePath Types.~> effect (Types.either_ fileError Types.boolean)) Nothing

-- listDirectory : FilePath -> effect<either<FileError, list<FilePath>>>
listDirectorySig :: TermSignature
listDirectorySig = sig $ TypeScheme []
  (filePath Types.~> effect (Types.either_ fileError (Types.list filePath))) Nothing

-- readFile : FilePath -> effect<either<FileError, binary>>
readFileSig :: TermSignature
readFileSig = sig $ TypeScheme []
  (filePath Types.~> effect (Types.either_ fileError Types.binary)) Nothing

-- removeDirectory : boolean -> FilePath -> effect<either<FileError, unit>>
removeDirectorySig :: TermSignature
removeDirectorySig = sig $ TypeScheme []
  (Types.boolean Types.~> filePath Types.~> effect (Types.either_ fileError Types.unit)) Nothing

-- removeFile : FilePath -> effect<either<FileError, unit>>
removeFileSig :: TermSignature
removeFileSig = sig $ TypeScheme []
  (filePath Types.~> effect (Types.either_ fileError Types.unit)) Nothing

-- rename : FilePath -> FilePath -> effect<either<FileError, unit>>
renameSig :: TermSignature
renameSig = sig $ TypeScheme []
  (filePath Types.~> filePath Types.~> effect (Types.either_ fileError Types.unit)) Nothing

-- status : FilePath -> effect<either<FileError, FileStatus>>
statusSig :: TermSignature
statusSig = sig $ TypeScheme []
  (filePath Types.~> effect (Types.either_ fileError fileStatus)) Nothing

-- writeFile : FilePath -> binary -> effect<either<FileError, unit>>
writeFileSig :: TermSignature
writeFileSig = sig $ TypeScheme []
  (filePath Types.~> Types.binary Types.~> effect (Types.either_ fileError Types.unit)) Nothing
