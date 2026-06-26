-- | Primitive declarations for the hydra.lib.files namespace.

module Hydra.Sources.Kernel.Lib.Files where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Overlay.Haskell.Dsl.Types         (effect)
import qualified Hydra.Error.File        as FileError
import qualified Hydra.File              as File
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding (readFile, writeFile, appendFile)


ns :: ModuleName
ns = ModuleName "hydra.lib.files"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.files module.")}
  where
    definitions = [appendFile, copy, createDirectory, exists, listDirectory, readFile,
                   removeDirectory, removeFile, rename, status, writeFile]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_

fileError, filePath, fileStatus :: Type
fileError = TypeVariable FileError._FileError
filePath = TypeVariable File._FilePath
fileStatus = TypeVariable File._FileStatus

result :: Type -> Type
result t = effect (Types.either_ fileError t)

appendFile :: PrimitiveDefinition
appendFile = define "appendFile" "Append bytes to the end of a file."
  (sig $ TypeScheme [] (filePath Types.~> Types.binary Types.~> result Types.unit) Nothing)
  ["appendFile(path, contents) describes an effectful computation which attempts to append the raw\
  \ bytes contents to the end of the file at path, creating the file if it does not exist. Unlike\
  \ writeFile, existing contents are preserved. File I/O is byte-oriented; to append text, encode\
  \ it to bytes first (e.g. via hydra.lib.text.encodeUtf8). A recoverable file-system failure is\
  \ returned as left(error); success is returned as right(unit)."]

copy :: PrimitiveDefinition
copy = define "copy" "Copy a file, or a directory tree, to a destination path."
  (sig $ TypeScheme []
    (Types.boolean Types.~> filePath Types.~> filePath Types.~> result Types.unit) Nothing)
  ["copy(recursive, source, destination) describes an effectful computation which attempts to copy\
  \ source to destination. When recursive is false, source must be a single file. When recursive\
  \ is true, source may be a directory, whose entire tree is copied. Copying is performed by the\
  \ host operating system rather than by reading and rewriting contents, preserving binary data\
  \ and avoiding a round trip through the language runtime. A recoverable file-system failure is\
  \ returned as left(error); success is returned as right(unit)."]

createDirectory :: PrimitiveDefinition
createDirectory = define "createDirectory" "Create a directory."
  (sig $ TypeScheme []
    (Types.boolean Types.~> filePath Types.~> result Types.unit) Nothing)
  ["createDirectory(recursive, path) describes an effectful computation which attempts to create a\
  \ directory at path. When recursive is false this corresponds to POSIX mkdir: it fails if path\
  \ already exists or if a parent is missing. When recursive is true, any missing parent\
  \ directories are created as well (as with mkdir -p) and an already-existing directory is not an\
  \ error. A recoverable file-system failure is returned as left(error); success is returned as\
  \ right(unit)."]

exists :: PrimitiveDefinition
exists = define "exists" "Test whether a path exists."
  (sig $ TypeScheme [] (filePath Types.~> result Types.boolean) Nothing)
  ["exists(path) describes an effectful computation which reports whether anything exists at path,\
  \ whether a file, directory, or other type of file. It does not error when the path is absent;\
  \ a missing path is reported as right(false). Use status to obtain the type of an existing path.\
  \ A recoverable file-system failure is returned as left(error)."]

listDirectory :: PrimitiveDefinition
listDirectory = define "listDirectory" "List the immediate entries of a directory."
  (sig $ TypeScheme [] (filePath Types.~> result (Types.list filePath)) Nothing)
  ["listDirectory(path) describes an effectful computation which returns the immediate entries of\
  \ the directory at path, excluding the special entries \".\" and \"..\". The result is one level\
  \ deep and is not ordered; to traverse a tree recursively, recurse using status. A recoverable\
  \ file-system failure is returned as left(error)."]

readFile :: PrimitiveDefinition
readFile = define "readFile" "Read the complete contents of a file as raw bytes."
  (sig $ TypeScheme [] (filePath Types.~> result Types.binary) Nothing)
  ["readFile(path) describes an effectful computation which attempts to read the entire contents of\
  \ the file at path as raw bytes, with no character decoding or newline translation. To interpret\
  \ the result as text, decode it (e.g. via hydra.lib.text.decodeUtf8). A recoverable file-system\
  \ failure is returned as left(error); success is returned as right(contents)."]

removeDirectory :: PrimitiveDefinition
removeDirectory = define "removeDirectory" "Remove a directory."
  (sig $ TypeScheme []
    (Types.boolean Types.~> filePath Types.~> result Types.unit) Nothing)
  ["removeDirectory(recursive, path) describes an effectful computation which attempts to remove the\
  \ directory at path. When recursive is false this corresponds to POSIX rmdir: it fails unless the\
  \ directory is empty. When recursive is true, the directory and its entire contents are removed\
  \ (as with rm -r). Removal is performed by the host operating system rather than by walking and\
  \ deleting entries individually. A recoverable file-system failure is returned as left(error);\
  \ success is returned as right(unit)."]

removeFile :: PrimitiveDefinition
removeFile = define "removeFile" "Remove a file."
  (sig $ TypeScheme [] (filePath Types.~> result Types.unit) Nothing)
  ["removeFile(path) describes an effectful computation which attempts to remove the file at path\
  \ (POSIX unlink). A recoverable file-system failure is returned as left(error); success is\
  \ returned as right(unit)."]

rename :: PrimitiveDefinition
rename = define "rename" "Rename or move a file or directory."
  (sig $ TypeScheme [] (filePath Types.~> filePath Types.~> result Types.unit) Nothing)
  ["rename(source, destination) describes an effectful computation which attempts to rename the\
  \ file or directory at source to destination. When source and destination are on the same file\
  \ system this is atomic (POSIX rename) and cannot be reproduced by a copy followed by a delete.\
  \ A recoverable file-system failure is returned as left(error); success is returned as\
  \ right(unit)."]

status :: PrimitiveDefinition
status = define "status" "Retrieve metadata about a file or directory."
  (sig $ TypeScheme [] (filePath Types.~> result fileStatus) Nothing)
  ["status(path) describes an effectful computation which retrieves metadata about the file at path\
  \ (POSIX stat), including its type, size, and modification time. Symbolic links are followed. A\
  \ path which does not exist yields left(notFound); other recoverable file-system failures are\
  \ also returned as left(error). Use exists for a boolean presence check that does not error on\
  \ absence."]

writeFile :: PrimitiveDefinition
writeFile = define "writeFile" "Write raw bytes as the complete contents of a file."
  (sig $ TypeScheme [] (filePath Types.~> Types.binary Types.~> result Types.unit) Nothing)
  ["writeFile(path, contents) describes an effectful computation which attempts to replace the file\
  \ at path with the raw bytes contents, with no character encoding or newline translation. To\
  \ write text, encode it to bytes first (e.g. via hydra.lib.text.encodeUtf8). A recoverable\
  \ file-system failure is returned as left(error); success is returned as right(unit)."]
