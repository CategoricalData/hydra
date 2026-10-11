-- | Primitive declarations for the hydra.core.lib.files namespace.

module Hydra.Sources.Kernel.Lib.Files where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Core.Overlay.Haskell.Dsl.Types         (effect)
import qualified Hydra.Core.Error.File        as FileError
import qualified Hydra.Core.File              as File
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding (readFile, writeFile, appendFile)


ns :: ModuleName
ns = ModuleName "hydra.core.lib.files"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = descriptionMetadataWithComments (Just "Primitives in the hydra.core.lib.files module.") [
              "Every primitive in this module is effectful: it describes a computation against the file system,\
              \ and executing that computation is separate from describing it."]}
  where
    definitions = [appendFile, copy, createDirectory, createSymlink, exists, listDirectory,
                   readFile, readSymlink, removeDirectory, removeFile, rename, status, writeFile]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_

fileError, filePath, fileStatus :: Type
fileError = TypeVariable FileError._FileError
filePath = TypeVariable File._FilePath
fileStatus = TypeVariable File._FileStatus

result :: Type -> Type
result t = effect (Types.either_ fileError t)

appendFile :: PrimitiveDefinition
appendFile = availableSince "0.17" $ withProvisions [
  requirement "preservesExistingContentsAndAppends"
    "appendFile adds the given bytes to the end of a file's existing contents, preserving what\
    \ was already there, rather than replacing it as writeFile does."]
  $ define "appendFile" "Append bytes to the end of a file."
  (sigWithParams [("path", "the path of the file to append to"),
                  ("contents", "the raw bytes to append")] $
    TypeScheme [] (filePath Types.~> Types.binary Types.~> result Types.unit) mempty)
  ["appendFile(path, contents) describes an effectful computation which attempts to append the raw\
  \ bytes contents to the end of the file at path, creating the file if it does not exist. Unlike\
  \ writeFile, existing contents are preserved. File I/O is byte-oriented; to append text, encode\
  \ it to bytes first (e.g. via hydra.core.lib.text.encodeUtf8). A recoverable file-system failure is\
  \ returned as left(error); success is returned as right(unit)."]

copy :: PrimitiveDefinition
copy = withProvisions [
  requirement "copiesSingleFile"
    "copy with recursive=false duplicates a single file's contents at the destination path.",
  requirement "copiesDirectoryTreeWhenRecursive"
    "copy with recursive=true duplicates an entire directory tree, including nested files, at the\
    \ destination path, succeeding even when the destination directory already exists.",
  requirement "errorsOnNonRecursiveDirectory"
    "copy with recursive=false on a directory source yields an error, rather than silently copying\
    \ only the directory entry or a subset of its contents."]
  $ deprecatedSince "0.18" "copyFile" $ define "copy" "Copy a file, or a directory tree, to a destination path."
  (sigWithParams [("recursive", "whether to copy a directory tree rather than a single file"),
                  ("source", "the path to copy from"),
                  ("destination", "the path to copy to")] $
    TypeScheme []
    (Types.boolean Types.~> filePath Types.~> filePath Types.~> result Types.unit) mempty)
  ["copy(recursive, source, destination) describes an effectful computation which attempts to copy\
  \ source to destination. When recursive is false, source must be a single file. When recursive\
  \ is true, source may be a directory, whose entire tree is copied. Copying is performed by the\
  \ host operating system rather than by reading and rewriting contents, preserving binary data\
  \ and avoiding a round trip through the language runtime. A recoverable file-system failure is\
  \ returned as left(error); success is returned as right(unit)."]

createDirectory :: PrimitiveDefinition
createDirectory = availableSince "0.17" $ withComments [
  "0.18 is planned to drop the leading recursive flag in favor of a flagless createDirectoryTree\
  \ primitive, mirroring the copy/copyFile+copyTree split; not yet implemented."]
  $ withProvisions [
  requirement "createsDirectoryThatThenExists"
    "createDirectory creates a directory at the given path such that a subsequent exists check on\
    \ that path reports true."]
  $ define "createDirectory" "Create a directory."
  (sigWithParams [("recursive", "whether to create missing parent directories as well"),
                  ("path", "the path of the directory to create")] $
    TypeScheme []
    (Types.boolean Types.~> filePath Types.~> result Types.unit) mempty)
  ["createDirectory(recursive, path) describes an effectful computation which attempts to create a\
  \ directory at path. When recursive is false this corresponds to POSIX mkdir: it fails if path\
  \ already exists or if a parent is missing. When recursive is true, any missing parent\
  \ directories are created as well (as with mkdir -p) and an already-existing directory is not an\
  \ error. A recoverable file-system failure is returned as left(error); success is returned as\
  \ right(unit)."]

createSymlink :: PrimitiveDefinition
createSymlink = availableSince "0.17.6" $ withProvisions [
  requirement "createsLinkObservableAsLinkType"
    "createSymlink creates a symbolic link such that status(followLinks=false) on the link path\
    \ reports fileType link; status(followLinks=true) on the link path reports the metadata of the\
    \ link's target (not the link itself) when the target exists.",
  requirement "allowsDanglingTargets"
    "createSymlink succeeds even when the target does not exist, creating a dangling link; the\
    \ dangling link is itself reported as present by exists (observed via status, not treated as\
    \ absent), and still reports fileType link under status(followLinks=false), but is reported as\
    \ notFound when followed via status(followLinks=true).",
  requirement "preservesTargetVerbatim"
    "createSymlink stores the target path exactly as given, including a relative path, with no\
    \ resolution or canonicalization; readSymlink returns that exact stored target.",
  requirement "errorsOnExistingLinkPath"
    "createSymlink fails (there is no force/replace flag) when the link path already exists,\
    \ whether that existing entry is a regular file or another symbolic link (including a dangling\
    \ one)."]
  $ define "createSymlink" "Create a symbolic link."
  (sigWithParams [("target", "the path that the new symbolic link will point to"),
                  ("link", "the path of the symbolic link to create")] $
    TypeScheme [] (filePath Types.~> filePath Types.~> result Types.unit) mempty)
  ["createSymlink(target, link) describes an effectful computation which attempts to create a\
  \ symbolic link at link, pointing to target (POSIX symlink). target is stored verbatim; it may\
  \ be a relative path, and it need not exist. There is no force flag: if link already exists\
  \ (including as a dangling symbolic link), the computation fails with left(alreadyExists); a\
  \ caller that wants replace-if-exists semantics should compose status(false, link),\
  \ removeFile, and createSymlink. A missing parent directory yields left(notFound); a host or\
  \ file system which refuses to create the link (e.g. an unprivileged process on Windows, or an\
  \ unsupported file system) yields left(permissionDenied) or left(other) as appropriate. A\
  \ recoverable file-system failure is returned as left(error); success is returned as\
  \ right(unit)."]

exists :: PrimitiveDefinition
exists = availableSince "0.17" $ withProvisions [
  requirement "falseForMissingPath"
    "exists returns right(false) for a missing path, without erroring on absence.",
  requirement "trueAfterCreation"
    "exists returns right(true) for a path immediately after a file or directory has been created\
    \ there."]
  $ define "exists" "Test whether a path exists."
  (sigWithParams [("path", "the path to test for existence")] $
    TypeScheme [] (filePath Types.~> result Types.boolean) mempty)
  ["exists(path) describes an effectful computation which reports whether anything exists at path,\
  \ whether a file, directory, or other type of file. It does not error when the path is absent;\
  \ a missing path is reported as right(false). Use status to obtain the type of an existing path.\
  \ A recoverable file-system failure is returned as left(error)."]

listDirectory :: PrimitiveDefinition
listDirectory = availableSince "0.17" $ withProvisions [
  requirement "listsImmediateEntries"
    "listDirectory returns exactly the immediate entries of the directory, one entry per file\
    \ actually present, excluding the special \".\" and \"..\" entries."]
  $ define "listDirectory" "List the immediate entries of a directory."
  (sigWithParams [("path", "the path of the directory to list")] $
    TypeScheme [] (filePath Types.~> result (Types.list filePath)) mempty)
  ["listDirectory(path) describes an effectful computation which returns the immediate entries of\
  \ the directory at path, excluding the special entries \".\" and \"..\". The result is one level\
  \ deep and is not ordered; to traverse a tree recursively, recurse using status. A recoverable\
  \ file-system failure is returned as left(error)."]

readFile :: PrimitiveDefinition
readFile = availableSince "0.17" $ withProvisions [
  requirement "roundTripsWithWriteFile"
    "readFile returns exactly the bytes most recently written to a file via writeFile (and any\
    \ subsequent appendFile calls), byte-exactly, with no character decoding or translation."]
  $ define "readFile" "Read the complete contents of a file as raw bytes."
  (sigWithParams [("path", "the path of the file to read")] $
    TypeScheme [] (filePath Types.~> result Types.binary) mempty)
  ["readFile(path) describes an effectful computation which attempts to read the entire contents of\
  \ the file at path as raw bytes, with no character decoding or newline translation. To interpret\
  \ the result as text, decode it (e.g. via hydra.core.lib.text.decodeUtf8). A recoverable file-system\
  \ failure is returned as left(error); success is returned as right(contents)."]

readSymlink :: PrimitiveDefinition
readSymlink = availableSince "0.17.6" $ withProvisions [
  requirement "returnsStoredTargetVerbatim"
    "readSymlink returns the target exactly as stored by createSymlink, with no chain resolution\
    \ or canonicalization, including a relative target returned relative.",
  requirement "errorsOnNonLinkOrMissingPath"
    "readSymlink yields left(invalidPath) when the path exists but is not a symbolic link, and\
    \ left(notFound) when the path does not exist at all -- two distinct error conditions, neither\
    \ of which is confused with a successful read."]
  $ define "readSymlink" "Read the target of a symbolic link."
  (sigWithParams [("path", "the path of the symbolic link to read")] $
    TypeScheme [] (filePath Types.~> result filePath) mempty)
  ["readSymlink(path) describes an effectful computation which attempts to read the target of the\
  \ symbolic link at path (POSIX readlink). This is a single-hop, unresolved read: the target is\
  \ returned exactly as stored, with no chain resolution or canonicalization; a relative target\
  \ is returned relative, and a dangling target (one that does not exist) is returned as-is. To\
  \ resolve a chain of links to a canonical path, compose repeated calls to readSymlink and\
  \ status. A missing path yields left(notFound); a path which exists but is not a symbolic link\
  \ yields left(invalidPath). A recoverable file-system failure is returned as left(error);\
  \ success is returned as right(target)."]

removeDirectory :: PrimitiveDefinition
removeDirectory = availableSince "0.17" $ withComments [
  "0.18 is planned to drop the leading recursive flag in favor of a flagless\
  \ removeDirectoryTree primitive, mirroring the copy/copyFile+copyTree split; not yet\
  \ implemented."]
  $ withProvisions [
  requirement "removesEmptyDirectoryWhenNonRecursive"
    "removeDirectory with recursive=false removes an empty directory, so that it no longer\
    \ exists.",
  requirement "errorsOnNonEmptyDirectoryWhenNonRecursive"
    "removeDirectory with recursive=false on a non-empty directory yields an error, rather than\
    \ silently removing it or its contents.",
  requirement "removesPopulatedDirectoryWhenRecursive"
    "removeDirectory with recursive=true removes a directory and its entire contents, so that the\
    \ directory no longer exists."]
  $ define "removeDirectory" "Remove a directory."
  (sigWithParams [("recursive", "whether to remove the directory's contents as well"),
                  ("path", "the path of the directory to remove")] $
    TypeScheme []
    (Types.boolean Types.~> filePath Types.~> result Types.unit) mempty)
  ["removeDirectory(recursive, path) describes an effectful computation which attempts to remove the\
  \ directory at path. When recursive is false this corresponds to POSIX rmdir: it fails unless the\
  \ directory is empty. When recursive is true, the directory and its entire contents are removed\
  \ (as with rm -r). Removal is performed by the host operating system rather than by walking and\
  \ deleting entries individually. A recoverable file-system failure is returned as left(error);\
  \ success is returned as right(unit)."]

removeFile :: PrimitiveDefinition
removeFile = availableSince "0.17" $ withProvisions [
  requirement "deletesFileSoItNoLongerExists"
    "removeFile deletes the file at the given path, so that a subsequent exists check reports\
    \ false."]
  $ define "removeFile" "Remove a file."
  (sigWithParams [("path", "the path of the file to remove")] $
    TypeScheme [] (filePath Types.~> result Types.unit) mempty)
  ["removeFile(path) describes an effectful computation which attempts to remove the file at path\
  \ (POSIX unlink). A recoverable file-system failure is returned as left(error); success is\
  \ returned as right(unit)."]

rename :: PrimitiveDefinition
rename = availableSince "0.17" $ withProvisions [
  requirement "movesToNewPathPreservingContents"
    "rename moves a file (or directory) from source to destination, such that the original\
    \ contents are readable at the destination path afterward."]
  $ define "rename" "Rename or move a file or directory."
  (sigWithParams [("source", "the path to rename from"),
                  ("destination", "the path to rename to")] $
    TypeScheme [] (filePath Types.~> filePath Types.~> result Types.unit) mempty)
  ["rename(source, destination) describes an effectful computation which attempts to rename the\
  \ file or directory at source to destination. When source and destination are on the same file\
  \ system this is atomic (POSIX rename) and cannot be reproduced by a copy followed by a delete.\
  \ A recoverable file-system failure is returned as left(error); success is returned as\
  \ right(unit)."]

status :: PrimitiveDefinition
status = availableSince "0.17" $ withProvisions [
  requirement "reportsCorrectTypeAndSizeForRegularFileAndDirectory"
    "status reports the correct fileType and size for a regular file (size equal to its byte\
    \ count) and reports directory as the fileType for a directory.",
  requirement "followLinksControlsLinkResolution"
    "status(followLinks=false) reports the metadata of a symbolic link itself (fileType link), while\
    \ status(followLinks=true) on the same path reports the metadata of the link's target (its true\
    \ fileType, not link) when the target exists -- the only way to observe fileType link is\
    \ followLinks=false.",
  requirement "notFoundOnMissingOrDanglingTarget"
    "status yields left(notFound) for a path that does not exist, and also for a dangling symbolic\
    \ link when followed via followLinks=true (even though the dangling link itself is not an\
    \ error under followLinks=false)."]
  $ define "status" "Retrieve metadata about a file or directory."
  (sigWithParams [("followLinks", "whether to resolve symbolic links (true reproduces the\
                  \ previous, follow, behavior; false inspects the entry itself)"),
                  ("path", "the path to retrieve metadata for")] $
    TypeScheme [] (Types.boolean Types.~> filePath Types.~> result fileStatus) mempty)
  ["status(followLinks, path) describes an effectful computation which retrieves metadata about\
  \ the file system entry at path, including its type, size, and modification time. When\
  \ followLinks is true, this corresponds to POSIX stat: if path is a symbolic link, the metadata\
  \ describes the link's target, and a dangling link (whose target does not exist) yields\
  \ left(notFound). When followLinks is false, this corresponds to POSIX lstat: the metadata\
  \ describes the link itself, with fileType link, a size equal to the length of the stored\
  \ target path, and the link's own timestamps; a dangling link is not an error in this mode.\
  \ status(false, path) is the only way to observe fileType link; status(true, path) never\
  \ produces it, since a followed link's metadata is always that of its (non-link) target. For\
  \ paths that are not symbolic links, both modes report identical metadata. A path which does\
  \ not exist yields left(notFound); other recoverable file-system failures are also returned as\
  \ left(error). Use exists for a boolean presence check that does not error on absence."]

writeFile :: PrimitiveDefinition
writeFile = availableSince "0.17" $ withProvisions [
  requirement "replacesCompleteFileContents"
    "writeFile replaces the complete contents of the file at the given path with the given bytes,\
    \ creating the file if it does not already exist."]
  $ define "writeFile" "Write raw bytes as the complete contents of a file."
  (sigWithParams [("path", "the path of the file to write"),
                  ("contents", "the raw bytes to write as the file's contents")] $
    TypeScheme [] (filePath Types.~> Types.binary Types.~> result Types.unit) mempty)
  ["writeFile(path, contents) describes an effectful computation which attempts to replace the file\
  \ at path with the raw bytes contents, with no character encoding or newline translation. To\
  \ write text, encode it to bytes first (e.g. via hydra.core.lib.text.encodeUtf8). A recoverable\
  \ file-system failure is returned as left(error); success is returned as right(unit)."]
