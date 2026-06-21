-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.files module.

module Hydra.Lib.Files where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
appendFile :: Packaging.PrimitiveDefinition
appendFile =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.appendFile"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Append bytes to the end of a file."),
        Packaging.entityMetadataComments = [
          "appendFile(path, contents) describes an effectful computation which attempts to append the raw bytes contents to the end of the file at path, creating the file if it does not exist. Unlike writeFile, existing contents are preserved. File I/O is byte-oriented; to append text, encode it to bytes first (e.g. via hydra.lib.text.encodeUtf8). A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBinary),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
copy :: Packaging.PrimitiveDefinition
copy =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.copy"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Copy a file, or a directory tree, to a destination path."),
        Packaging.entityMetadataComments = [
          "copy(recursive, source, destination) describes an effectful computation which attempts to copy source to destination. When recursive is false, source must be a single file. When recursive is true, source may be a directory, whose entire tree is copied. Copying is performed by the host operating system rather than by reading and rewriting contents, preserving binary data and avoiding a round trip through the language runtime. A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
createDirectory :: Packaging.PrimitiveDefinition
createDirectory =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.createDirectory"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Create a directory."),
        Packaging.entityMetadataComments = [
          "createDirectory(recursive, path) describes an effectful computation which attempts to create a directory at path. When recursive is false this corresponds to POSIX mkdir: it fails if path already exists or if a parent is missing. When recursive is true, any missing parent directories are created as well (as with mkdir -p) and an already-existing directory is not an error. A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
exists :: Packaging.PrimitiveDefinition
exists =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.exists"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether a path exists."),
        Packaging.entityMetadataComments = [
          "exists(path) describes an effectful computation which reports whether anything exists at path, whether a file, directory, or other type of file. It does not error when the path is absent; a missing path is reported as right(false). Use status to obtain the type of an existing path. A recoverable file-system failure is returned as left(error)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBoolean)})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
listDirectory :: Packaging.PrimitiveDefinition
listDirectory =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.listDirectory"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "List the immediate entries of a directory."),
        Packaging.entityMetadataComments = [
          "listDirectory(path) describes an effectful computation which returns the immediate entries of the directory at path, excluding the special entries \".\" and \"..\". The result is one level deep and is not ordered; to traverse a tree recursively, recurse using status. A recoverable file-system failure is returned as left(error)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.file.FilePath")))})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
readFile :: Packaging.PrimitiveDefinition
readFile =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.readFile"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Read the complete contents of a file as raw bytes."),
        Packaging.entityMetadataComments = [
          "readFile(path) describes an effectful computation which attempts to read the entire contents of the file at path as raw bytes, with no character decoding or newline translation. To interpret the result as text, decode it (e.g. via hydra.lib.text.decodeUtf8). A recoverable file-system failure is returned as left(error); success is returned as right(contents)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBinary)})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
removeDirectory :: Packaging.PrimitiveDefinition
removeDirectory =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.removeDirectory"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Remove a directory."),
        Packaging.entityMetadataComments = [
          "removeDirectory(recursive, path) describes an effectful computation which attempts to remove the directory at path. When recursive is false this corresponds to POSIX rmdir: it fails unless the directory is empty. When recursive is true, the directory and its entire contents are removed (as with rm -r). Removal is performed by the host operating system rather than by walking and deleting entries individually. A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
removeFile :: Packaging.PrimitiveDefinition
removeFile =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.removeFile"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Remove a file."),
        Packaging.entityMetadataComments = [
          "removeFile(path) describes an effectful computation which attempts to remove the file at path (POSIX unlink). A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
rename :: Packaging.PrimitiveDefinition
rename =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.rename"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Rename or move a file or directory."),
        Packaging.entityMetadataComments = [
          "rename(source, destination) describes an effectful computation which attempts to rename the file or directory at source to destination. When source and destination are on the same file system this is atomic (POSIX rename) and cannot be reproduced by a copy followed by a delete. A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
status :: Packaging.PrimitiveDefinition
status =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.status"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Retrieve metadata about a file or directory."),
        Packaging.entityMetadataComments = [
          "status(path) describes an effectful computation which retrieves metadata about the file at path (POSIX stat), including its type, size, and modification time. Symbolic links are followed. A path which does not exist yields left(notFound); other recoverable file-system failures are also returned as left(error). Use exists for a boolean presence check that does not error on absence."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.file.FileStatus"))})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
writeFile :: Packaging.PrimitiveDefinition
writeFile =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.files.writeFile"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Write raw bytes as the complete contents of a file."),
        Packaging.entityMetadataComments = [
          "writeFile(path, contents) describes an effectful computation which attempts to replace the file at path with the raw bytes contents, with no character encoding or newline translation. To write text, encode it to bytes first (e.g. via hydra.lib.text.encodeUtf8). A recoverable file-system failure is returned as left(error); success is returned as right(unit)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.file.FilePath")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBinary),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEffect (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.file.FileError")),
            Core.eitherTypeRight = Core.TypeUnit})))}},
      Packaging.primitiveDefinitionIsPure = False,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
