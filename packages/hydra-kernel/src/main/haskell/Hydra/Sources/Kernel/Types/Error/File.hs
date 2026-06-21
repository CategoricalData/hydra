module Hydra.Sources.Kernel.Types.Error.File where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.File as File


ns :: ModuleName
ns = ModuleName "hydra.error.file"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [File.ns],
            moduleMetadata = descriptionMetadata (Just "File-system error types")}
  where
    definitions = [
      fileError]

fileError :: TypeDefinition
fileError = define "FileError" $
  doc "A recoverable file-system error" $
  T.union [
    "alreadyExists">:
      doc "A path already exists where one was required not to (POSIX EEXIST), e.g. creating a directory that is already present" $
      File.filePath,
    "invalidPath">:
      doc "The path is syntactically invalid or cannot be represented by the host file system" $
      T.string,
    "notFound">:
      doc "The requested path does not exist" $
      File.filePath,
    "other">:
      doc "Any other file-system error, represented as a host-provided message" $
      T.string,
    "permissionDenied">:
      doc "The host denied access to the requested path" $
      File.filePath]
