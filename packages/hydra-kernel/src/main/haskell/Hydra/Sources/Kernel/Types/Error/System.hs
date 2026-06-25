module Hydra.Sources.Kernel.Types.Error.System where

import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.File as File


ns :: ModuleName
ns = ModuleName "hydra.error.system"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [File.ns],
            moduleMetadata = descriptionMetadata (Just "System-call error types")}
  where
    definitions = [
      systemError]

systemError :: TypeDefinition
systemError = define "SystemError" $
  doc ("A recoverable failure to launch a process or to perform a hydra.lib.system system call, named"
    ++ " after the POSIX errno values the host reports. A child that launches successfully and then"
    ++ " exits non-zero is not a SystemError; it is a ProcessResult with a non-zero exitCode") $
  T.union [
    "commandNotFound">:
      doc ("POSIX ENOENT: the executable named by Command.program does not exist or is not found on"
        ++ " PATH. Reported by the exec family (XSH,"
        ++ " https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html)")
      File.filePath,
    "permissionDenied">:
      doc ("POSIX EACCES: the executable exists but is not executable, or a path component denies"
        ++ " search permission")
      File.filePath,
    "invalidWorkingDirectory">:
      doc ("POSIX ENOENT or ENOTDIR raised by the implied chdir() to Command.workingDirectory")
      File.filePath,
    "interrupted">:
      doc ("The call was interrupted before the child produced a result (a portable abstraction over"
        ++ " POSIX EINTR and host-level kills); the child's disposition is unknown")
      T.unit,
    "other">:
      doc ("Any other failure, carrying the host-provided error message verbatim (covers errno values"
        ++ " not modeled above, such as EMFILE, ENOMEM, or E2BIG)")
      T.string]
