-- Note: this is an automatically generated file. Do not edit.
-- | System-call error types

module Hydra.Error.System where
import qualified Hydra.Core as Core
import qualified Hydra.File as File
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A recoverable failure to launch a process or to perform a hydra.lib.system system call, named after the POSIX errno values the host reports. A child that launches successfully and then exits non-zero is not a SystemError; it is a ProcessResult with a non-zero exitCode
data SystemError =
  -- | POSIX ENOENT: the executable named by Command.program does not exist or is not found on PATH. Reported by the exec family (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html)
  SystemErrorCommandNotFound File.FilePath |
  -- | POSIX EACCES: the executable exists but is not executable, or a path component denies search permission
  SystemErrorPermissionDenied File.FilePath |
  -- | POSIX ENOENT or ENOTDIR raised by the implied chdir() to Command.workingDirectory
  SystemErrorInvalidWorkingDirectory File.FilePath |
  -- | The call was interrupted before the child produced a result (a portable abstraction over POSIX EINTR and host-level kills); the child's disposition is unknown
  SystemErrorInterrupted |
  -- | Any other failure, carrying the host-provided error message verbatim (covers errno values not modeled above, such as EMFILE, ENOMEM, or E2BIG)
  SystemErrorOther String
  deriving (Eq, Ord, Read, Show)
_SystemError = Core.Name "hydra.error.system.SystemError"
_SystemError_commandNotFound = Core.Name "commandNotFound"
_SystemError_permissionDenied = Core.Name "permissionDenied"
_SystemError_invalidWorkingDirectory = Core.Name "invalidWorkingDirectory"
_SystemError_interrupted = Core.Name "interrupted"
_SystemError_other = Core.Name "other"
