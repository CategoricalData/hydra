-- Note: this is an automatically generated file. Do not edit.

-- | A model for a program's interface to the host operating system: running other programs, the environment, the working directory, and process exit status. Names and semantics follow the POSIX System Interfaces (XSH) volume; see https://pubs.opengroup.org/onlinepubs/9799919799. Only the portable, cross-platform subset is modeled.

module Hydra.System where

import qualified Hydra.Core as Core
import qualified Hydra.File as File
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Map as M

-- | A description of a program to run, supplying the inputs the POSIX posix_spawn / execve family takes: the executable, its argument vector, and optionally a working directory and a replacement environment. Only the portable subset is modeled; POSIX file actions, spawn attributes, and signal masks are omitted. See https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html and https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html
data Command =
  Command {
    -- | The executable to run; a POSIX pathname, resolved against PATH by the host when it contains no slash (as for the execvp/execlp family). POSIX argv[0] (the program name) is supplied by the host from this field
    commandProgram :: File.FilePath,
    -- | The arguments following the program name, becoming argv[1..] for the child. The program name (argv[0]) is not included here
    commandArguments :: [String],
    -- | The directory in which to run the child, as if chdir() (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/chdir.html) were called before exec. None inherits the parent's working directory
    commandWorkingDirectory :: (Maybe File.FilePath),
    -- | The complete environment for the child (POSIX environ). None inherits the parent's environment; given(m) replaces it entirely, as execve's envp argument does. There is no partial-merge form; merge in pure code before calling
    commandEnvironment :: (Maybe (M.Map EnvironmentVariable String))}
  deriving (Eq, Ord, Read, Show)

_Command = Core.Name "hydra.system.Command"

_Command_program = Core.Name "program"

_Command_arguments = Core.Name "arguments"

_Command_workingDirectory = Core.Name "workingDirectory"

_Command_environment = Core.Name "environment"

-- | The name of an environment variable: its identity within the POSIX environment list environ (XBD section 8, Environment Variables, https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html). POSIX models each entry as a "name=value" string; this type wraps the name, which is the constrained, identifying part: XBD section 8 requires names to consist of characters from the portable character set and to contain no '=' (the name/value separator). A variable's value is an arbitrary string and is left unwrapped
newtype EnvironmentVariable =
  EnvironmentVariable {
    unEnvironmentVariable :: String}
  deriving (Eq, Ord, Read, Show)

_EnvironmentVariable = Core.Name "hydra.system.EnvironmentVariable"

-- | The outcome of a child process that ran to completion, as obtained by wait() / waitpid() (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/wait.html) together with its captured output. Only normal termination is modeled directly; abnormal termination (POSIX WIFSIGNALED) is surfaced through exitCode using the host's convention rather than as a separate field
data ProcessResult =
  ProcessResult {
    -- | The child's exit status (POSIX WEXITSTATUS); 0 denotes success by convention
    processResultExitCode :: StatusCode,
    -- | The bytes the child wrote to file descriptor 1, standard output (XBD STDOUT_FILENO)
    processResultStdout :: B.ByteString,
    -- | The bytes the child wrote to file descriptor 2, standard error (XBD STDERR_FILENO)
    processResultStderr :: B.ByteString}
  deriving (Eq, Ord, Read, Show)

_ProcessResult = Core.Name "hydra.system.ProcessResult"

_ProcessResult_exitCode = Core.Name "exitCode"

_ProcessResult_stdout = Core.Name "stdout"

_ProcessResult_stderr = Core.Name "stderr"

-- | A process exit status: the value passed to the POSIX exit() function and reported by wait() / waitpid() for a normally-terminated child (XSH; see https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html and https://pubs.opengroup.org/onlinepubs/9799919799/functions/wait.html). By the convention of XCU section 2.8.2, Exit Status for Commands (https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_19_08_02), 0 denotes success and non-zero denotes failure. POSIX passes only the low 8 bits of the status through wait(); Hydra widens this to a signed int32 so that host runtimes which expose a fuller code (e.g. Windows process exit codes, or the negative "killed by signal N" convention) can be represented without loss
newtype StatusCode =
  StatusCode {
    unStatusCode :: Int}
  deriving (Eq, Ord, Read, Show)

_StatusCode = Core.Name "hydra.system.StatusCode"
