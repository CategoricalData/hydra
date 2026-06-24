-- | Primitive declarations for the hydra.lib.system namespace.

module Hydra.Sources.Kernel.Lib.System where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import           Hydra.Dsl.Meta.Phantoms as Phantoms hiding (exit)
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Dsl.Types         (effect)
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), exit)


ns :: ModuleName
ns = ModuleName "hydra.lib.system"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.system module.")}
  where
    definitions = [execute, exit, getEnvironment, getEnvironmentVariable, getTime, getWorkingDirectory]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_

-- Named type aliases
command, environmentVariable, filePath, processResult, statusCode, systemError, timespec :: Type
command            = TypeVariable (Name "hydra.system.Command")
environmentVariable = TypeVariable (Name "hydra.system.EnvironmentVariable")
filePath           = TypeVariable (Name "hydra.file.FilePath")
processResult      = TypeVariable (Name "hydra.system.ProcessResult")
statusCode         = TypeVariable (Name "hydra.system.StatusCode")
systemError        = TypeVariable (Name "hydra.error.system.SystemError")
timespec           = TypeVariable (Name "hydra.time.Timespec")

execute :: PrimitiveDefinition
execute = define "execute" "Run a program to completion and capture its result."
  (sig $ TypeScheme [] (command Types.~> effect (Types.either_ systemError processResult)) Nothing)
  ["execute(command) describes an effectful computation which runs the program described by command\
  \ to completion and returns its result. This is the POSIX \"execute a command\" operation at the\
  \ posix_spawn / system() altitude\
  \ (https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html,\
  \ https://pubs.opengroup.org/onlinepubs/9799919799/functions/system.html): the child is spawned,\
  \ Hydra waits for it (POSIX waitpid), and standard output and standard error are captured as raw\
  \ bytes. Output is byte-oriented, with no character decoding or newline translation; decode it to\
  \ text via hydra.lib.text.decodeUtf8. A child that runs and exits with a non-zero status (POSIX\
  \ WEXITSTATUS) is returned as right(result) with that StatusCode, following the XCU section 2.8.2\
  \ exit-status convention. Only a failure to launch -- for example POSIX ENOENT, EACCES, or a bad\
  \ working directory -- is returned as left(error). Unlike the shell system(), no intermediate\
  \ shell is invoked; command.program is executed directly (as the execvp family does), so shell\
  \ syntax in arguments is not interpreted."]

exit :: PrimitiveDefinition
exit = define "exit" "Terminate the current process with a status code."
  (sig $ TypeScheme [] (statusCode Types.~> effect Types.unit) Nothing)
  ["exit(code) describes an effectful computation which terminates the current process immediately\
  \ with the given status, exactly as the POSIX exit() function (XSH,\
  \ https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html). The status is reported\
  \ to the parent through wait(); by the XCU section 2.8.2 convention, 0 denotes success and\
  \ non-zero denotes failure. This computation does not return, so subsequent effects in a sequence\
  \ are not performed."]

getEnvironment :: PrimitiveDefinition
getEnvironment = define "getEnvironment" "Get the full set of environment variables."
  (sig $ TypeScheme [] (effect (Types.map environmentVariable Types.string)) Nothing)
  ["getEnvironment describes an effectful computation which returns the entire environment of the\
  \ current process -- the POSIX environment list environ (XBD section 8, Environment Variables,\
  \ https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html) -- as a map from\
  \ variable name to value. POSIX represents each entry as a \"name=value\" string; this primitive\
  \ splits each at the first '=' into an EnvironmentVariable key and a string value. This\
  \ computation does not fail; an empty environment yields an empty map."]

getEnvironmentVariable :: PrimitiveDefinition
getEnvironmentVariable = define "getEnvironmentVariable" "Look up a single environment variable by name."
  (sig $ TypeScheme [] (environmentVariable Types.~> effect (Types.optional Types.string)) Nothing)
  ["getEnvironmentVariable(name) describes an effectful computation which returns the value of the\
  \ environment variable name, or none if it is not present -- the POSIX getenv() function (XSH,\
  \ https://pubs.opengroup.org/onlinepubs/9799919799/functions/getenv.html). A variable that is\
  \ present but set to the empty string is returned as given(\"\"), distinct from the none that\
  \ POSIX getenv signals with a null pointer. This computation does not fail."]

getTime :: PrimitiveDefinition
getTime = define "getTime" "Get the current wall-clock time."
  (sig $ TypeScheme [] (effect timespec) Nothing)
  ["getTime describes an effectful computation which returns the current wall-clock time as a\
  \ Timespec (seconds and nanoseconds since the Unix Epoch) -- the POSIX clock_gettime() function\
  \ with the CLOCK_REALTIME clock (XSH,\
  \ https://pubs.opengroup.org/onlinepubs/9799919799/functions/clock_gettime.html). The actual\
  \ resolution is implementation- and platform-defined and may be coarser than nanoseconds. As with\
  \ CLOCK_REALTIME, the clock is not monotonic: it can jump or move backwards when the host's wall\
  \ clock is adjusted. This computation does not fail."]

getWorkingDirectory :: PrimitiveDefinition
getWorkingDirectory = define "getWorkingDirectory" "Get the current working directory."
  (sig $ TypeScheme [] (effect (Types.either_ systemError filePath)) Nothing)
  ["getWorkingDirectory describes an effectful computation which returns the absolute pathname of\
  \ the current working directory -- the POSIX getcwd() function (XSH,\
  \ https://pubs.opengroup.org/onlinepubs/9799919799/functions/getcwd.html) -- as a FilePath. A\
  \ recoverable failure (for example POSIX EACCES, or the working directory having been removed) is\
  \ returned as left(error); success is returned as right(path)."]
