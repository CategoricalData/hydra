<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.system

<!-- Note: this is an automatically generated file. Do not edit. -->

Process and environment access.

Every primitive in this module is effectful: it describes a computation against the ambient state of the running process -- its environment, its working directory, the system clock, child processes -- and executing that computation is separate from describing it.

## hydra.core.lib.system.execute

Run a program to completion and capture its result.

`∀.hydra.core.system.Command → effect<either<hydra.core.error.system.SystemError, hydra.core.system.ProcessResult>>`

*Since: 0.17.*

execute(command) describes an effectful computation which runs the program described by command to completion and returns its result. This is the POSIX "execute a command" operation at the posix_spawn / system() altitude (https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html, https://pubs.opengroup.org/onlinepubs/9799919799/functions/system.html): the child is spawned, Hydra waits for it (POSIX waitpid), and standard output and standard error are captured as raw bytes. Output is byte-oriented, with no character decoding or newline translation; decode it to text via hydra.core.lib.text.decodeUtf8. A child that runs and exits with a non-zero status (POSIX WEXITSTATUS) is returned as right(result) with that StatusCode, following the XCU section 2.8.2 exit-status convention. Only a failure to launch -- for example POSIX ENOENT, EACCES, or a bad working directory -- is returned as left(error). Unlike the shell system(), no intermediate shell is invoked; command.program is executed directly (as the execvp family does), so shell syntax in arguments is not interpreted.

**[HYDRA-CORE-LIB-SYSTEM-EXECUTE-CAPTURES-STDOUT-BYTE-EXACTLY] (requirement) **execute captures the child program's standard output as raw bytes, byte-exactly, with no newline translation, including the full argument vector being passed through to the child in order.

**[HYDRA-CORE-LIB-SYSTEM-EXECUTE-CAPTURES-STDERR-SEPARATELY] (requirement) **execute captures the child program's standard error separately from standard output, so diagnostics written to stderr do not leak into the captured stdout.

**[HYDRA-CORE-LIB-SYSTEM-EXECUTE-REPORTS-EXIT-STATUS-AS-RIGHT] (requirement) **execute reports a child's exit status as right(result), for both a zero (successful) and a non-zero exit status -- a non-zero exit is a normal, successful launch outcome, not a SystemError.

**[HYDRA-CORE-LIB-SYSTEM-EXECUTE-HONORS-WORKING-DIRECTORY-AND-ENVIRONMENT-FIELDS] (requirement) **execute honors the command's workingDirectory field (running the child with that directory as its current directory) and its environment field (replacing the child's environment with exactly the given bindings, not merely augmenting it).

**[HYDRA-CORE-LIB-SYSTEM-EXECUTE-LEFT-COMMAND-NOT-FOUND-ON-MISSING-PROGRAM] (requirement) **execute returns left(commandNotFound) when the program does not exist, distinguishing a launch failure from a program that ran and exited non-zero.

## hydra.core.lib.system.exit

Terminate the current process with a status code.

`∀.hydra.core.system.StatusCode → effect<unit>`

*Since: 0.17.*

exit(code) describes an effectful computation which terminates the current process immediately with the given status, exactly as the POSIX exit() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html). The status is reported to the parent through wait(); by the XCU section 2.8.2 convention, 0 denotes success and non-zero denotes failure. This computation does not return, so subsequent effects in a sequence are not performed.

## hydra.core.lib.system.getEnvironment

Get the full set of environment variables.

`∀.effect<map<hydra.core.system.EnvironmentVariable, string>>`

*Since: 0.17.*

getEnvironment describes an effectful computation which returns the entire environment of the current process -- the POSIX environment list environ (XBD section 8, Environment Variables, https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html) -- as a map from variable name to value. POSIX represents each entry as a "name=value" string; this primitive splits each at the first '=' into an EnvironmentVariable key and a string value. This computation does not fail; an empty environment yields an empty map.

**[HYDRA-CORE-LIB-SYSTEM-GET-ENVIRONMENT-RETURNS-EXACT-ENVIRONMENT-CONTENTS] (requirement) **getEnvironment returns a map containing exactly the current process's environment, with no entries for variables that are not actually set (observed by the absence of a guaranteed-unset variable name).

## hydra.core.lib.system.getEnvironmentVariable

Look up a single environment variable by name.

`∀.hydra.core.system.EnvironmentVariable → effect<optional<string>>`

*Since: 0.17.*

getEnvironmentVariable(name) describes an effectful computation which returns the value of the environment variable name, or none if it is not present -- the POSIX getenv() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/getenv.html). A variable that is present but set to the empty string is returned as given(""), distinct from the none that POSIX getenv signals with a null pointer. This computation does not fail.

**[HYDRA-CORE-LIB-SYSTEM-GET-ENVIRONMENT-VARIABLE-NONE-FOR-ABSENT-VARIABLE] (requirement) **getEnvironmentVariable returns none when the named environment variable is not set.

## hydra.core.lib.system.getTime

Get the current wall-clock time.

`∀.effect<hydra.core.time.Timespec>`

*Since: 0.17.*

getTime describes an effectful computation which returns the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix Epoch) -- the POSIX clock_gettime() function with the CLOCK_REALTIME clock (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/clock_gettime.html). The actual resolution is implementation- and platform-defined and may be coarser than nanoseconds. As with CLOCK_REALTIME, the clock is not monotonic: it can jump or move backwards when the host's wall clock is adjusted. This computation does not fail.

**[HYDRA-CORE-LIB-SYSTEM-GET-TIME-RETURNS-POSITIVE-EPOCH-SECONDS] (requirement) **getTime returns a Timespec whose seconds field is a positive number of seconds since the Unix epoch.

**[HYDRA-CORE-LIB-SYSTEM-GET-TIME-RETURNS-NANOSECONDS-IN-VALID-RANGE] (requirement) **getTime returns a Timespec whose nanoseconds field is within the documented range [0, 1000000000).

## hydra.core.lib.system.getWorkingDirectory

Get the current working directory.

`∀.effect<either<hydra.core.error.system.SystemError, hydra.core.file.FilePath>>`

*Since: 0.17.*

getWorkingDirectory describes an effectful computation which returns the absolute pathname of the current working directory -- the POSIX getcwd() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/getcwd.html) -- as a FilePath. A recoverable failure (for example POSIX EACCES, or the working directory having been removed) is returned as left(error); success is returned as right(path).

**[HYDRA-CORE-LIB-SYSTEM-GET-WORKING-DIRECTORY-RETURNS-NON-EMPTY-PATH] (requirement) **getWorkingDirectory returns a non-empty absolute path for the current working directory.

## hydra.core.lib.system.readStdin

Read the complete contents of standard input as raw bytes.

`∀.effect<either<hydra.core.error.system.SystemError, binary>>`

*Since: 0.17.6.*

readStdin describes an effectful computation which reads standard input (POSIX file descriptor 0) until end-of-file and returns the complete contents as raw bytes, with no character decoding or newline translation -- the POSIX read() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/read.html) applied repeatedly to fd 0 until it signals end-of-file. To interpret the result as text, decode it (e.g. via hydra.core.lib.text.decodeUtf8). A recoverable I/O failure is returned as left(error); success is returned as right(contents), including the empty byte string if stdin is closed or empty.

## hydra.core.lib.system.writeStderr

Write raw bytes to standard error.

`∀.binary → effect<either<hydra.core.error.system.SystemError, unit>>`

*Since: 0.17.6.*

writeStderr(bytes) describes an effectful computation which writes bytes to standard error (POSIX file descriptor 2), with no character encoding or newline translation -- the POSIX write() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/write.html) applied repeatedly to fd 2 until all bytes are written. To write text, encode it first (e.g. via hydra.core.lib.text.encodeUtf8). A recoverable I/O failure (for example a closed pipe, POSIX EPIPE) is returned as left(error); success is returned as right(unit).

## hydra.core.lib.system.writeStdout

Write raw bytes to standard output.

`∀.binary → effect<either<hydra.core.error.system.SystemError, unit>>`

*Since: 0.17.6.*

writeStdout(bytes) describes an effectful computation which writes bytes to standard output (POSIX file descriptor 1), with no character encoding or newline translation -- the POSIX write() function (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/write.html) applied repeatedly to fd 1 until all bytes are written. To write text, encode it first (e.g. via hydra.core.lib.text.encodeUtf8). A recoverable I/O failure (for example a closed pipe, POSIX EPIPE) is returned as left(error); success is returned as right(unit).
