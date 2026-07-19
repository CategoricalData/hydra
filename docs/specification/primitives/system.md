<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/System.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.system

Process and environment access.
Every primitive in this module is effectful: it describes a computation against the ambient
state of the running process — its environment, its working directory, the system clock, child
processes — and executing that computation is separate from describing it.
The types used here — [Command](../types/system.md#command),
<!-- [PENDING types page] -->
[ProcessResult](../types/system.md#processresult), [StatusCode](../types/system.md#statuscode),
[EnvironmentVariable](../types/system.md#environmentvariable),
[SystemError](../types/system.md#systemerror), [Timespec](../types/time.md#timespec),
[FilePath](../types/files.md#filepath) — are documented in the types pages.

#### execute — **Draft**

`Command → effect<either<SystemError, ProcessResult>>`

Usage: `execute command`

Run a program to completion and capture its result.
Describes an effectful computation which runs the program described by `command` to completion
and returns its result as a [ProcessResult](../types/system.md#processresult).
The child process is spawned, Hydra waits for it to terminate (as with POSIX `waitpid`), and
its standard output and standard error are captured as raw bytes.
Output is byte-oriented, with no character decoding or newline translation; decode it to text
with `hydra.lib.text.decodeUtf8`.
A child that launches and then exits with a non-zero status is not an error: it is returned as
`right result` carrying that [StatusCode](../types/system.md#statuscode), following the POSIX
exit-status convention (0 denotes success, non-zero denotes failure).
Only a failure to launch — for example a missing program, insufficient permissions, or a bad
working directory — is returned as `left` of a
[SystemError](../types/system.md#systemerror).
No intermediate shell is invoked: the command's program is executed directly, so shell syntax
in arguments is not interpreted.

Effectful: process control — spawns and waits for a child process.

Since: 0.17

#### exit — **Draft**

`StatusCode → effect<unit>`

Usage: `exit code`

Terminate the current process with a status code.
Describes an effectful computation which terminates the current process immediately with the
status `code`, as with the POSIX `exit` function.
The status is reported to the parent process; by the POSIX convention, 0 denotes success and
non-zero denotes failure.
This computation never returns normally: subsequent effects in a sequence are not performed.

Effectful: process control — terminates the current process.

Since: 0.17

#### getEnvironment — **Draft**

`effect<map<EnvironmentVariable, string>>`

Usage: `getEnvironment`

Get the full set of environment variables.
Describes an effectful computation which returns the entire environment of the current process
(the POSIX environment list, `environ`) as a map from variable name to value.
POSIX represents each entry as a single `name=value` string; this primitive splits each entry
at the first `=` into an [EnvironmentVariable](../types/system.md#environmentvariable) key and
a string value.
This computation does not fail; an empty environment yields an empty map.

Effectful: reads the process environment.

Since: 0.17

#### getEnvironmentVariable — **Draft**

`EnvironmentVariable → effect<optional<string>>`

Usage: `getEnvironmentVariable name`

Look up a single environment variable by name.
Describes an effectful computation which returns the value of the environment variable `name`,
or `none` if it is not set (the POSIX `getenv` function).
A variable that is present but set to the empty string is returned as `given ""`, distinct from
`none`.
This computation does not fail.

Effectful: reads the process environment.

Since: 0.17

#### getTime — **Draft**

`effect<Timespec>`

Usage: `getTime`

Get the current wall-clock time.
Describes an effectful computation which returns the current wall-clock time as a
[Timespec](../types/time.md#timespec) — seconds and nanoseconds since the Unix Epoch — as with
the POSIX `clock_gettime` function on the real-time clock.
The actual resolution is implementation- and platform-defined and may be coarser than
nanoseconds.
The clock is not monotonic: it can jump or move backwards when the host's wall clock is
adjusted.
This computation does not fail.

Effectful: reads the system clock.

Since: 0.17

#### getWorkingDirectory — **Draft**

`effect<either<SystemError, FilePath>>`

Usage: `getWorkingDirectory`

Get the current working directory.
Describes an effectful computation which returns the absolute pathname of the current working
directory (the POSIX `getcwd` function) as a [FilePath](../types/files.md#filepath).
A recoverable failure — for example insufficient permissions, or the working directory having
been removed — is returned as `left` of a [SystemError](../types/system.md#systemerror);
success is returned as `right path`.

Effectful: reads process state (the working directory).

Since: 0.17
