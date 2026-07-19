<!-- NOTE: this page will be automatically generated from the kernel type definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/System.hs and
     Types/Error/System.hs (generator not yet built). Hand-authored draft under #417.
     The types/ pages document kernel types as needed by the primitive specifications;
     coverage is driven by those needs, not completeness. -->

# System types

Kernel types used by the [hydra.lib.system](../primitives/system.md) primitives.
These types live in the `hydra.system` and `hydra.error.system` namespaces.

## Command

A description of a program to run, supplying the inputs the POSIX `posix_spawn`/`execve` family
takes: the executable, its argument vector, and optionally a working directory and a replacement
environment.
Only the portable subset is modeled; POSIX file actions, spawn attributes, and signal masks are
omitted.
A record with the following fields:

| Field | Type | Meaning |
|---|---|---|
| `program` | `FilePath` | the executable to run; a POSIX pathname, resolved against `PATH` by the host when it contains no slash; the child's `argv[0]` is supplied by the host from this field |
| `arguments` | `list<string>` | the arguments following the program name, becoming `argv[1..]` for the child; the program name itself is not included |
| `workingDirectory` | `optional<FilePath>` | the directory in which to run the child, as if `chdir` were called before exec; `none` inherits the parent's working directory |
| `environment` | `optional<map<EnvironmentVariable, string>>` | the complete environment for the child; `none` inherits the parent's environment; a `given` map replaces it entirely (there is no partial-merge form; merge in pure code before calling) |

## EnvironmentVariable

The name of an environment variable: its identity within the POSIX environment list `environ`.
POSIX models each entry as a `name=value` string; this type wraps the name, which must consist
of characters from the portable character set and contain no `=`.
A wrapped `string`.

## ProcessResult

The outcome of a child process that ran to completion, together with its captured output.
Only normal termination is modeled directly; abnormal termination (POSIX `WIFSIGNALED`) is
surfaced through `exitCode` using the host's convention rather than as a separate field.
A record with the following fields:

| Field | Type | Meaning |
|---|---|---|
| `exitCode` | `StatusCode` | the child's exit status (POSIX `WEXITSTATUS`); `0` denotes success by convention |
| `stdout` | `binary` | the bytes the child wrote to standard output |
| `stderr` | `binary` | the bytes the child wrote to standard error |

## StatusCode

A process exit status: the value passed to the POSIX `exit` function and reported for a
normally-terminated child.
By POSIX convention, `0` denotes success and any non-zero value denotes failure.
A wrapped `int32`.

## SystemError

A recoverable failure to launch a process or to perform a `hydra.lib.system` system call, named
after the POSIX `errno` values the host reports.
A child that launches successfully and then exits non-zero is not a `SystemError`; it is a
`ProcessResult` with a non-zero `exitCode`.
A union with the following variants:

| Variant | Carries | Meaning |
|---|---|---|
| `commandNotFound` | `FilePath` | POSIX `ENOENT`: the executable does not exist or is not found on `PATH` |
| `permissionDenied` | `FilePath` | POSIX `EACCES`: the executable exists but is not executable, or a path component denies search permission |
| `invalidWorkingDirectory` | `FilePath` | POSIX `ENOENT`/`ENOTDIR` raised by the implied `chdir` to the working directory |
| `interrupted` | `unit` | the call was interrupted before the child produced a result; the child's disposition is unknown |
| `other` | `string` | any other failure, carrying the host-provided error message verbatim |
