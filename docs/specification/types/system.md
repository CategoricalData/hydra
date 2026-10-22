<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.system

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.system.Command

*(undocumented)*

`∀.record{program:hydra.core.file.FilePath@{description="The executable to run; a POSIX pathname, resolved against PATH by the host when it contains no slash (as for the execvp/execlp family). POSIX argv[0] (the program name) is supplied by the host from this field"}, arguments:list<string>@{description="The arguments following the program name, becoming argv[1..] for the child. The program name (argv[0]) is not included here"}, workingDirectory:optional<hydra.core.file.FilePath>@{description="The directory in which to run the child, as if chdir() (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/chdir.html) were called before exec. None inherits the parent's working directory"}, environment:optional<map<hydra.core.system.EnvironmentVariable, string>>@{description="The complete environment for the child (POSIX environ). None inherits the parent's environment; given(m) replaces it entirely, as execve's envp argument does. There is no partial-merge form; merge in pure code before calling"}}@{description="A description of a program to run, supplying the inputs the POSIX posix_spawn / execve family takes: the executable, its argument vector, and optionally a working directory and a replacement environment. Only the portable subset is modeled; POSIX file actions, spawn attributes, and signal masks are omitted. See https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html and https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html"}`

## hydra.core.system.EnvironmentVariable

*(undocumented)*

`∀.wrap(string)@{description="The name of an environment variable: its identity within the POSIX environment list environ (XBD section 8, Environment Variables, https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html). POSIX models each entry as a \"name=value\" string; this type wraps the name, which is the constrained, identifying part: XBD section 8 requires names to consist of characters from the portable character set and to contain no '=' (the name/value separator). A variable's value is an arbitrary string and is left unwrapped"}`

## hydra.core.system.ProcessResult

*(undocumented)*

`∀.record{exitCode:hydra.core.system.StatusCode@{description="The child's exit status (POSIX WEXITSTATUS); 0 denotes success by convention"}, stdout:binary@{description="The bytes the child wrote to file descriptor 1, standard output (XBD STDOUT_FILENO)"}, stderr:binary@{description="The bytes the child wrote to file descriptor 2, standard error (XBD STDERR_FILENO)"}}@{description="The outcome of a child process that ran to completion, as obtained by wait() / waitpid() (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/wait.html) together with its captured output. Only normal termination is modeled directly; abnormal termination (POSIX WIFSIGNALED) is surfaced through exitCode using the host's convention rather than as a separate field"}`

## hydra.core.system.StatusCode

*(undocumented)*

`∀.wrap(int32)@{description="A process exit status: the value passed to the POSIX exit() function and reported by wait() / waitpid() for a normally-terminated child (XSH; see https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html and https://pubs.opengroup.org/onlinepubs/9799919799/functions/wait.html). By the convention of XCU section 2.8.2, Exit Status for Commands (https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_19_08_02), 0 denotes success and non-zero denotes failure. POSIX passes only the low 8 bits of the status through wait(); Hydra widens this to a signed int32 so that host runtimes which expose a fuller code (e.g. Windows process exit codes, or the negative \"killed by signal N\" convention) can be represented without loss"}`

# hydra.core.error.system

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.error.system.SystemError

*(undocumented)*

`∀.union{commandNotFound:hydra.core.file.FilePath@{description="POSIX ENOENT: the executable named by Command.program does not exist or is not found on PATH. Reported by the exec family (XSH, https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html)"}, permissionDenied:hydra.core.file.FilePath@{description="POSIX EACCES: the executable exists but is not executable, or a path component denies search permission"}, invalidWorkingDirectory:hydra.core.file.FilePath@{description="POSIX ENOENT or ENOTDIR raised by the implied chdir() to Command.workingDirectory"}, interrupted:unit@{description="The call was interrupted before the child produced a result (a portable abstraction over POSIX EINTR and host-level kills); the child's disposition is unknown"}, other:string@{description="Any other failure, carrying the host-provided error message verbatim (covers errno values not modeled above, such as EMFILE, ENOMEM, or E2BIG)"}}@{description="A recoverable failure to launch a process or to perform a hydra.core.lib.system system call, named after the POSIX errno values the host reports. A child that launches successfully and then exits non-zero is not a SystemError; it is a ProcessResult with a non-zero exitCode"}`
