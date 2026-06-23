# Design proposal: `hydra.lib.system` — an effectful system-call library

**Issue:** #498 (create an effectful `hydra.lib.system` primitive library).
**North star:** #416 — promote Hydra's Bash/Python build system into Hydra itself.
**Status:** design locked; **implemented and green on all 8 hosts** (Haskell, Python, Java, Scala,
Clojure, Common Lisp, Emacs Lisp, Scheme), with 15 effectful system test cases (error paths, separate
stdout/stderr capture, non-zero exit codes, working directory, replacement environment, clock range).
Scheme needed a guile-specific workaround: run a replacement environment via `env -i NAME=VAL ...` rather
than mutating the global `environ` (the latter SIGSEGVs guile in-process), and reach generated record
constructors/accessors by eval-in-module rather than importing the type module.

This document is a sketch / in-flight proposal — it lives outside the public documentation surface
(not the wiki, not `docs/`) per the CLAUDE.md three-tier rule.

---

## 1. Goal and altitude

`hydra.lib.system` is the third effectful library, joining `hydra.lib.effects` (the effect monad
combinators) and `hydra.lib.files` (filesystem I/O). It is **Hydra's "System Interfaces" library**, in
the sense of the POSIX XSH (System Interfaces) volume — the same volume that holds both `system()` /
`getenv` *and* `clock_gettime`.

**Charter / inclusion criterion.** A primitive belongs in `hydra.lib.system` if it is a *service the
host environment provides to a running program*. That covers: executing other programs, reading the
environment and the program's own arguments, the working directory, terminating the process, and
**the system clock**. Clock time is a system service, so `getTime` is a charter member, not an awkward
guest. A future host-provided service such as geoposition would belong here for the same reason. What
does *not* belong: the filesystem (that is `hydra.lib.files`) and the process's own standard streams
(that is the future `hydra.lib.io`).

**Altitude decision (agreed): a portable POSIX subset, modeled the way `hydra.file` models
`struct stat`.** POSIX is the conceptual anchor for *naming and semantics*, but we model only the
cross-platform subset that survives translation to all eight hosts (Haskell, Java, Python, Scala, and
the four Lisp dialects) **and** to non-Unix platforms like Windows — which is the whole payoff of #416
(no more `*nix`-via-Bash dependency). We do **not** model `fork`/`exec`/`wait`/signals/`ptrace`
literally; those don't map onto the JVM or Python's `subprocess` cleanly and would betray the
portability goal.

This mirrors the precedent already set in the kernel: `hydra.file.FileStatus` cites the Open Group
POSIX base specification (Issue 8, `pubs.opengroup.org/onlinepubs/9799919799`) and explicitly keeps
only the portable subset of `struct stat`, dropping `st_dev`, `st_ino`, `st_uid`, etc.

### POSIX references this library draws on

| Concept | POSIX reference | What we model |
|---|---|---|
| Run a program | Shell & Utilities; `posix_spawn` / `system()` semantics | run-to-completion subprocess with args, cwd, env |
| Environment | `getenv` (XSH); `environ` (XBD §8) | read one var; read all vars |
| Working directory | `getcwd` (XSH) | read cwd |
| Program arguments | `argv` (XBD §12.1 utility argument syntax) | read this process's args |
| Exit | `exit` / `_exit` (XSH); exit-status conventions (XCU §2.8.2) | terminate with an integer status |

We deliberately stay at the `system()`/`posix_spawn` altitude (a child runs to completion and yields a
status plus captured output), not the `fork`+`execve`+`waitpid` altitude.

---

## 2. What a *language* needs (beyond just this build system)

The build scripts (#416 endpoint) tell us the concrete minimum. A survey of `bin/*.sh`:

- **Subprocess execution dominates**: `stack` (63), `gradle` (14), `sbt` (17), `python`/`python3`
  (~280), `clojure` (44), `emacs` (51), `guile`, `sbcl`, `npx`. Every one is: launch with args + cwd +
  env, wait, branch on **exit code** (`$?` / `set -e` / `exit N` appear ~97 times).
- **Environment variables** are pervasive: `HYDRA_ROOT` (105), `HYDRA_ROOT_DIR` (45), `JAVA_HOME`,
  `PATH`, `HOME`, plus many `HYDRA_*` sentinels and `*_VERSION` pins.
- **Working directory** matters (`cd`/`pushd`/`popd` per-package).
- A few scripts **read stdin** (`check-env.sh`, `sync.sh`, `test.sh`) for interactive confirmation.

But #498 asks for a *broadly useful language library*, not a build-script shim. The durable core that
(a) every host already provides and (b) any program — not just a build — wants is small:

1. **Process execution** — run a command, get exit status + captured stdout/stderr.
2. **Environment** — read one variable; read the whole environment.
3. **Working directory** — read it (mutation is rare and process-global; see §5).
4. **Exit** — terminate with a status code.
5. **System clock** — `getTime`. A host-provided service (per the §1 charter), needed by any build for
   timestamps (the scripts use `date`).

What we deliberately **exclude** (and why):
- **Filesystem mutation** — already `hydra.lib.files`. No overlap.
- **Program arguments** — a Hydra program is *handed* its arguments as an entrypoint parameter (POSIX
   gives `argv` no accessor either; it is a parameter to `main()`). The host's generated `main` shim
   passes them in. So there is no `getArguments` primitive — argument-passing stays pure data flow.
- **Standard streams** (stdin/stdout/stderr) — a different concern (the process's own streams, not a
   host *service*); deferred to a future `hydra.lib.io`.
- **fork / signals / process groups / pipes-between-children** — host-divergent, non-portable.
- **Async / streaming subprocess I/O** — v2. Run-to-completion first (matches `system()`).

---

## 3. Proposed primitive surface (v1)

Following the `hydra.lib.files` shape exactly: `primImpure = Phantoms.impurePrimitiveInModule ns`;
fallible calls return `effect<either<SystemError, t>>`; byte-oriented I/O uses `binary`, with text via
`hydra.lib.text.encodeUtf8` / `decodeUtf8`.

| Primitive | Signature | Notes / POSIX anchor |
|---|---|---|
| `execute` | `Command -> effect<either<SystemError, ProcessResult>>` | run to completion; capture stdout/stderr as `binary`. POSIX `system()` ("execute a command"); `left` only on *launch* failure, `right` with non-zero `StatusCode` when the child ran and failed. |
| `getEnvironment` | `effect<map<EnvironmentVariable, string>>` | the full environment (`environ`), name → value. Infallible. |
| `getEnvironmentVariable` | `EnvironmentVariable -> effect<optional<string>>` | look up one variable by name (`getenv`); `none` if unset. Infallible. |
| `getWorkingDirectory` | `effect<either<SystemError, FilePath>>` | `getcwd`. Reuses `hydra.file.FilePath`. |
| `getTime` | `effect<Timespec>` | current wall-clock time since the Unix epoch. Reuses `hydra.time.Timespec`. Infallible. |
| `exit` | `StatusCode -> effect<unit>` | terminate with status (`exit`). Does not return. |

`getEnvironment` / `getEnvironmentVariable` / `getTime` are modeled as **infallible**
(`effect<t>`, not `effect<either<...>>`): reading the environment and the clock does not fail on
any host. `exit` is likewise `effect<unit>` (its "failure" is that the process is gone).

**No `getArguments`.** A Hydra program does not *query* for its command-line arguments; POSIX gives
`argv` no accessor either — it is a parameter to `main()`. Arguments are therefore modeled as **a
parameter to a Hydra program's entrypoint**, supplied by the host's generated `main` shim (which lives
in `heads/`, not in `hydra.lib.system`). This keeps argument-passing as pure data flow rather than an
effect, is more faithful to lambda calculus, and removes the ambiguity between "this process's
arguments" and `Command.arguments` (the args passed *to a child*) — there is now only the latter.

**Decisions (resolved):**
- **Naming = `execute`** — POSIX's own verb: `system()` "executes a command". `spawn` is reserved by
  POSIX (`posix_spawn`) for the async create-and-return semantics we are deferring.
- **Non-zero exit = `right`** — only failure to *launch* (ENOENT/EACCES on the program, bad cwd) is
  `left SystemError`; a child that ran and exited non-zero is a normal `ProcessResult` with that
  `exitCode`. Matches `subprocess.run`, `ProcessBuilder`, and `$?` in the build scripts.
- **stdin/stdout/stderr → split into `hydra.lib.io`**, not here. `hydra.lib.system` stays cohesively
  about process/OS control; a process's own standard streams are a separate concern (future branch).
- **No `setWorkingDirectory` / `setEnvironmentVariable`** — process-global mutation is awkward under a
  pure effect model; pass `workingDirectory` / `environment` per `Command` instead. POSIX treats the two
  asymmetrically (`getenv` is clean; `setenv`/`putenv` carry thread-safety and memory-ownership hazards),
  so modeling only the read side keeps us in the portable, well-behaved subset. Consequence for testing:
  the read primitives (`getEnvironment` / `getEnvironmentVariable`) can only be asserted against a
  *guaranteed-absent* variable in our own process, since we cannot set our own environment; reading a
  *known-present* value is instead exercised via the child round-trip (`execute` of `env` with a given
  `Command.environment`). Testability alone is not sufficient reason to add a setter (considered and
  declined).
- **`now` lives here** — one effectful-OS library for now (a build system needs timestamps; POSIX keeps
  both process control and `clock_gettime` in the XSH System Interfaces volume). May split into
  `hydra.lib.time` later if the surface grows.

### Backing types

The `hydra.system` module (parallel to `hydra.file`):

```
EnvironmentVariable = wrap string   -- an environment-variable NAME / identifier (POSIX environ name)
StatusCode          = wrap int32    -- a process exit / wait status (POSIX exit status)

Command = record {
  program          : FilePath,                            -- the executable; reuse hydra.file.FilePath
  arguments        : list<string>,                        -- argv after the program
  workingDirectory : optional<FilePath>,                  -- none = inherit parent's cwd
  environment      : optional<map<EnvironmentVariable, string>> }  -- name -> value; none = inherit; given = replace
                                          -- (no standardInput: stdin handling deferred to hydra.lib.io)

ProcessResult = record {
  exitCode : StatusCode,   -- the child's exit status (0 = success by convention)
  stdout   : binary,       -- captured standard output
  stderr   : binary }      -- captured standard error
```

`StatusCode` wraps `int32` so an exit status is a self-describing nominal type rather than a bare
integer (the same motivation as naming `FilePath` rather than reusing `string`). `EnvironmentVariable`
wraps `string` over a variable's **name** — the variable's identity, the thing you look up by. The name
is also the constrained part: POSIX (XBD section 8) requires environment-variable names to consist of
characters from the portable character set and to contain no `=` (the name/value separator in
`environ`). A variable's *value* is an arbitrary `string` and stays unwrapped. Hence `environment` is a
`map<EnvironmentVariable, string>` (name → value).

The `hydra.error.system` module (parallel to `hydra.error.file`, namespace `hydra.error.system`) holds
the error type. **Conservative-but-non-trivial** set: four named, portable launch-failure cases that
every host can distinguish, plus an `other` catch-all.

```
-- module hydra.error.system
SystemError = union {
  commandNotFound  : FilePath,   -- ENOENT: the program does not exist / not on PATH
  permissionDenied : FilePath,   -- EACCES: the program is not executable / access denied
  invalidWorkingDirectory : FilePath,  -- ENOENT/ENOTDIR on the requested workingDirectory
  interrupted      : unit,       -- the call was interrupted before the child produced a result (portable EINTR/host-kill abstraction)
  other            : string }    -- any other launch failure, as a host-provided message
```

Variants dropped from the wider candidate set to stay conservative: per-signal detail (host-divergent),
resource-limit cases (`EMFILE`/`ENOMEM`, fold into `other`), and a separate `notADirectory`
(folded into `invalidWorkingDirectory`). These only describe *launch* failure — a child that ran and
exited non-zero is a normal `ProcessResult` (`right`), not a `SystemError`.

---

## 3a. POSIX-aligned doc comments (final wording)

Following the precedent of `hydra.file` and `hydra.time`, **every type and every primitive states
explicitly how it relates to POSIX** — naming the relevant POSIX function, header, or volume, linking
the Open Group Issue 8 base specification (`pubs.opengroup.org/onlinepubs/9799919799`), and citing the
section where useful. As with `FileStatus`/`Timespec`, where Hydra models only a portable subset, the
comment says so.

Spec anchors used below (Open Group Base Specifications, Issue 8):
- XSH System Interfaces: `…/functions/<name>.html`
- XBD Base Definitions: `…/basedefs/…` (e.g. `environ` is XBD §8, *Environment Variables*)
- XCU Shell & Utilities: `…/utilities/…` (exit-status conventions, XCU §2.8.2)

### Types — `hydra.system`

**`StatusCode`** (`wrap int32`)
> A process exit status. Corresponds to the value passed to the POSIX `exit()` function and reported by
> `wait()` / `waitpid()` for a normally-terminated child (XSH; see
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html and
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/wait.html). By the convention of XCU
> section 2.8.2, *Exit Status for Commands*
> (https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_19_08_02), 0 denotes
> success and non-zero denotes failure. POSIX passes only the low 8 bits of the status through `wait()`;
> Hydra widens this to a signed `int32` so that host runtimes which expose a fuller code (e.g. Windows
> process exit codes, or the negative "killed by signal N" convention) can be represented without loss.

**`EnvironmentVariable`** (`wrap string`)
> The name of an environment variable — its identity within the POSIX environment list `environ` (XBD
> section 8, *Environment Variables*,
> https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html). POSIX models each entry as
> a `"name=value"` string; this type wraps the *name* portion, which is the constrained, identifying
> part: XBD section 8 requires names to consist of characters from the portable character set and to
> contain no `=` (the name/value separator). A variable's *value* is an arbitrary `string` and is left
> unwrapped. Used as the key type of the environment map and as the argument to `getEnvironmentVariable`.

**`Command`** (record)
> A description of a program to run, supplying the inputs POSIX `posix_spawn` / `execve` take: the
> executable, its argument vector, and optionally a working directory and a replacement environment.
> Hydra models the portable subset only; POSIX file actions, spawn attributes, and signal masks are
> omitted. See
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html and
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html.
> - `program` — the executable to run; a POSIX pathname (resolved against `PATH` by the host when it
>   contains no slash, as for the `execvp`/`execlp` family). Reuses `hydra.file.FilePath`.
> - `arguments` — the arguments following the program name. These become `argv[1..]` for the child;
>   POSIX `argv[0]` (the program name) is supplied by the host from `program` and is not included here.
> - `workingDirectory` — the directory in which to run the child, as if `chdir()` (XSH,
>   https://pubs.opengroup.org/onlinepubs/9799919799/functions/chdir.html) were called before `exec`.
>   `none` inherits the parent's working directory.
> - `environment` — the complete environment for the child (POSIX `environ`). `none` inherits the
>   parent's environment; `given(m)` replaces it entirely, as `execve`'s `envp` argument does (there is
>   no partial-merge form; merge in pure code before calling).

**`ProcessResult`** (record)
> The outcome of a child process that ran to completion, as obtained by `wait()` / `waitpid()` (XSH,
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/wait.html) together with its captured
> output. Only normal termination is modeled directly; abnormal termination (POSIX `WIFSIGNALED`) is
> surfaced through `exitCode` using the host's convention rather than as a separate field.
> - `exitCode` — the child's `StatusCode` (POSIX `WEXITSTATUS`).
> - `stdout` — the bytes the child wrote to file descriptor 1 (standard output; XBD `STDOUT_FILENO`).
> - `stderr` — the bytes the child wrote to file descriptor 2 (standard error; XBD `STDERR_FILENO`).

### Types — `hydra.error.system`

**`SystemError`** (union)
> A recoverable failure to *launch* a process or to perform a `hydra.lib.system` system call, named
> after the POSIX `errno` values the host reports. A child that launches successfully and then exits
> non-zero is **not** a `SystemError` — it is a `ProcessResult` with a non-zero `exitCode`.
> - `commandNotFound` — POSIX `ENOENT`: the executable named by `program` does not exist or is not
>   found on `PATH`. Reported by the `exec` family (XSH,
>   https://pubs.opengroup.org/onlinepubs/9799919799/functions/execve.html).
> - `permissionDenied` — POSIX `EACCES`: the executable exists but is not executable, or a path
>   component denies search permission.
> - `invalidWorkingDirectory` — POSIX `ENOENT` or `ENOTDIR` raised by the implied `chdir()` to
>   `Command.workingDirectory`.
> - `interrupted` — the call was interrupted before the child produced a result (a portable abstraction
>   over POSIX `EINTR` and host-level kills); the child's disposition is unknown.
> - `other` — any other failure, carrying the host-provided error message verbatim (covers `errno`
>   values not modeled above, such as `EMFILE`, `ENOMEM`, or `E2BIG`).

### Primitives — `hydra.lib.system`

**`execute`** : `Command -> effect<either<SystemError, ProcessResult>>`
> `execute(command)` describes an effectful computation which runs the program described by `command`
> to completion and returns its result. This is the POSIX "execute a command" operation at the
> `posix_spawn` / `system()` altitude
> (https://pubs.opengroup.org/onlinepubs/9799919799/functions/posix_spawn.html,
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/system.html): the child is spawned,
> Hydra waits for it (POSIX `waitpid`), and standard output and standard error are captured as raw
> bytes. Output is byte-oriented, with no character decoding or newline translation; decode it to text
> via `hydra.lib.text.decodeUtf8`. A child that runs and exits with a non-zero status (POSIX
> `WEXITSTATUS`) is returned as `right(result)` with that `StatusCode`, following the XCU section 2.8.2
> exit-status convention. Only a failure to *launch* — for example POSIX `ENOENT`, `EACCES`, or a bad
> working directory — is returned as `left(error)`. Unlike the shell `system()`, no intermediate shell
> is invoked; `command.program` is executed directly (as the `execvp` family does), so shell syntax in
> arguments is not interpreted.

**`exit`** : `StatusCode -> effect<unit>`
> `exit(code)` describes an effectful computation which terminates the current process immediately with
> the given status, exactly as the POSIX `exit()` function (XSH,
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/exit.html). The status is reported to the
> parent through `wait()`; by the XCU section 2.8.2 convention, 0 denotes success and non-zero denotes
> failure. This computation does not return, so subsequent effects in a sequence are not performed.

**`getEnvironment`** : `effect<map<EnvironmentVariable, string>>`
> `getEnvironment` describes an effectful computation which returns the entire environment of the
> current process — the POSIX environment list `environ` (XBD section 8, *Environment Variables*,
> https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap08.html) — as a map from variable
> name to value. POSIX represents each entry as a `"name=value"` string; this primitive splits each at
> the first `=` into an `EnvironmentVariable` key and a `string` value. This computation does not fail;
> an empty environment yields an empty map.

**`getEnvironmentVariable`** : `EnvironmentVariable -> effect<optional<string>>`
> `getEnvironmentVariable(name)` describes an effectful computation which returns the value of the
> environment variable `name`, or `none` if it is not present — the POSIX `getenv()` function (XSH,
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/getenv.html). A variable that is present
> but set to the empty string is returned as `given("")`, distinct from the `none` that POSIX `getenv`
> signals with a null pointer. This computation does not fail.

**`getWorkingDirectory`** : `effect<either<SystemError, FilePath>>`
> `getWorkingDirectory` describes an effectful computation which returns the absolute pathname of the
> current working directory — the POSIX `getcwd()` function (XSH,
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/getcwd.html) — as a `FilePath`. A
> recoverable failure (for example POSIX `EACCES`, or the working directory having been removed) is
> returned as `left(error)`; success is returned as `right(path)`.

**`getTime`** : `effect<Timespec>`
> `getTime` describes an effectful computation which returns the current wall-clock time as a `Timespec`
> (seconds and nanoseconds since the Unix Epoch) — the POSIX `clock_gettime()` function with the
> `CLOCK_REALTIME` clock (XSH,
> https://pubs.opengroup.org/onlinepubs/9799919799/functions/clock_gettime.html). The actual resolution
> is implementation- and platform-defined and may be coarser than nanoseconds. As with `CLOCK_REALTIME`,
> the clock is not monotonic: it can jump or move backwards when the host's wall clock is adjusted. This
> computation does not fail.

---

## 4. Why this is portable across 8 hosts

Each host has a direct, well-trodden mapping — no host-specific concepts leak into the signatures:

| Host | `execute` | env | `getTime` |
|---|---|---|---|
| Haskell | `System.Process.readCreateProcessWithExitCode` | `System.Environment` | `Data.Time.Clock.POSIX.getPOSIXTime` |
| Java | `ProcessBuilder` + `Process.waitFor` | `System.getenv` | `Instant.now()` |
| Python | `subprocess.run(capture_output=True)` | `os.environ` | `time.time_ns()` |
| Scala | `scala.sys.process` / `ProcessBuilder` | `sys.env` | `System.currentTimeMillis` / `Instant.now` |
| Lisp ×4 | dialect process API (`sb-ext:run-program`, `clojure.java.shell`, `guile (system)`, Emacs `call-process`) | dialect env API | dialect clock API |

Byte-orientation (`binary`) is what keeps this honest across hosts: no implicit charset or
newline translation, exactly as `hydra.lib.files` does. Callers decode with `hydra.lib.text`.

---

## 5. Resolved decisions

All review questions are settled:

1. **stdin/stdout/stderr** → split into a separate **`hydra.lib.io`** (future branch); not in
   `hydra.lib.system`.
2. **`setWorkingDirectory` / `setEnvironmentVariable`** → **omit**; pass per-`Command`.
3. **Non-zero exit** → `right ProcessResult`; only *launch* failure is `left SystemError`.
4. **`SystemError`** → conservative-but-non-trivial: `commandNotFound`, `permissionDenied`,
   `invalidWorkingDirectory`, `interrupted`, `other` — in its own module `hydra.error.system`.
5. **`getTime`/clock** → **included here.** Per the charter (§1), the system clock is a host-provided
   service, so it is a charter member of the System Interfaces library — not a candidate for splitting
   out. Reuses `hydra.time.Timespec`.
6. **Naming** → `execute` (POSIX `system()` verb); `getTime` (POSIX `clock_gettime`); the remaining
   names align with their POSIX functions (`getenv`, `getcwd`, `exit`).
7. **No `getArguments`** → a Hydra program receives its arguments as an entrypoint parameter (POSIX
   gives `argv` no accessor); the host `main` shim supplies them. Removes the `getArguments` vs.
   `Command.arguments` ambiguity.
8. **`StatusCode := wrap int32`** and **`EnvironmentVariable := wrap string`** (the variable *name* /
   map key) are nominal wrapper types, in `hydra.system`.

---

## 6. Path from here to #416

This library is necessary but not sufficient for promoting the build scripts. After `hydra.lib.system`
lands, #416 still needs: glob/path-manipulation helpers (pure, can be Hydra-side), the digest/caching
logic (pure), argument parsing (pure), and a Hydra "main" entrypoint per host that the build can be
generated into. `hydra.lib.system` provides the effectful floor those build on.
