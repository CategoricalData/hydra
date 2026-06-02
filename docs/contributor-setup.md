# Setting up a Hydra contributor environment

This guide is for contributors building Hydra from source — extending the kernel, regenerating code,
or running the cross-host test matrix.
If you only want to **use** Hydra as a library from your own project, see
[Getting started](getting-started.md) instead.

Hydra spans eight language implementations.
You do not need all of them to be productive — most contributor workflows only need a subset.
This guide groups prerequisites into scopes so you can install the minimum you need and add the rest later.

## Scope tiers

Pick the smallest tier that covers the work you plan to do.
Tiers 1–3 are strict supersets; tiers 4 and 5 are optional add-ons you only need for Go work
or GitHub authoring.

| Tier | Covers | What you can run |
|------|--------|------------------|
| **Kernel** | Haskell host only | `stack test`, `heads/haskell/bin/sync-haskell.sh` (Phase 1) |
| **Triad** | Kernel + Java + Python | `/sync-default`, `/bootstrap` default, `/test` default |
| **Full matrix** | Triad + Scala + four Lisp dialects | `/sync` (all × all), `/bootstrap all`, full 8 × 8 |
| **Go bud** | Optional Go target | `/sync-go` |
| **Authoring** | Optional ops tools | Issue/PR interaction, releases |

`/sync` and the slash commands referenced above are documented in [.claude/commands/](../.claude/commands/)
and behave the same as their `bin/` counterparts.

### Linux prerequisites (Debian/Ubuntu)

Refresh apt indices before your first install: `sudo apt update`.
The install commands below assume `curl` and `gnupg` are present —
both are part of the Debian/Ubuntu base install but may be missing in minimal containers:

```bash
sudo apt install -y curl gnupg
```

## Tier 1 — Kernel (Haskell)

The minimum useful environment.
With this you can edit kernel DSL sources, regenerate the Haskell kernel, and run `stack test`.

### Required

- **`stack`** (the Haskell Tool Stack), any recent version.
  Stack manages GHC and Cabal versions for you, so you do not need to install them separately.
  - macOS: `brew install haskell-stack`
  - Linux: `curl -sSL https://get.haskellstack.org/ | sudo sh` —
    the installer apt-installs C-toolchain dependencies (`libgmp-dev`, `g++`, `zlib1g-dev`, ...)
    and writes `stack` to `/usr/local/bin/`, so `sudo` is required on Debian/Ubuntu.
  - Other platforms: see <https://docs.haskellstack.org/en/stable/install_and_upgrade/>.
  - Verify: `stack --version`
  - **One-time GHC bootstrap:** from `heads/haskell/`, run
    `stack setup --install-ghc` to download the GHC version pinned in `stack.yaml`
    (currently 9.10.2, ~700 MB).
    The `--install-ghc` flag is required because Hydra's `stack.yaml` sets
    `install-ghc: false` to prevent surprise downloads.
  - If you see warnings about untested GHC/Cabal versions, run `stack upgrade`.
    See the Stack section of [packages/hydra-haskell/README.md](../packages/hydra-haskell/README.md#troubleshooting).
- **`python3`** ≥ 3.9.
  Several pipeline helpers under `bin/lib/` are written in Python, so it is required even when
  you are not generating Python code.
  - macOS / Linux: usually preinstalled; otherwise install from your package manager.
  - Verify: `python3 --version`

### Test it

```bash
cd heads/haskell
stack setup --install-ghc      # one-time; downloads GHC 9.10.2 if absent (~700 MB)
stack build hydra:lib          # first run also compiles Hackage dependencies (~5 GB on disk total, ~20 min)
stack test                     # full kernel test suite, once the library compiles
```

The first build is slow because Stack downloads and compiles ~80 transitive dependencies
into `~/.stack/` (~3 GB) and Hydra's own ~800 modules into `heads/haskell/.stack-work/` (~2 GB).
Subsequent builds reuse both caches and are fast.
If `stack test` passes, you are ready for kernel work.

## Tier 2 — Triad (Haskell + Java + Python)

The default scope for `/sync-default`, `/bootstrap`, and `/test`.
This is what runs before merging changes upstream.

### Adds on top of Tier 1

- **JDK** ≥ 11.
  Hydra-Java is built with `sourceCompatibility = JavaVersion.VERSION_11`, so JDK 11 or newer works.
  - macOS: `brew install --cask temurin@17` (or any 11+ vendor)
  - Linux (Debian/Ubuntu): `sudo apt install openjdk-17-jdk-headless` —
    use the `-headless` variant on servers to skip GUI dependencies; switch to
    `openjdk-17-jdk` if you want `jconsole`, `jvisualvm`, etc.
  - Linux (Fedora): `sudo dnf install java-17-openjdk-devel`
  - Verify: `java -version`
- **`gradle`** is **not** required separately — the repo uses the Gradle Wrapper (`./gradlew`).
  Bringing your own Gradle is fine if you have it.

Python 3 from Tier 1 covers both pipeline helpers and the Python target itself; no additional install needed.

### Test it

```bash
./bin/sync-default.sh
```

This regenerates the Haskell, Java, and Python distributions and runs the Haskell test suite.
Target-language tests run via `./bin/test.sh` (default scope is the triad).

## Tier 3 — Full matrix (8 × 8)

Required for `/sync` with `--hosts all --targets all`, `/bootstrap all`, and issue #409
("Everything-to-everything bootstrapping run").

### Adds on top of Tier 2

- **`sbt`** for Scala.
  - macOS: `brew install sbt`
  - Linux (Debian/Ubuntu) — add the sbt apt repo first; `sbt` is not in the default repos:
    ```bash
    sudo mkdir -p /etc/apt/keyrings
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" \
      | sudo gpg --dearmor -o /etc/apt/keyrings/sbt.gpg
    echo "deb [signed-by=/etc/apt/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" \
      | sudo tee /etc/apt/sources.list.d/sbt.list
    sudo apt update && sudo apt install -y sbt
    ```
  - Other platforms: see <https://www.scala-sbt.org/download.html>
  - Verify: `sbt --numeric-version` (the first run downloads Scala for the launcher and is slow).
- **`clojure`** (Clojure CLI, tools.deps).
  Pulls Clojure runtime via the JDK from Tier 2.
  - macOS: `brew install clojure/tools/clojure`
  - Linux: install `rlwrap` first, then run the official installer.
    `clojure` is not in Debian's default apt repos.
    ```bash
    sudo apt install -y rlwrap
    curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
    chmod +x linux-install.sh
    sudo ./linux-install.sh
    rm linux-install.sh
    ```
  - Verify: `clojure --version`
- **`sbcl`** for Common Lisp.
  - macOS: `brew install sbcl`
  - Linux: `sudo apt install -y sbcl` / `sudo dnf install -y sbcl`
  - Verify: `sbcl --version`
- **`emacs`** for Emacs Lisp (used in `--batch` mode).
  Emacs Lisp is the least mature host — expect rough edges.
  - macOS: `brew install emacs`
  - Linux: `sudo apt install -y emacs-nox` (headless) or `emacs` (with GUI) / `sudo dnf install -y emacs`
  - Verify: `emacs --version`
- **`guile`** (preferred) **or `chibi-scheme`** for Scheme.
  The shared Lisp test runner picks whichever it finds.
  Chibi-scheme is not packaged in Debian 12; use Guile there.
  - macOS: `brew install guile`
  - Linux: `sudo apt install -y guile-3.0` / `sudo dnf install -y guile30`
  - Verify: `guile --version`

### Test it

```bash
./bin/sync.sh --hosts all --targets all
./bin/test.sh haskell,java,python,scala,clojure,common-lisp,emacs-lisp,scheme
```

The full matrix takes much longer than the triad — the issue #409 discussion estimates ~64× longer
than the routine 3 × 3.

## Tier 4 — Go bud (optional)

Go is a "head bud": the kernel can be generated to Go via `/sync-go`, but the Go coder still has
emission bugs and the head has no test suite.
Only install if you are working on the Go host.

- **`go`** ≥ 1.22 (the version pinned in [`heads/go/go.mod`](../heads/go/go.mod)).
  Note: Debian 12's `golang-go` package is 1.19 and **too old** —
  use the upstream tarball, `snap`, or backports.
  - macOS: `brew install go`
  - Linux: download a current tarball from <https://go.dev/dl/> and extract to `/usr/local/go`,
    or `sudo snap install go --classic` if snap is available.
  - Verify: `go version`

## Tier 5 — Authoring (optional)

For interacting with GitHub from the command line and during releases.

- **`gh`** (GitHub CLI).
  Without it, scripts and AI assistants fall back to web fetches for issues and PRs, which works
  but is less convenient.
  - macOS: `brew install gh`
  - Linux (Debian/Ubuntu) — add the GitHub CLI apt repo first; `gh` is not in the default repos:
    ```bash
    sudo mkdir -p /etc/apt/keyrings
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
      | sudo tee /etc/apt/keyrings/githubcli-archive-keyring.gpg > /dev/null
    sudo chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
      | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
    sudo apt update && sudo apt install -y gh
    ```
  - Other platforms: see <https://github.com/cli/cli#installation>
  - Verify: `gh --version`, then `gh auth login` to authenticate.

## Verifying your environment

Run `bin/check-env.sh --<tier>` (where `<tier>` is `kernel`, `triad`, `full`, `go`, `authoring`,
or `all`) to probe which prerequisites are installed for that scope.
The script prints a green/red table and exits non-zero if anything in scope is missing.

```bash
bin/check-env.sh --triad
```

The script only checks that each binary is on `PATH` — it does not enforce minimum versions.
If a tool is installed but too old (e.g., Debian 12's Go 1.19 versus Hydra's required 1.22),
the script still reports it as present; consult the per-tool notes above for version floors.

If `bin/check-env.sh` does not exist yet in your checkout, install tools manually from the lists above.

## Next steps

Once your environment is set up:

- [DSL guide](dsl-guide.md) — how to write Hydra modules in the Haskell DSL.
- [Java DSL guide](dsl-guide-java.md) / [Python DSL guide](dsl-guide-python.md) — host-native authoring.
- [Adding a primitive](recipes/adding-primitives.md) — the smallest end-to-end change.
- [Code generation recipe](recipes/code-generation.md) — full regenerate workflow.
- [Build system](build-system.md) — pipeline phases, cache model, what invalidates what.
- [Recipes index](recipes/index.md) — the full set of task-oriented guides.
- Per-host build/test detail: `packages/hydra-<lang>/README.md`.
