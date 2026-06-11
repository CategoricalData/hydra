# Migration shims: building when a published host can't

Since 0.16, the Hydra build consumes **published, versioned hosts** by default rather than building
each host locally (issue [#370](https://github.com/CategoricalData/hydra/issues/370)).
The Java DSL→JSON step runs against `net.fortytwo.hydra:hydra-java` from Maven Central; the Python step
against `hydra-python` from PyPI; the version is pinned in `hydra.json`
(see [The build system](../build-system.md#consuming-published-hosts)).

This works because Hydra's data model is **forward-compatible**: a previously-published host can process
the current build's data as long as `hydra.core` and `hydra.packaging` (especially the `Module` type)
have not changed incompatibly since that host was released
(see [Self-bootstrapping and forward-compatibility](https://github.com/CategoricalData/hydra/wiki/Packaging#self-bootstrapping-and-forward-compatibility)).

This recipe covers what to do when that assumption breaks — when a published host cannot build the
current tree. There are two distinct failure modes with two different fixes.

## Decide which failure mode you're in

| | **Mode A — a release is buggy** | **Mode B — a backward-incompatible change** |
|---|---|---|
| What happened | A published host version has a bug (bad codegen, a crash), but the data model is unchanged. | A kernel change to `hydra.core` / `hydra.packaging` means **no** published host — including the latest — can process the new data. |
| Symptom | The current `hostVersion` fails, but an **earlier** published version works. | **Every** published version fails the same way; the change is in this tree's kernel. |
| Fix | **Pin** to an earlier good version (no local build). | **Local-host shim**: build the host locally, use it this cycle, publish it, then pin to it. |
| Section | [Mode A](#mode-a-pin-to-an-earlier-good-release) | [Mode B](#mode-b-the-local-host-shim) |

If you are unsure, try Mode A first (it is cheaper and non-destructive). If pinning to the last few
published versions all fail identically, you are in Mode B.

## Mode A: pin to an earlier good release

The published-host version each host depends on is resolved from `hydra.json`:
`hostVersionOverrides[<pkg>]` if present, else the global `hostVersion`
(`bin/lib/hydra-packages.py host-version <pkg>`).

To pin a single host back without affecting the others, add a `hostVersionOverrides` entry by hand:

```jsonc
// hydra.json
{
  "currentVersion": "0.16.1",
  "hostVersion": "0.16.0",
  "hostVersionOverrides": {
    "hydra-java": "0.15.0"   // hydra-java 0.16.0 is buggy; use the last good one
  },
  ...
}
```

Then re-run the sync. The Java DSL→JSON step now resolves `hydra-java:0.15.0` from Maven; Python and the
rest are untouched. Because `component_identity` keys the build cache off the host version, the override
also invalidates exactly that host's cached outputs and nothing else (see
[the per-target generator stamp](../build-system.md#the-per-target-generator-stamp)).

Remove the override once a fixed version is published and `hostVersion` is bumped past the bad release.

> Pinning only works if an **earlier published version can process the current data**. If the breakage
> is caused by a kernel change in *this* tree (Mode B), every published version will fail, and pinning
> cannot help.

## Mode B: the local-host shim

When a backward-incompatible kernel change means no published host can handle the new data, you must
build a host **from the current sources**, use it for this build, and then publish it so the default
path can resume.

### The shim ordering: Haskell first, then Java/Python

The hosts are not peers. **Haskell is the bootstrap root** for *generating* every target's `dist/`: the
Java and Python coder runtimes are produced by the Haskell host (`dist/java/`, `dist/python/` come out
of Phase 3/4), so a local rebuild must start with Haskell.

Note that the Haskell host itself now *consumes* the published `hydra-kernel` + `hydra-haskell` Hackage
packages by default for its own compile (#370) — it is no longer "always from source." But it has the
same `--local-host` escape (build the whole host from local source), and that is what you use here. So
the local-host shims compose in order:

```
Haskell (--local-host: kernel from source)  →  dist/{java,python}/  →  Java/Python --local-host  →  DSL→JSON
```

Under a breaking change you therefore:

1. Rebuild the Haskell host from source: `heads/haskell/bin/sync-haskell.sh --local-host`. This compiles
   the kernel locally (instead of the published Hackage dependency) and regenerates the kernel JSON +
   `dist/{java,python}` against the new data model. (For a single edited Haskell coder rather than the
   whole host, set `hostVersionOverrides["hydra-<lang>"] = "local"` in `hydra.json` and keep the rest
   from Hackage.)
2. Run the Java / Python DSL→JSON step in **local-host** mode, which builds the coder from that
   freshly-generated local `dist/` instead of the published artifact.

### Java local-host shim

`bin/update-java-json.sh` (and `bin/generate-hydra-java-from-java.sh`) take `--local-host`:

```bash
bin/generate-hydra-java-from-java.sh --local-host
```

This compiles the coder from the local rollup (`:hydra-java:compileHeadsExtrasJava`, i.e. the local
`dist/java` tree) instead of resolving `hydra-java` from Maven, then runs `hydra.UpdateJavaJson`.

### Python local-host shim

`bin/generate-hydra-python-from-python.sh` takes `--local-host`:

```bash
bin/generate-hydra-python-from-python.sh --local-host
```

This puts `dist/python/hydra-{kernel,python}` on `PYTHONPATH` (built by `bin/sync-python.sh`) instead
of the published-wheel venv.

### Haskell local-host shim

`heads/haskell/bin/sync-haskell.sh` takes `--local-host`:

```bash
heads/haskell/bin/sync-haskell.sh --local-host
```

By default (`--published-host`) the host links `hydra-kernel` + `hydra-haskell` from Hackage (pinned at
`hydra.json` `hostVersion`) and compiles only the other coders + drivers + DSL sources. `--local-host`
puts the kernel + Haskell-coder runtime back on the local compile path (today's from-source behavior),
for a backward-incompatible kernel change the published kernel can't express, or for cutting a kernel
release. The Hackage-consumable set is *derived by probing* whether `hydra-<pkg>-<hostVersion>` actually
exists on Hackage — so a coder published in a future cycle is consumed automatically with no config
change. To force just one package local while consuming the rest, set
`hostVersionOverrides["hydra-<pkg>"] = "local"` in `hydra.json` (no flag needed); a local coder still
compiles against the published kernel, valid under the forward-compatibility contract.

### Publish the interim host, then pin to it

The local-host shim gets *this* build unblocked, but every subsequent build (and CI, and other
contributors) still resolves the broken published version. To make the new host the consumed default,
publish it locally and pin to it:

- **Java** — publish to the local Maven repo (`~/.m2`), which the json-driver project already lists via
  `mavenLocal()`:

  ```bash
  # from the per-package dist build, publish the interim hydra-java + hydra-kernel
  (cd dist/java/hydra-kernel && ./gradlew publishToMavenLocal)
  (cd dist/java/hydra-java   && ./gradlew publishToMavenLocal)
  ```

- **Python** — build a wheel from the local `dist/python/hydra-python` and install it into the
  published-host venv (`bin/lib/python-published-host.sh` installs from the default index, which
  includes a local interim build):

  ```bash
  # build + install an interim wheel into heads/python/.venv-published-host
  (cd dist/python/hydra-python && python -m build)
  # then point the venv at it, or bump the pinned version below
  ```

Then bump the pin so the default consume path picks up the interim version:

```bash
bin/bump-host-version.sh 0.16.1          # global, if the whole release moves
# or hand-edit hydra.json:hostVersionOverrides for a single host
```

Once the real fixed version is published to Maven Central / PyPI, bump `hostVersion` to it and remove
any `hostVersionOverrides` / local interim artifacts.

## The bootstrap (cold-`dist/json`) edge

The Java/Python DSL→JSON drivers read the kernel universe from `dist/json/hydra-kernel/`, and the
Haskell generator needs the native packages' JSON present to resolve cross-package references
(e.g. `hydra-scala` → `hydra.java.serde`). `dist/json/**` is committed, so a normal checkout always has
these. If you have deliberately emptied `dist/json/` (a true cold bootstrap), seed the kernel and native
package JSON via a full local Haskell build **before** the Java/Python steps run — the same
Haskell-first ordering as a Mode B shim.

## See also

- [The build system](../build-system.md) — the cache model, `component_identity`, and the consume path.
- [Self-bootstrapping and forward-compatibility](https://github.com/CategoricalData/hydra/wiki/Packaging#self-bootstrapping-and-forward-compatibility)
  (wiki) — why a previous release's host can be trusted, and when it can't.
- [Release policy](https://github.com/CategoricalData/hydra/wiki/Release-policy) (wiki) — the
  compatibility commitments that keep Mode B rare.
