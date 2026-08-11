# Generating code from your own Hydra modules (downstream projects)

A guide for projects that depend on Hydra (via the published `hydra-kernel` / `hydra-java` /
`hydra-python` / `hydra-build` artifacts) and want to generate source code from their own Hydra
module definitions — as opposed to contributing to Hydra itself.

[Generating code with Hydra](code-generation.md) covers the general pipeline (DSL vs. JSON paths,
the `writeXxx` functions, sync scripts). This recipe covers the parts of that pipeline that are
relevant to an external caller, and the parts that are not.

## The wrong path (and why it looks right)

The easiest entry point to find is `Codegen.generateSourceFiles(...)`. It is pure, well
documented, and produces `(filePath, content)` pairs for a target language. It does **not**
write a `manifest.json`, fold cross-module inference context beyond its own arguments, or derive
the emission-time `overlaySubs` set (below). A driver built only on `generateSourceFiles` ends up
hand-rebuilding that machinery.

The next entry points a reader finds — `Generation.inferAndWriteByPackage` and
`Generation.writePackageManifests` (both host-native; Java: `hydra.overlay.java.build.Generation`,
same design in the Haskell `Hydra.Generation`) — look like the supported multi-step API because
they have worked examples in [`code-generation.md`](code-generation.md) and
[`build-system.md`](../build-system.md). They are not intended for a downstream project: both call
`Generation.groupByPackage`, which builds a routing map by **scanning the filesystem for
subdirectories that already contain `src/main/json/manifest.json`**. On a fresh single-package
output root, that scan is empty, so routing the very first module fails:

```
unrouted module: com.example.model is not declared in any package's manifest (RoutingMap)
```

This is Hydra's own multi-package self-hosted build orchestration (hydra-kernel → hydra-java →
hydra-python → hydra-jvm, topologically sorted via each package's `package.json`). A new
single-package downstream project cannot bootstrap into it — there is no pre-existing manifest to
route against.

## The supported path

Two pure, public functions — already on the classpath via `hydra-build` — cover what a downstream
project actually needs, with no routing and no dependency on a pre-existing manifest tree:

- **`hydra.Codegen.inferModulesGiven(ctx, bsGraph, universeMods, targetMods)`** — the same
  function `inferAndWriteByPackage` calls once per package internally. Pass every module in your
  project as both `universeMods` and `targetMods` to type-check them all together; cross-module
  references resolve automatically since every module is in scope at once.
- **`hydra.build.ManifestWriter.packageManifestJson(pkg, mainMods, dslMods, encMods, testMods)`** —
  pure manifest field-assembly and serialization, no filesystem access. Produces the same
  `manifest.json` contents `writePackageManifests` would, for one package, without routing.

Minimal Java example, one project, one package, no pre-existing output tree:

```java
InferenceContext ctx = new InferenceContext(0, new ArrayList<>());
Graph bsGraph = Generation.bootstrapGraph();
List<Module> myModules = /* your project's modules */;

Either<Error_, List<Module>> result =
    Codegen.inferModulesGiven(ctx, bsGraph, myModules, myModules);
List<Module> inferred = ((Either.Right<Error_, List<Module>>) result).value;

// Generate source files per module, same as generateSourceFiles alone would require:
for (Module m : inferred) {
    List<Pair<String, String>> files = Codegen.generateSourceFiles(
        coderFn, lang, /* doInfer */ false, bsGraph, inferred, List.of(m), ctx);
    // write files...
}

// Then write a manifest for the whole project:
Value manifest = ManifestWriter.packageManifestJson(
    "my-project", inferred, List.of(), List.of(), List.of());
Files.write(manifestPath, Json.printJson(manifest).getBytes(UTF_8));
```

`doInfer` is `false` on the `generateSourceFiles` call because `inferModulesGiven` already
attached inferred types to `inferred`'s bindings — re-inferring would repeat the same work
`inferModulesGiven` just did.

This is not a special case or a workaround: `inferAndWriteByPackage` is exactly this call, looped
once per package in dependency order, with routing and file I/O wrapped around it. A single-package
downstream project just skips the loop and the routing.

## Adjacent gaps in the same flow

Two related rough edges surface once a downstream driver reaches this level. Neither blocks the
path above; both cost a rediscovery if undocumented.

### Kernel leaf types referenced by primitives (#640)

A term using a primitive like `hydra.lib.files.readFile` references kernel types
(`hydra.file.FilePath`, `hydra.error.file.FileError`) that are not necessarily among your project's
own modules or their declared dependencies — primitives resolve globally, so nothing forces you to
declare the dependency. Passing those kernel type modules via `universeMods`/`targetMods` above is
necessary but not sufficient: `inferModulesGiven` (and `generateSourceFiles` used standalone) need
the types folded into the **graph**, not just listed as a universe module, or term-level resolution
fails with `NoSuchBinding`.

The Haskell host has a ready-made list for this, `Hydra.Generation.kernelTypeUniverse` (`hydra.time`,
`hydra.file`, `hydra.error.file`, `hydra.system`, `hydra.error.system`) — fold it into the graph
with `Codegen.modulesToGraph(bootstrapGraph, universe, universe)` before generating. There is no
Java/Python equivalent yet, so a Java or Python downstream project referencing these primitives
must currently obtain the same modules (e.g. from the published `hydra-kernel` JSON resources, if
present on the classpath, or decode them from JSON some other way) and fold them the same way.

### Deriving `overlaySubs` for the 5-arg coder (#644)

`hydra.java.Coder.moduleToJava` / `hydra.python.Coder.moduleToPython` take an `overlaySubs` set
that drives emission-time redirection of `hydra.lib.<sub>` primitive calls to their per-target
overlay implementations. There is no published helper to derive this set; it can be reconstructed
from the bootstrap graph's primitive names:

```java
Set<String> overlaySubs = new HashSet<>();
for (Name n : Generation.bootstrapGraph().primitives.keySet()) {
    String v = n.value;
    if (v.startsWith("hydra.lib.")) {
        String rest = v.substring("hydra.lib.".length());
        int dot = rest.indexOf('.');
        if (dot > 0) overlaySubs.add(rest.substring(0, dot));
    }
}
```

This yields the full set of `hydra.lib.*` subnamespaces (`effects`, `files`, `text`, ...) that have
per-target overlay implementations.

### Indexed access over `hydra.lib.lists.map` output (#651)

On Java and the four Lisp-dialect hosts, `hydra.lib.lists.map` (and similar list primitives) return
a linked (cons) structure, so repeated indexed access in a loop is quadratic rather than linear —
easy to hit in a downstream project's own traversal code, since nothing at the type level signals
the cost. Python, Scala, and TypeScript are unaffected (array/tuple-backed). See
[List representation and indexed-access performance](list-performance.md) for the full per-host
breakdown and the materialize-before-indexing workaround.

## Related documentation

- [Generating code with Hydra](code-generation.md) — the general pipeline, DSL vs. JSON paths,
  `writeXxx` signatures, and Hydra's own sync scripts (multi-package orchestration, not this path).
- [The Hydra build system](../build-system.md) — phases, caching, published-host consumption.
