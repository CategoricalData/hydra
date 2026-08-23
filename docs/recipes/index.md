# Hydra developer recipes

Step-by-step guides for common Hydra development tasks. These recipes provide practical, hands-on instructions
for extending Hydra, implementing new features, and working with Hydra's architecture.

> **Where docs live.** `docs/` (this directory) is for developers working with the code: procedural recipes,
> implementation-level details, build-system mechanics, troubleshooting. The
> [Hydra wiki](https://github.com/CategoricalData/hydra/wiki) is for user-facing documentation that explains
> Hydra's design as it is — conceptual framing, property-graph and RDF design, release policy. Provisional
> material — sketches, in-flight proposals — belongs in issues or branch plans, not here.

## Available Recipes

### Core Development

- **[Adding new type and term constructors to Hydra Core](extending-hydra-core.md)** - Complete process including schema updates, type inference, rewriting functions, and solving the bootstrap problem.
- **[Adding new primitive functions](adding-primitives.md)** - Guide for implementing primitive functions across Haskell, Java, Python, Scala, and Lisp implementations.

### Testing

- **[Extending the hydra-kernel test suite](extending-tests.md)** - How to add new test cases to Hydra's cross-language test suite.

### Implementations

- **[Creating a new Hydra implementation](new-implementation.md)** - Step-by-step guide for implementing Hydra in a new language
- **[Synchronizing Hydra-Python](syncing-python.md)** - How to regenerate Python artifacts from Hydra-Haskell sources
- **[Exporting modules to JSON](json-kernel.md)** - How to export and verify Hydra modules (kernel, main, test) as JSON for cross-language access

### Refactoring

- **[Refactoring the Hydra kernel](refactoring.md)** — how to create, rename, or delete kernel elements or modules, and propagate the change across all implementations. Includes
  [moving or renaming modules (namespace refactoring)](refactoring.md#moving-or-renaming-modules-namespace-refactoring).
- **[Promoting raw code to Hydra modules](promoting-code.md)** — convert raw Haskell code into Hydra source modules that can be generated to multiple target languages.

### Code Generation

- **[Generating code with Hydra](code-generation.md)** - End-to-end guide to generating source code from Hydra modules: DSL vs JSON paths, the writeXxx functions, bootstrap CLI, sync scripts, and troubleshooting
- **[Generating code from your own Hydra modules](downstream-codegen.md)** - For downstream projects: the supported entry points (`ManifestWriter.packageManifestJson` + `Codegen.inferModulesGiven`), why the multi-package orchestration functions don't apply, and adjacent gaps (#640, #644)
- **[Ingesting JSON data into a Hydra schema](ingesting-json.md)** - Loading external JSON into typed host values via the build-the-Term-directly path: generating decoders, Term-construction reference, fail-fast decoder roundtrip, and cycle-breaking patterns
- **[Migration shims: building when a published host can't](migration-shims.md)** - What to do when a published host (Java/Python/Haskell) can't build the current tree: pinning to an earlier good release vs. the local-host shim for backward-incompatible kernel changes
- **[List representation and indexed-access performance](list-performance.md)** - Which hosts back `hydra.lib.lists.map` output with a linked (cons) structure vs. an array-backed one, why repeated indexed access is quadratic on the affected hosts, and the per-host materialize workaround
- For the build/sync/cache *model* (phases, what each cache keys on, the published-host consume model for all three hosts), see **[The Hydra build system](../build-system.md)** at the top level of `docs/`.

### Benchmarking

- **[Running benchmarks](running-benchmarks.md)** - How to run the kernel-tests and cross-host inference benchmarks, and how to read the dashboards

### Maintenance

- **[Repository maintenance](maintenance.md)** - Periodic checks: non-source files, stale generated artifacts, definition ordering

### Development Workflow

- **[LLM-assisted development](llm-assisted-development.md)** - Best practices for using AI assistants when working with Hydra

### Troubleshooting

- **[Troubleshooting guide](../troubleshooting.md)** - Debugging strategies, primitive dispatch tracing, and common errors across all implementations

### History

Retrospectives on settled work, kept for reference — feature histories (why a major feature was built the
way it was) and completed investigations (what was tried, what worked, what didn't). These are *not*
current recipes; the shipped mechanics live in the main docs, and each entry links out to them.

Feature retrospectives:

- **[Self-hosting: transposing the coders to host-native DSLs](../history/self-hosting-coder-transpose.md)** —
  the #182/#344/#346 migration that made Java, Python, and Scala generate from host-native coders; byte-identity
  as the oracle, the cold-start bootstrap paradox, and the bugs the transpose surfaced.
- **[The overlay/ restructuring and the hydra.overlay namespace](../history/overlay-restructuring.md)** —
  #418/#434/#501/#511: how host-native source consolidated under `overlay/`, why the namespace boundary
  exists, and how `bindings/` was folded in with build config modeled as Hydra types.
- **[Consuming the published host, and how "oil and water" was forged](../history/published-host-consume-model.md)** —
  #369/#370/#500/#608: the bootstrap-circularity problem, the consume-published decision, the `--local-host`
  shim, and the two bugs that turned oil-and-water into a CI-enforced invariant.
- **[Design record: `hydra.pg.model` ↔ `hydra.neo4j.model` mapping](../history/design-pg-neo4j-mapping.md)** —
  the design rationale behind the shipped `hydra.neo4j.pg` mapping (#510); the feature itself is documented
  in the [hydra-pg README](../../packages/hydra-pg/README.md).

Completed investigations:

- **[Python host performance investigation](../history/python-host-perf-investigation.md)** — multi-session
  work that brought Hydra-Python from "unusable for term-level workloads" to "competitive with Haskell and
  Java." Lessons likely apply to other hosts.
- **[Emacs Lisp collections and lazy-let performance fix](../history/emacs-lisp-collections-perf.md)** —
  #361, mirroring the Common Lisp (#360) and Python (#344) collection fixes. Captures the three-host
  structural similarity.
- **[Inference scaling — cross-host complexity-class analysis](../history/inference-bench-complexity-analysis.md)** —
  hydra-bench-driven scaling study across implementations; cross-link from
  [Running benchmarks](running-benchmarks.md).
- **[Attempted per-SCC fold rewrite of `inferModules`](../history/inferModules-per-scc-attempt.md)** —
  measured-failed experiment to address the cold-CI heap overflow; documents the ~4× peak-memory and ~2×
  wall-time regression of a naive per-SCC fold. Useful if anyone revisits incremental inference. Preserved
  on local branch `wip_per_scc_inferModules`.

## About Recipes

Recipes are practical, task-oriented guides that walk through specific development scenarios. Each recipe includes:

- Clear prerequisites and context
- Step-by-step instructions
- Code examples and file locations
- Common pitfalls and troubleshooting tips
- Verification steps

These complement Hydra's reference documentation by focusing on "how to accomplish X" rather than "what is X."

## Contributing Recipes

Have a common development task that would make a good recipe? Contributions are welcome! Recipes should:

- Focus on a specific, well-defined task
- Include concrete examples from the Hydra codebase
- Provide complete, tested instructions
- Note any version-specific considerations

See the existing recipes for examples of structure and style.
