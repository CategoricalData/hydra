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

- **[Extending the common test suite](extending-tests.md)** - How to add new test cases to Hydra's cross-language test suite.

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
- **[Ingesting JSON data into a Hydra schema](ingesting-json.md)** - Loading external JSON into typed host values via the build-the-Term-directly path: generating decoders, Term-construction reference, fail-fast decoder roundtrip, and cycle-breaking patterns
- For the build/sync/cache *model* (phases, what each cache keys on, the path to #347), see **[The Hydra build system](../build-system.md)** at the top level of `docs/`.

### Benchmarking

- **[Running benchmarks](running-benchmarks.md)** - How to run the kernel-tests and cross-host inference benchmarks, and how to read the dashboards

### Maintenance

- **[Repository maintenance](maintenance.md)** - Periodic checks: non-source files, stale generated artifacts, definition ordering

### Development Workflow

- **[LLM-assisted development](llm-assisted-development.md)** - Best practices for using AI assistants when working with Hydra

### Troubleshooting

- **[Troubleshooting guide](../troubleshooting.md)** - Debugging strategies, primitive dispatch tracing, and common errors across all implementations

### Historical investigations

Completed work products from past investigations, kept for reference. These are *not* current recipes; they
capture what was tried, what worked, and what didn't.

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
- **[Lazy fix design: Coder.hs edits](../history/lazy-fix-design.md)** — pre-edit design sketch for replacing
  the eager-walrus + `@lru_cache(1)` pattern with `hydra.python.util.Lazy` in the *legacy Haskell* Python
  coder. **Status: likely superseded** by the Python-native coder rewrite (#344). Kept pending review.

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
