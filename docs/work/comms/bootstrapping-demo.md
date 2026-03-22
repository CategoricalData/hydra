# Bootstrapping Demo — Presentation Plan

**Title**:
```
Graph programming
        in any language
                    (well… three languages so far)
            with Hydra
```

Screen-capture demo with audio commentary, preceded by a brief slideshow. Target audience: engineers. Format: similar to GenPG and Hydra-Python LinkedIn demos.

## Demo walkthrough

- **Slide 1: Title**
- **Slide 2: "How many languages does your application speak?"**
  - JavaScript for the UI, Go for the middle tier, Scala for streaming, C++ at the database level, Python for analysis, etc.
- **Slide 3: "Different languages, same code"**
  - Some logic needs to span language boundaries — this happens a lot in graph applications
  - Validation checks (front-end during query construction, back-end before execution — need to be semantically equivalent)
  - Business logic (real-time execution vs. analysis)
  - Serialization/deserialization
  - Graph transformations
  - Additional examples from prior docs:
    - Regulatory compliance logic (e.g. IFR fuel requirements) — generalizes to finance, healthcare, tax
    - Framework implementations (e.g. TinkerPop across JVM, Python, .NET)
    - War stories: Uber/Dragon diverged into separate Haskell and Java tools; Microsoft had inconsistent UI vs. server validation
- **Slide 4: "Data exchange → logic exchange"**
  - Protobuf, Avro, GraphQL, Thrift — great for shared structure
  - But what if you need to share a program?
  - Hydra: embedded DSL with multiple hosts, built on LambdaGraph (programs = graphs, evaluation = graph transformation)
  - Original motivations in Uber and Apache TinkerPop
  - Speaker notes — prior demos / story so far:
    - LambdaGraph — the formal foundation (KGC 2024). Why care: a principled basis for working with graph data and programs uniformly.
    - The GenPG demo — showed Hydra solving a real data engineering problem end-to-end.
    - Hydra-Python — proved the approach works beyond Haskell, in a mainstream language with a very different runtime.
    - Hydra-Java — now also complete (issue #166, Feb 2026). Three widely separated languages pass the same 2700+ tests.
- **Slide 5: "Mutual self-hosting"**
  - Hydra is written in Hydra (self-hosting)
  - Transpiles to multiple mainstream languages (multi-target) — so far Haskell, Java, Python, Clojure, Scala; in progress: Rust, Go
  - Each implementation passes the same test suite (semantics-preserving)
  - Each implementation generates every other (mutual self-hosting) — Java→Python, Python→Haskell, Haskell→Java, Python→Python, etc.
  - Speaker notes — the conceptual ladder (for deeper explanation if needed):
    - **Self-hosting**: A language whose compiler can generate its own executable kernel. Examples: GCC (C), Rustc (Rust), Go.
    - **Transpilation**: One host language compiles programs into a second language. Examples: Fable (F# → JS), PureScript (→ JS).
    - **Multi-target transpilation**: Transpile into multiple other languages. Examples: Kotlin Multiplatform (→ JVM, JS, native). Particularly widely separated languages — the best example is Haxe (C++, C#, Java, JS, Python, etc.).
    - **Polyglot self-hosting**: Map own kernel into multiple targets. Nim comes closest: compiles into C, C++, Objective-C, and (partially) JavaScript. But those targets are closely related, and no evidence the generated code can bootstrap back.
    - **Mutual self-hosting**: Each generated implementation is a fully capable compiler that can regenerate the kernel in any target, including back into itself. Transitive: A→B, B→C, C→A — all producing equivalent, test-verified output.
- **Slide 6: "Translingual programming"**
  - Write programs in the language you like
  - Run programs in the language(s) you need
  - Optimize for: equivalence guarantees, predictable performance
  - Speaker notes — why it matters for you:
    - **Language freedom**: No language is privileged. Java isn't a second-class citizen generated from Haskell — it's a peer that can generate everything Haskell can. Your team picks the language; Hydra meets you there.
    - **Trust**: Every code generator asks you to trust it. Hydra is the only one that trusts itself. The generated code isn't a second-class artifact — it's the compiler. 2700+ tests verify equivalence across all 9 paths.
    - **Scale**: 129 kernel modules, 74 extension modules, 46 test modules — the entire Hydra codebase, including the Java and Python coders themselves, is bootstrapped and tested across all paths.
- **Slide 7: "Hydra bootstrapping demo"**
  - Hydra's last major milestone before 1.0
  - Generate three implementations from any host language
  - Each implementation generates all three (9 paths)
  - Run common test suite in each; "9 green" = fully translingual
  - Benchmarking: compare run times, correctness → performance
  - How it works: DSL sources → JSON (language-independent) → load in any host → generate any target → build & test
- **Slide 8+: Demo screenshots** (placeholders for bootstrapping matrix, benchmarking results)
- **Orientation** — brief tour of the repo, DSL sources, and interchange format
  - Show repo structure: hydra-haskell, hydra-java, hydra-python, hydra-ext
  - Show Python DSL sources — this is what writing Hydra looks like in a mainstream language
    - `hydra/dsl/meta/examples/core_types.py` — type definitions (the "schema" angle)
    - `hydra/dsl/meta/examples/rewriting.py` — kernel functions with lambdas and pattern matching (the "portable programs" angle)
  - Show a few JSON module files in `src/gen-main/json/` — the language-independent representation these DSL sources compile to, which makes mutual self-hosting possible
- **Bootstrapping demo**
  - Kick off `bootstrap-all.sh` — show it starting, explain what it's doing, then cut to pre-computed results
  - Walk through the 9 output directories in `/tmp/hydra-bootstrapping-demo/`
  - Show the summary matrix — timing, file counts, test results for all 9 paths
  - Drill into java-to-python — show generated Python code to demonstrate it's idiomatic, not ugly transpiler output
  - Diff two hosts targeting the same language (e.g. `diff haskell-to-python/ java-to-python/`) — the mic-drop moment showing independent implementations produce equivalent output
- **Benchmarking**
  - Kick off the benchmarking script — show it starting, then cut to pre-computed results
  - Show performance comparison across hosts and targets
- **Wrap up**
  - Recap: language freedom, trust, scale
  - Call to action (repo link, Discord, try it yourself)

## Open questions

- Call to action: try it yourself? Link to repo? Discord?
