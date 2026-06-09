# Hydra changelog

All notable changes to the Hydra project are documented in this file.

This changelog tracks changes across all Hydra implementations
(Haskell, Java, Python, Scala, TypeScript, Clojure, Common Lisp, Emacs Lisp, Scheme)
and supporting infrastructure.

The format is inspired by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and [Apache TinkerPop](https://github.com/apache/tinkerpop/blob/master/CHANGELOG.asciidoc).

Pre-1.0 versions are listed below.
Untagged versions correspond to in-repository version bumps that were not pushed as Git tags or Hackage releases;
they are documented here for completeness.

---

## [0.16.0] - TBD

Release candidate. Set the release date when 0.16.0 is tagged.
Major themes: kernel-oriented validation rules, JSON Schema coder promotion,
homogenization of writer conventions, a unified `Module.dependencies` field,
primitive-metadata reification, finalized `hydra.packaging`, all-hosts
self-hosting and per-package distributions, and a deep kernel-vocabulary cleanup
(`maybe`→`optional`, order-preserving JSON).

### Highlights

- **Kernel-oriented style checks** in `hydra.validate.*` ([#320](https://github.com/CategoricalData/hydra/issues/320), [#321](https://github.com/CategoricalData/hydra/issues/321), [#351](https://github.com/CategoricalData/hydra/issues/351), [#352](https://github.com/CategoricalData/hydra/issues/352)).
- **JSON Schema coder promoted** to a fully functional DSL coder ([#350](https://github.com/CategoricalData/hydra/issues/350)).
- **`PrimitiveDefinition` and per-namespace registry modules** ([#156](https://github.com/CategoricalData/hydra/issues/156)), with
  host-independent specifications for every standard-library primitive ([#319](https://github.com/CategoricalData/hydra/issues/319)).
- **Self-hosting coder sources** transposed into per-language DSLs ([#344](https://github.com/CategoricalData/hydra/issues/344)), and **all eight hosts
  now pass the common test suite** — TypeScript completed as a head ([#126](https://github.com/CategoricalData/hydra/issues/126)), and the
  Scala and four Lisp hosts brought to functional/self-hosting parity
  ([#422](https://github.com/CategoricalData/hydra/issues/422), [#423](https://github.com/CategoricalData/hydra/issues/423), [#425](https://github.com/CategoricalData/hydra/issues/425), [#426](https://github.com/CategoricalData/hydra/issues/426), [#427](https://github.com/CategoricalData/hydra/issues/427)).
- **Finalized `hydra.packaging`** ([#369](https://github.com/CategoricalData/hydra/issues/369)) and a unified `Module.dependencies` field ([#354](https://github.com/CategoricalData/hydra/issues/354)).
- **`maybe` → `optional` vocabulary** unified across the kernel and all hosts ([#401](https://github.com/CategoricalData/hydra/issues/401));
  **order-preserving JSON objects** ([#398](https://github.com/CategoricalData/hydra/issues/398), [#230](https://github.com/CategoricalData/hydra/issues/230)).
- **Per-package distributions** for Hackage, mirroring Java/Python ([#418](https://github.com/CategoricalData/hydra/issues/418)); **persistent
  immutable collections** adopted across hosts ([#359](https://github.com/CategoricalData/hydra/issues/359), [#360](https://github.com/CategoricalData/hydra/issues/360), [#361](https://github.com/CategoricalData/hydra/issues/361), [#362](https://github.com/CategoricalData/hydra/issues/362)).
- **Homogenized writer conventions** across all target languages ([#339](https://github.com/CategoricalData/hydra/issues/339)).

### New features

- Kernel-oriented style checks in `hydra.validate.*` ([#321](https://github.com/CategoricalData/hydra/issues/321)):
  alphabetical ordering of kernel definitions ([#351](https://github.com/CategoricalData/hydra/issues/351)),
  `doc` annotations on every kernel definition ([#352](https://github.com/CategoricalData/hydra/issues/352)).
- JSON Schema coder promotion ([#350](https://github.com/CategoricalData/hydra/issues/350)): full DSL-based coder; `forall` and type
  application support; schema/language namespaces lifted.
- `PrimitiveDefinition` reified in `hydra.packaging` and per-namespace
  registry modules under `Hydra/Sources/Kernel/Lib/<Sub>.hs` ([#156](https://github.com/CategoricalData/hydra/issues/156)):
  the 13 `hydra.lib.<sub>` modules are the canonical primitive registry,
  declaring name, description, signature, isPure / isTotal flags, and an
  optional `defaultImplementation` per primitive. Adds `TermSignature`
  and `TypeClassConstraint`; `Primitive` restructured to
  `{ definition, implementation }`; `TermDefinition.typeScheme` replaced
  by `TermDefinition.signature`. Resolves [#156](https://github.com/CategoricalData/hydra/issues/156).
- Schema flip: `AnnotatedTerm.annotation` and `AnnotatedType.annotation`
  are now typed as `Term` and `Type` (respectively), rather than
  `Map<Name, Term>` ([#386](https://github.com/CategoricalData/hydra/issues/386)). The canonical encoding for an annotation map
  is `inject(Term){map: TermMap [(TermVariable key, value), …]}`. Two new
  kernel helpers bridge the host-friendly Map view and the schema:
  `wrapAnnotationMap :: Map<Name, Term> → Term` and
  `getAnnotationMap :: Term → Map<Name, Term>`. The latter accepts both
  `TermVariable` keys (the canonical shape) and transitional
  `TermWrap`-of-Name keys so older fixtures load unchanged. Existing
  host-level DSL helpers (`annots`, `annotated`) hide the schema change
  from most call sites.
- Host-independent specifications for every standard-library primitive ([#319](https://github.com/CategoricalData/hydra/issues/319)):
  populated the `PrimitiveDefinition.comments` field for all 240 primitives
  across 13 `hydra.lib.*` namespaces, citing authoritative external sources
  (IEEE 754-2019 for floating-point operations; Unicode general categories
  and simple case mapping for character predicates; Haskell `Data.List` /
  `Data.Map.Strict` / `Data.Set` / `Data.Char` / `Data.Either` / `Data.Maybe`
  for collection and either/maybe semantics) and explicitly flagging
  host-defined behavior (regex syntax, special-value literal capitalization).
  Threaded the `comments` argument through `toPrimitive` and
  `toPrimitiveNoDefault` helpers and through each `Lib/<Sub>.hs` `primNoDef`.
  Resolves [#319](https://github.com/CategoricalData/hydra/issues/319).
- Reshaped the validation API ([#320](https://github.com/CategoricalData/hydra/issues/320)): configurable rules, an errors/warnings
  split, and bounded result accumulation, underpinning the kernel-oriented
  style checks.
- Transposed the per-language coder sources into appropriate host-native DSLs
  for self-hosting ([#344](https://github.com/CategoricalData/hydra/issues/344)); the Java and Python coders are now authored
  host-native, with DSL synthesis extended to coder-package type modules
  ([#358](https://github.com/CategoricalData/hydra/issues/358)).
- Language-specific EDSL features ([#233](https://github.com/CategoricalData/hydra/issues/233)): per-language `caseConventions`,
  `supportedFeatures`, and `defaultFileExtension` on the kernel `Language`,
  letting coders read emission flags from the target language rather than
  positional arguments.
- Primitive-metadata-driven laziness in otherwise-eager coders ([#391](https://github.com/CategoricalData/hydra/issues/391)):
  host registries stamp `isLazy` per argument and force thunks at call sites.
- New `/test` skill and `bin/test.sh` for target-language test validation ([#387](https://github.com/CategoricalData/hydra/issues/387)),
  closing the gap where `sync` ran only the Haskell `stack test`.
- Automatic differentiation: a `grad` transform for Hydra ([#324](https://github.com/CategoricalData/hydra/issues/324)).
- `hydra.show.error.pg` — string representations for `InvalidGraphError` ([#374](https://github.com/CategoricalData/hydra/issues/374)).

### Improvements

- Homogenized writer conventions across all target languages ([#339](https://github.com/CategoricalData/hydra/issues/339)):
  `*ToExpr` writer renames, adaptive layout for 120-width compaction.
- Disambiguated format-version field naming across JSON sidecar/metadata files ([#412](https://github.com/CategoricalData/hydra/issues/412)):
  the digest's `version` / `formatVersion` pair became `digestFormatVersion` (the digest's own
  schema) / `moduleFormatVersion` (the module-JSON wire format); both digest serializers now carry
  both. Added `manifestFormatVersion` to generated `manifest.json` files and `packageFormatVersion`
  to the hand-authored `packages/<pkg>/package.json` descriptors. All four reset to `1` for 0.16.0.
  Corrected `docs/json-format.md`, which had told consumers to read a field that lives only in the
  gitignored, non-shipped digest.
- Merged `Module.termDependencies` and `Module.typeDependencies`
  into a single `Module.dependencies` field ([#354](https://github.com/CategoricalData/hydra/issues/354)).
- Finalized `hydra.packaging` and its dependencies for 0.16 ([#369](https://github.com/CategoricalData/hydra/issues/369)): replaced
  `Binding` with `TypeDefinition` in type modules ([#396](https://github.com/CategoricalData/hydra/issues/396)), added a `comments` field to
  `Module` and `Package` ([#402](https://github.com/CategoricalData/hydra/issues/402)), and renamed residual namespace-named symbols to
  `moduleName` ([#404](https://github.com/CategoricalData/hydra/issues/404)).
- Resolved the `maybe` / `optional` ambiguity ([#401](https://github.com/CategoricalData/hydra/issues/401)): renamed the `hydra.lib.maybes`
  library to `optionals`, the host `Maybe` ADT to `Optional` (`Just`/`Nothing` →
  `given`/`none`), and the `maybeTerm` extractor to `optionalTerm`, across the
  kernel and every host.
- Order-preserving JSON objects ([#398](https://github.com/CategoricalData/hydra/issues/398), [#230](https://github.com/CategoricalData/hydra/issues/230)): `Value.object` changed from a map to an
  ordered list of `(string, Value)` pairs; `Value` equality is order-sensitive.
- Renamed `Namespace` to `ModuleName` ([#316](https://github.com/CategoricalData/hydra/issues/316)); renamed the phantom DSL types
  `TBinding`/`TTerm`/`TTermDefinition` to `TypedBinding`/`TypedTerm`/
  `TypedTermDefinition` ([#397](https://github.com/CategoricalData/hydra/issues/397)).
- Moved from a type-class enum to type-class names ([#275](https://github.com/CategoricalData/hydra/issues/275)).
- Repackaged the "eval lib" modules as kernel lib modules ([#260](https://github.com/CategoricalData/hydra/issues/260)) and promoted
  remaining unpromoted sources ([#355](https://github.com/CategoricalData/hydra/issues/355), [#356](https://github.com/CategoricalData/hydra/issues/356)).
- Re-evaluated and reshaped `hydra.context` into `InferenceContext` ([#368](https://github.com/CategoricalData/hydra/issues/368)).
- Cleaned up legacy syntax models across all heads ([#297](https://github.com/CategoricalData/hydra/issues/297)).
- Adopted efficient persistent immutable collections across hosts ([#359](https://github.com/CategoricalData/hydra/issues/359)):
  Common Lisp ([#360](https://github.com/CategoricalData/hydra/issues/360)), Emacs Lisp ([#361](https://github.com/CategoricalData/hydra/issues/361)), and Python ([#362](https://github.com/CategoricalData/hydra/issues/362)).
- Migrated kernel definition names with underscores to camelCase ([#348](https://github.com/CategoricalData/hydra/issues/348)).
- Reviewed and tidied qualified/unqualified imports in the Haskell DSL sources ([#308](https://github.com/CategoricalData/hydra/issues/308)).
- Eliminated `Hydra.Module.Compat` shim ([#315](https://github.com/CategoricalData/hydra/issues/315)).
- Build pipeline: transform DSL → JSON per-package ([#381](https://github.com/CategoricalData/hydra/issues/381)) with per-package
  incremental test inference replacing the flat-universe path ([#395](https://github.com/CategoricalData/hydra/issues/395)); generator now
  removes stale outputs in `dist/<lang>/<pkg>/` ([#357](https://github.com/CategoricalData/hydra/issues/357), [#393](https://github.com/CategoricalData/hydra/issues/393), [#405](https://github.com/CategoricalData/hydra/issues/405)); self-heal opt-in
  package digests ([#378](https://github.com/CategoricalData/hydra/issues/378)); exclude generation digests from version control ([#379](https://github.com/CategoricalData/hydra/issues/379));
  `sync.sh` reconciles generated-file drift rather than failing on it ([#392](https://github.com/CategoricalData/hydra/issues/392)); fail
  loud on decoder strictness / shell failures ([#414](https://github.com/CategoricalData/hydra/issues/414)).
- Host generation drivers now accept all valid (host, target) pairs instead of
  rejecting several with `Unknown target` ([#421](https://github.com/CategoricalData/hydra/issues/421)).
- Moved the Gradle build infrastructure into `heads/java/` ([#384](https://github.com/CategoricalData/hydra/issues/384)); removed the
  top-level `pixi.toml` ([#385](https://github.com/CategoricalData/hydra/issues/385)).
- Eliminated post-generation patches in generated tests ([#307](https://github.com/CategoricalData/hydra/issues/307)); fixed loader and
  test-runner kludges ([#309](https://github.com/CategoricalData/hydra/issues/309)); re-enabled test-timing metrics across all heads ([#311](https://github.com/CategoricalData/hydra/issues/311)).
- Performance / benchmarks work ([#277](https://github.com/CategoricalData/hydra/issues/277)); usability assessment ([#283](https://github.com/CategoricalData/hydra/issues/283)).
- Split the monolithic `hydra` Hackage distribution into per-package
  distributions ([#418](https://github.com/CategoricalData/hydra/issues/418)), mirroring the per-package layout already used for Java
  (Maven Central) and Python (PyPI). The 0.16.0 Haskell publish set is
  `hydra-kernel`, `hydra-haskell`, and a new `hydra` umbrella package whose
  `Hydra` module re-exports the kernel surface (`Hydra.Kernel`) plus the Haskell
  coder's `moduleToHaskell` / `moduleToHaskellModule`, preserving a single
  convenient entry point and the existing Hackage landing page. Inter-package
  dependencies are exact-version-pinned and published leaves-first; a
  dependency-closure guard prevents publishing a package whose Hydra
  dependencies are not themselves in the publish set. New tooling:
  `heads/haskell/bin/{assemble-haskell-distribution,publish-hackage,verify-haskell-distribution}.sh`
  and `bin/lib/generate-haskell-package-build.py`.

### Bug fixes

- Encode `uint32` JSON literals as numbers, not strings ([#431](https://github.com/CategoricalData/hydra/issues/431)):
  `uint32`'s maximum (`2^32 - 1`) is well below JavaScript's `2^53 - 1` safe-integer
  boundary, so the string encoding was unnecessary and asymmetric with `int32`.
  The decoder accepts either a number or a string for `uint32` (forward compatibility);
  the module format version is unchanged.
- Satisfied or suppressed warnings in generated Java ([#349](https://github.com/CategoricalData/hydra/issues/349)).
- Restored JSON parser test cases ([#336](https://github.com/CategoricalData/hydra/issues/336)):
  fixed polymorphic case-statement handling in the eta-expander
  (`Reduction.etaExpandTerm`) and the Java coder (`encodeElimination`),
  added a structural fallback to Python's `hydra.lib.equality.compare`
  for term values that lack a native `<` ordering.
- `termAlternatives` / `typeAlternatives` no longer drop annotations during
  adaptation ([#353](https://github.com/CategoricalData/hydra/issues/353)).
- Scala host now decodes module JSON containing term definitions ([#342](https://github.com/CategoricalData/hydra/issues/342)).
- Java coder: fixed exponential cost from eagerly evaluating `let`-bound `cases`
  defaults on rewriting hot paths ([#372](https://github.com/CategoricalData/hydra/issues/372)); emit explicit type arguments on
  `PersistentMap.ofEntries` and siblings for non-empty values ([#394](https://github.com/CategoricalData/hydra/issues/394)).
- Python-host codegen: removed extra `pyStringToPyStrings` wrapping in
  `stringToPyExpression` ([#367](https://github.com/CategoricalData/hydra/issues/367)).
- Default `escape_literal_char` no longer mangles RDF triples ([#363](https://github.com/CategoricalData/hydra/issues/363)).
- Sync robustness: stale java/python coder JSON no longer breaks sync after a
  kernel rename ([#406](https://github.com/CategoricalData/hydra/issues/406)); the sync digest no longer omits native coder modules
  ([#400](https://github.com/CategoricalData/hydra/issues/400)); removed the dead `needsThunking` helper from the legacy Java coder
  DSL ([#411](https://github.com/CategoricalData/hydra/issues/411)); `heads/java/build.gradle` no longer references non-existent
  source dirs ([#408](https://github.com/CategoricalData/hydra/issues/408)).
- Cross-host self-hosting fixes folded into the all-hosts milestone above
  (Scala emitter + TCO [#422](https://github.com/CategoricalData/hydra/issues/422), [#423](https://github.com/CategoricalData/hydra/issues/423); TypeScript inference/runner [#424](https://github.com/CategoricalData/hydra/issues/424), [#429](https://github.com/CategoricalData/hydra/issues/429), [#444](https://github.com/CategoricalData/hydra/issues/444);
  Clojure/Common-Lisp/Scheme/Emacs-Lisp loaders + coders [#366](https://github.com/CategoricalData/hydra/issues/366), [#389](https://github.com/CategoricalData/hydra/issues/389), [#407](https://github.com/CategoricalData/hydra/issues/407), [#425](https://github.com/CategoricalData/hydra/issues/425),
  [#426](https://github.com/CategoricalData/hydra/issues/426), [#427](https://github.com/CategoricalData/hydra/issues/427), [#432](https://github.com/CategoricalData/hydra/issues/432), [#438](https://github.com/CategoricalData/hydra/issues/438), [#439](https://github.com/CategoricalData/hydra/issues/439), [#443](https://github.com/CategoricalData/hydra/issues/443)).

### Removed

- Removed the `bigfloat` literal type from `FloatType` and `FloatValue` ([#330](https://github.com/CategoricalData/hydra/issues/330)).
  Arbitrary-precision floating-point values were inconsistently supported
  across host languages (Java/Scala in particular lacked a true IEEE 754
  arbitrary-precision implementation), and nothing in the codebase relied on
  them. `FloatType` and `FloatValue` now have only `float32` and `float64`
  variants. Removed library primitives: `bigfloatToBigint`, `bigfloatToFloat32`,
  `bigfloatToFloat64`, `bigintToBigfloat`, `float32ToBigfloat`,
  `float64ToBigfloat`, `readBigfloat`, `showBigfloat`, `roundBigfloat`. New
  primitives `float32ToFloat64` and `float64ToFloat32` (the latter lossy)
  replace the removed bigfloat-routed conversions.
- Removed `heads/haskell/bin/assemble-hackage-sdist.sh` ([#418](https://github.com/CategoricalData/hydra/issues/418)), the 0.15-era
  bridge that flattened the multi-source-dir Haskell head into one self-contained
  `hydra` sdist. Superseded by the per-package assembler (see Improvements).

### Documentation

- Comprehensive documentation refresh for 0.16 ([#383](https://github.com/CategoricalData/hydra/issues/383))
  spanning nine tracks across READMEs, `docs/`, the wiki, CLAUDE.md, and
  in-source kernel comments.
- New `docs/json-format.md` reference describing the canonical JSON wire format
  (tagged-union duality, optional-field rules, IEEE 754 sentinels, integer
  threshold).
- New wiki pages: `Design` (rationale behind each major choice) and `Inference`
  (HM type inference with class constraints).
- Cross-checked every prose doc against current code; corrected drift in
  primitive-checking sections of `docs/recipes/maintenance.md`, the `Primitive`
  shape in `docs/implementation.md` and `docs/recipes/adding-primitives.md`,
  and the build-system narrative.
- Wove motivation ("the why") through the existing doc surface rather than
  collecting it into separately-titled sidebars; landed an inference-overview
  centerpiece on the wiki ([#377](https://github.com/CategoricalData/hydra/issues/377)).
- Aligned doc vocabulary with the LambdaGraph paper and the literature review
  for the design space Hydra inhabits.
- Net-new READMEs for `packages/hydra-kernel`, `packages/hydra-ext`,
  `packages/hydra-coq`, and `packages/hydra-wasm`; top-level `README.md`
  now carries a canonical packages table.
- Audited and refreshed the Claude surface: CLAUDE.md "Where to look up X"
  table extended with rows for `docs/getting-started.md`, `docs/json-format.md`,
  and the `Design` / `Inference` wiki pages; hard-rules block reordered by
  violation frequency; `claude/pitfalls.md` superseded entries folded into
  `docs/troubleshooting.md` / recipes.
- Project-descriptor strings reviewed and aligned across 24 build / packaging
  files (Hackage synopsis, Maven Central description, PyPI / conda-forge
  classifiers, Hydra-format `package.json` files).
- `docs/index.html` (the GitHub Pages landing page) refreshed with audience
  hops into wiki Concepts / Design, `docs/getting-started.md`, and the GitHub
  repo, plus the full published-artifact list across Hackage, Maven Central,
  PyPI, and conda-forge.
- In-source kernel-code rationale comments added at key Sources sites covering
  injection / pair / either / unit-void duality, error taxonomy, variant
  records, inference-context threading, validation profiles, reduction state,
  lexical filters, name mangling, scoping, variable handling, defaults
  integration, and Tarjan SCC choice rationale. JSON encode/decode modules
  cross-reference `docs/json-format.md`.
- Filed `Primitive.implementation` carrier-simplification follow-up
  ([#446](https://github.com/CategoricalData/hydra/issues/446))
  to drop the unused `InferenceContext` and `Graph` parameters once the
  `Lib/Defaults/` integration ([#437](https://github.com/CategoricalData/hydra/issues/437))
  lands.

---

## [0.15.0] - 2026-04-29

A structural-cleanup release.
The repository is reorganized into a three-tree
`packages/` (DSL sources) / `heads/` (per-host runtime) / `dist/` (generated output)
layout ([#290](https://github.com/CategoricalData/hydra/issues/290)), the kernel is simplified ([#251](https://github.com/CategoricalData/hydra/issues/251), [#292](https://github.com/CategoricalData/hydra/issues/292), [#332](https://github.com/CategoricalData/hydra/issues/332)), and Java + Python now
ship as per-package publishable artifacts on Maven Central / PyPI / conda-forge
([#305](https://github.com/CategoricalData/hydra/issues/305)).
Incremental, content-hash-based caches accelerate the sync pipeline ([#247](https://github.com/CategoricalData/hydra/issues/247)).
Three new generation-only targets — Coq ([#326](https://github.com/CategoricalData/hydra/issues/326)), WebAssembly ([#325](https://github.com/CategoricalData/hydra/issues/325)), and an
automatic-differentiation demo ([#324](https://github.com/CategoricalData/hydra/issues/324)) — are in progress as Claude collaborations.

### Highlights

- **Three-tree repository layout**: `packages/` (DSL), `heads/` (host runtime),
  `dist/` (generated) ([#290](https://github.com/CategoricalData/hydra/issues/290)).
- **Per-package distributions** for Java (4 Maven artifacts) and Python (5 wheels):
  `hydra-kernel`, `hydra-pg`, `hydra-rdf`, `hydra-{java,python}`, plus `hydra-ext`
  on PyPI ([#305](https://github.com/CategoricalData/hydra/issues/305)).
- **Kernel simplifications**: `Function`/`Elimination` removed ([#332](https://github.com/CategoricalData/hydra/issues/332)),
  `FunctionPrimitive` removed ([#251](https://github.com/CategoricalData/hydra/issues/251)), `Hydra.Ext.*` prefix retired ([#331](https://github.com/CategoricalData/hydra/issues/331)),
  `Context` error wrapper gone ([#292](https://github.com/CategoricalData/hydra/issues/292)).
- **Incremental caches** across the sync pipeline; ~70x speedup on no-op resync ([#247](https://github.com/CategoricalData/hydra/issues/247)).
- **JSON kernel format v1** with formal wire-format spec and four-field rename ([#343](https://github.com/CategoricalData/hydra/issues/343)).
- **Three new generation-only targets** (in progress): Coq ([#326](https://github.com/CategoricalData/hydra/issues/326)), WebAssembly ([#325](https://github.com/CategoricalData/hydra/issues/325)),
  automatic-differentiation demo ([#324](https://github.com/CategoricalData/hydra/issues/324)).

### New features

- Per-package distributions ([#305](https://github.com/CategoricalData/hydra/issues/305)): standalone Maven / PyPI / conda-forge builds,
  generated `build.gradle` / `pyproject.toml` per package, transitive dep resolution.
- JSON kernel format v1 ([#343](https://github.com/CategoricalData/hydra/issues/343)): formal spec at `docs/json-format.md`,
  four-field rename to `typeScheme`, `Module` field reorder, `formatVersion` stamp.
- `decimal` literal type in the kernel ([#338](https://github.com/CategoricalData/hydra/issues/338)) across all hosts.
- Type-directed JSON encoder with idiomatic optional encoding ([#314](https://github.com/CategoricalData/hydra/issues/314), [#318](https://github.com/CategoricalData/hydra/issues/318)).
- Incremental type inference plus content-hash caches ([#247](https://github.com/CategoricalData/hydra/issues/247)).
- Coq generation target ([#326](https://github.com/CategoricalData/hydra/issues/326), in progress): `hydra-coq` package, 131/131 `.v` files pass `coqc`.
- WebAssembly target ([#325](https://github.com/CategoricalData/hydra/issues/325), in progress): `hydra-wasm` package, M4 closure mechanism.
- Automatic-differentiation demo ([#324](https://github.com/CategoricalData/hydra/issues/324), in progress): symbolic source-to-source.
- `hydra.show.error` module ([#265](https://github.com/CategoricalData/hydra/issues/265)): consolidated error-message builders.
- `hydra.lib.maybes.toList` primitive ([#257](https://github.com/CategoricalData/hydra/issues/257)).
- NaN / Inf round-tripping through JSON and per-language serdes ([#312](https://github.com/CategoricalData/hydra/issues/312), [#318](https://github.com/CategoricalData/hydra/issues/318), [#330](https://github.com/CategoricalData/hydra/issues/330)).
- Typeclass inference test group ([#274](https://github.com/CategoricalData/hydra/issues/274)).
- Term and type linters ([#232](https://github.com/CategoricalData/hydra/issues/232)).
- Validation for modules and graphs ([#155](https://github.com/CategoricalData/hydra/issues/155)).
- User-defined-function (UDF) support and DSL-vs-primitive overrides
  ([#62](https://github.com/CategoricalData/hydra/issues/62), [#63](https://github.com/CategoricalData/hydra/issues/63), [#102](https://github.com/CategoricalData/hydra/issues/102), [#158](https://github.com/CategoricalData/hydra/issues/158)).
- Stale-source detection across sync entrypoints ([#228](https://github.com/CategoricalData/hydra/issues/228)).

### Improvements

- Sync infrastructure: matrix tool, per-package orchestrator, per-language wrappers ([#290](https://github.com/CategoricalData/hydra/issues/290)).
- TestGraph post-generation patches eliminated; DSL emits `TestEnv` refs directly.
- Minimized standard imports in generated Haskell ([#161](https://github.com/CategoricalData/hydra/issues/161)); improved auto-aliases ([#322](https://github.com/CategoricalData/hydra/issues/322)).
- Removed kernel term dependencies from the interpreter ([#257](https://github.com/CategoricalData/hydra/issues/257)).
- Removed `Graph` and `Context` arguments from primitive implementations ([#266](https://github.com/CategoricalData/hydra/issues/266)).
- Refactored kernel term modules: split rewriting/schemas, renamed show.meta → codegen,
  moved `Coder` to `coders`, merged `extract.helpers` ([#221](https://github.com/CategoricalData/hydra/issues/221)).
- Removed Aeson and HsYAML dependencies; YAML coder moved to `hydra-ext` ([#261](https://github.com/CategoricalData/hydra/issues/261)).
- Lisp recursive let bindings via SCC ([#341](https://github.com/CategoricalData/hydra/issues/341)); parenthesis cleanup ([#84](https://github.com/CategoricalData/hydra/issues/84)).
- Everything-to-everything bootstrapping demo ([#254](https://github.com/CategoricalData/hydra/issues/254)).
- Re-enabled skipped Python tests; `disabledForPython` retired ([#263](https://github.com/CategoricalData/hydra/issues/263)).
- Promoted host-specific code in `packages/` into per-head locations ([#337](https://github.com/CategoricalData/hydra/issues/337)).
- Migrated `hydra.common` Java module out ([#10](https://github.com/CategoricalData/hydra/issues/10)).
- Test infrastructure unification under `UniversalTestCase` ([#246](https://github.com/CategoricalData/hydra/issues/246)).
- Default property graph → RDF mapping ([#296](https://github.com/CategoricalData/hydra/issues/296)).
- 13 unsafe partial primitives replaced with `Maybe`-returning alternatives ([#201](https://github.com/CategoricalData/hydra/issues/201)).
- Java generated code uses standard `java.util` collection interfaces ([#313](https://github.com/CategoricalData/hydra/issues/313)).
- `Term.union` renamed to `Term.inject` ([#334](https://github.com/CategoricalData/hydra/issues/334)).
- Inconsistencies in domain DSLs cleaned up ([#219](https://github.com/CategoricalData/hydra/issues/219)).
- Coder packages moved out of hydra-haskell ([#182](https://github.com/CategoricalData/hydra/issues/182)).
- Java package-naming scheme revised ([#223](https://github.com/CategoricalData/hydra/issues/223)).
- Decimal used (instead of bigfloat) for JSON numbers ([#340](https://github.com/CategoricalData/hydra/issues/340)).
- Hydra kernel moved out of hydra-haskell ([#282](https://github.com/CategoricalData/hydra/issues/282)).

### Bug fixes

- `substInTypeScheme` capture-unsafe substitution under nested forall binders.
- Java `Float64` arithmetic primitives corrected for current `PrimitiveFunction` API.
- Clojure decimal handling: value-based equality, `BigInteger` return from `decimal_to_bigint`.
- Scheme runtime R7RS imports for `sets.scm` / `maps.scm` / `eithers.map_set`.
- Python coder defaults corrected from `Py.Name` to `Py.Expression`.
- Scala host JSON-decode failure on modules containing term definitions ([#342](https://github.com/CategoricalData/hydra/issues/342)).
- Lisp coder mutually-recursive let bindings for Clojure ([#341](https://github.com/CategoricalData/hydra/issues/341)).

### Documentation

- Documentation refresh for the 0.15 packaging restructure ([#290](https://github.com/CategoricalData/hydra/issues/290), [#331](https://github.com/CategoricalData/hydra/issues/331)); CLAUDE.md,
  READMEs, recipes, demos, and seven wiki pages updated.
- Code-generation recipe rewritten for the per-package dist layout ([#282](https://github.com/CategoricalData/hydra/issues/282)).
- New `maintenance.md` recipe consolidates non-source-file scans, stale generated
  detection, design-violation checks, and freshness checks.
- Documentation style guide added at `docs/documentation-style-guide.md`.
- Cross-worktree messaging protocol documented in CLAUDE.md.
- Decimal type documented in the lexicon and per-language READMEs ([#338](https://github.com/CategoricalData/hydra/issues/338)).
- JSON encoding format docs updated for idiomatic optional encoding ([#314](https://github.com/CategoricalData/hydra/issues/314)).

---

## [0.14.1] - 2026-03-30

Patch release.
Regenerates all implementations after a fix to the group eval primitive ([#281](https://github.com/CategoricalData/hydra/issues/281)),
and relocates the property-graph and RDF/SHACL "ext" modules into hydra-java and hydra-python.

### Improvements

- Moved PG, RDF, and SHACL ext modules from hydra-ext into hydra-java and hydra-python.

### Bug fixes

- Group eval primitive fix; all targets regenerated ([#281](https://github.com/CategoricalData/hydra/issues/281)).
- JavaDoc HTML-entity escaping in generated `DestructuringPattern.java`.

---

## [0.14.0] - 2026-03-29

Major release adding four new complete Hydra implementations — Scala ([#273](https://github.com/CategoricalData/hydra/issues/273)),
Clojure ([#278](https://github.com/CategoricalData/hydra/issues/278)), Common Lisp, and Scheme — bringing the total to seven bootstrapping hosts
(plus Emacs Lisp, which passes the test suite but does not yet participate in bootstrapping).
Also includes kernel simplifications, an Avro bidirectional coder ([#301](https://github.com/CategoricalData/hydra/issues/301)),
eval primitives ([#281](https://github.com/CategoricalData/hydra/issues/281)), new demos, and significant performance work on the Scheme host.

### Highlights

- **Four new complete implementations**: Hydra-Scala ([#273](https://github.com/CategoricalData/hydra/issues/273)), Hydra-Clojure ([#278](https://github.com/CategoricalData/hydra/issues/278)),
  Hydra-Common Lisp, and Hydra-Scheme pass the common test suite and can serve as
  bootstrapping hosts.
  The four Lisp dialects share a single coder and serializer.
- **Kernel simplifications**: removed `RowType`/`WrappedType` ([#82](https://github.com/CategoricalData/hydra/issues/82)),
  unified `Graph`/`TypeContext`/`InferenceContext` ([#192](https://github.com/CategoricalData/hydra/issues/192)),
  replaced `Module.elements` with `Module.definitions` ([#214](https://github.com/CategoricalData/hydra/issues/214)),
  consolidated error types ([#268](https://github.com/CategoricalData/hydra/issues/268)), and promoted all staging modules to DSL sources ([#267](https://github.com/CategoricalData/hydra/issues/267)).
- **Avro bidirectional coder** ([#301](https://github.com/CategoricalData/hydra/issues/301)) with property graph pipeline demo.
- **Scheme bootstrap performance**: 58 minutes → 5.7 minutes via O(1) vhash data structures
  and IEEE 754 float precision fixes.

### Breaking changes

- Removed legacy adapter framework; replaced by the staging-promoted variant ([#236](https://github.com/CategoricalData/hydra/issues/236), [#295](https://github.com/CategoricalData/hydra/issues/295)).
- Replaced legacy Flow monad with Either ([#245](https://github.com/CategoricalData/hydra/issues/245)).
- Removed `Module.elements`; use `Module.definitions` ([#214](https://github.com/CategoricalData/hydra/issues/214)).
- Removed `RowType` and `WrappedType` ([#82](https://github.com/CategoricalData/hydra/issues/82)).
- Renamed `hydra.accessors` to `hydra.paths` ([#271](https://github.com/CategoricalData/hydra/issues/271)).
- Renamed `hydra.tarjan` and merged into `hydra.sorting` ([#220](https://github.com/CategoricalData/hydra/issues/220)).
- Merged `hydra.constraints` into `hydra.query` ([#272](https://github.com/CategoricalData/hydra/issues/272)).
- Moved `hydra.workflow` to hydra-ext ([#270](https://github.com/CategoricalData/hydra/issues/270)).
- Consolidated `hydra.compute` into `hydra.util` ([#269](https://github.com/CategoricalData/hydra/issues/269)).
- Removed `hydra.describe` ([#216](https://github.com/CategoricalData/hydra/issues/216)) and types from `hydra.mantle` ([#217](https://github.com/CategoricalData/hydra/issues/217)).
- Replaced `OtherError` with task-specific error types ([#268](https://github.com/CategoricalData/hydra/issues/268)).
- Java: lowered minimum Java version to 11 ([#249](https://github.com/CategoricalData/hydra/issues/249)).
- Java: `Pair` replaces `Tuple2` ([#252](https://github.com/CategoricalData/hydra/issues/252)); `Comparable` implemented for generated classes ([#131](https://github.com/CategoricalData/hydra/issues/131)).

### New features

- Hydra-Scala ([#273](https://github.com/CategoricalData/hydra/issues/273)): complete implementation, sbt/Scala 3, 3043 tests.
- Hydra-Lisp: Clojure ([#278](https://github.com/CategoricalData/hydra/issues/278)), Common Lisp, Emacs Lisp, Scheme — four dialects,
  one coder. CI workflows for Clojure, Common Lisp, and Scheme.
- Avro bidirectional coder ([#301](https://github.com/CategoricalData/hydra/issues/301)) with demo.
- Eval primitives ([#281](https://github.com/CategoricalData/hydra/issues/281)): Logic, Equality, Math, collections, and Groups.
- Generated DSL modules ([#180](https://github.com/CategoricalData/hydra/issues/180)): `hydra.dsls` generates DSL helpers from type modules.
- New demos: GraphQL+JSON ([#279](https://github.com/CategoricalData/hydra/issues/279)), PG validation ([#284](https://github.com/CategoricalData/hydra/issues/284)), SHACL ([#294](https://github.com/CategoricalData/hydra/issues/294)), Avro+PG.
- Packaging module ([#290](https://github.com/CategoricalData/hydra/issues/290)): `hydra.packaging` with `Package` type and validation.
- New primitives: `lists.foldr` ([#280](https://github.com/CategoricalData/hydra/issues/280)), `maybes.toList`, float rounding ([#264](https://github.com/CategoricalData/hydra/issues/264), [#285](https://github.com/CategoricalData/hydra/issues/285)).
- Cross-language benchmarking suite ([#234](https://github.com/CategoricalData/hydra/issues/234)).
- Limited typeclass inference ([#164](https://github.com/CategoricalData/hydra/issues/164)).
- Standardized validation patterns across modules ([#291](https://github.com/CategoricalData/hydra/issues/291)).

### Improvements

- Promoted all staging modules to DSL sources ([#267](https://github.com/CategoricalData/hydra/issues/267)).
- Removed legacy adapters, JSON/YAML coders, grammar framework ([#236](https://github.com/CategoricalData/hydra/issues/236), [#295](https://github.com/CategoricalData/hydra/issues/295)).
- Java: efficient immutable data structures ([#193](https://github.com/CategoricalData/hydra/issues/193)); flattened term naming.
- Java: hidden specialized collection classes in type-level files ([#313](https://github.com/CategoricalData/hydra/issues/313)).
- Inference error messages improved ([#231](https://github.com/CategoricalData/hydra/issues/231)).
- Combined nested let bindings in the Haskell coder ([#248](https://github.com/CategoricalData/hydra/issues/248)).
- Simplified `hydra.adapt.simple` ([#255](https://github.com/CategoricalData/hydra/issues/255)).
- Removed Haskell operators dependency from tests ([#288](https://github.com/CategoricalData/hydra/issues/288)).
- Regression tests for non-Haskell/Java coders ([#85](https://github.com/CategoricalData/hydra/issues/85)).
- Regex support added ([#293](https://github.com/CategoricalData/hydra/issues/293)).
- `hydra.annotations.isNativeType` revisited ([#214](https://github.com/CategoricalData/hydra/issues/214)).
- Migrated grammar-generated models ([#295](https://github.com/CategoricalData/hydra/issues/295)).
- `<<= -shouldBe-> <<~` operator change ([#276](https://github.com/CategoricalData/hydra/issues/276)).
- Minimize Generation.hs ([#225](https://github.com/CategoricalData/hydra/issues/225)).
- Implemented missing Flows primitives ([#183](https://github.com/CategoricalData/hydra/issues/183)).
- Removed JavaDocs from the repository ([#262](https://github.com/CategoricalData/hydra/issues/262)).
- Floating-point test portability fixes for Linux CI ([#264](https://github.com/CategoricalData/hydra/issues/264), [#285](https://github.com/CategoricalData/hydra/issues/285)).
- Moved YAML coder to hydra-ext ([#224](https://github.com/CategoricalData/hydra/issues/224)).
- Minimized native Python and Java primitives in favor of DSL implementations ([#244](https://github.com/CategoricalData/hydra/issues/244)).
- Removed the `disabledForPython` test tag ([#250](https://github.com/CategoricalData/hydra/issues/250)).

### Release tooling

- 0.14 release tracking ([#304](https://github.com/CategoricalData/hydra/issues/304)).

### Documentation

- Thorough cleanup of user documentation ([#302](https://github.com/CategoricalData/hydra/issues/302), [#303](https://github.com/CategoricalData/hydra/issues/303)).
- Coding style guide added to wiki.
- Updated 'new implementation' recipe with learnings from Clojure head ([#278](https://github.com/CategoricalData/hydra/issues/278)).

---

## [0.13.0] - 2026-02-27

Major release completing Hydra-Python ([#66](https://github.com/CategoricalData/hydra/issues/66)) and Hydra-Java ([#166](https://github.com/CategoricalData/hydra/issues/166)) as self-hosting Hydra implementations
and demonstrating mutual self-hosting across all three languages.
Significant improvements to type inference, type checking, rewriting, and adaptation.
New language features include first-class Either types, typeclass inference, binary data support,
and a native JSON parser/writer.
Comprehensive tooling for cross-implementation bootstrapping
and provisional support for Go, Rust, and JavaScript targets.

### Highlights

- **Mutual self-hosting across three implementations**: Hydra is now fully self-hosting
  in Haskell ([#179](https://github.com/CategoricalData/hydra/issues/179)), Java, and Python, and can cross-generate between every combination
  of host and target language (9 paths total).
- **Hydra-Java is complete** ([#166](https://github.com/CategoricalData/hydra/issues/166)): all kernel and generation tests pass.
- **Hydra-Python is complete** ([#66](https://github.com/CategoricalData/hydra/issues/66)): 100% test parity with Haskell.
- **Language coders promoted into the Hydra kernel** ([#176](https://github.com/CategoricalData/hydra/issues/176)): Haskell, Java, and Python
  coders are now defined as Hydra DSL modules, making them self-hosting.
- **Either type support** ([#210](https://github.com/CategoricalData/hydra/issues/210)) with full inference, checking, and library support.
- **Native JSON parser and writer** ([#188](https://github.com/CategoricalData/hydra/issues/188), [#243](https://github.com/CategoricalData/hydra/issues/243), [#253](https://github.com/CategoricalData/hydra/issues/253)): Hydra's own JSON parser and writer
  replace the Aeson dependency; bootstrap-from-JSON architecture for cross-implementation
  code generation.

### Breaking changes

- Renamed `Optional` to `Maybe` throughout the codebase ([#204](https://github.com/CategoricalData/hydra/issues/204)):
  `hydra.lib.optionals` → `hydra.lib.maybes`; all `optional` variants renamed to `maybe`.
- Removed unlabeled product and sum types; replaced with pair types ([#212](https://github.com/CategoricalData/hydra/issues/212)).
- Renamed `hydra.core.TypedTerm` to `hydra.core.TypeApplicationTerm`.
- Renamed `hydra.json` to `hydra.json.model`.
- Renamed `hydra.lib.maps.remove` to `hydra.lib.maps.delete`.
- Removed deprecated `hydra.decoding` module.
- Removed old `hydra.mantle.Either` type (replaced by core `Either`).
- Removed `hydra.lib.tuples` library.
- Removed C# coder stub (C# syntax model remains).
- DSL syntax migration: removed `OverloadedStrings` for term expressions ([#238](https://github.com/CategoricalData/hydra/issues/238)).
- Removed `Hydra.Dsl.ShorthandTypes` ([#222](https://github.com/CategoricalData/hydra/issues/222)).

### New features

- Self-hosting in Haskell ([#179](https://github.com/CategoricalData/hydra/issues/179)) and full mutual cross-generation (Haskell ↔ Java ↔ Python).
- New bootstrapping demo validating all 9 paths across 249 modules.
- Promoted Haskell, Java, and Python coders from staging code into Hydra DSL sources ([#176](https://github.com/CategoricalData/hydra/issues/176)).
- JSON-as-source-of-truth bootstrapping architecture ([#243](https://github.com/CategoricalData/hydra/issues/243), [#253](https://github.com/CategoricalData/hydra/issues/253)):
  new executables and scripts enabling `bootstrap-from-json` for cross-implementation
  code generation.
- Generated term encoders and decoders replace legacy hand-coded modules ([#47](https://github.com/CategoricalData/hydra/issues/47)).
- Hydra-Python completion ([#66](https://github.com/CategoricalData/hydra/issues/66)): restructured project layout with `src/main` and
  `src/gen-main` ([#191](https://github.com/CategoricalData/hydra/issues/191)); full library implementations including `hydra.lib.flows`;
  `@lru_cache` optimization for nullary bindings; PyPy compatibility.
- Hydra-Java completion ([#166](https://github.com/CategoricalData/hydra/issues/166)): all kernel and generation tests pass; deep changes
  to Java coder; `Lazy` utility class for delayed evaluation; array-type support
  in Java serde; tail-call optimization in the Java coder; `Tuple` implements `Comparable`.
- Either type ([#210](https://github.com/CategoricalData/hydra/issues/210)): `EitherType`, `either` term constructor, `hydra.lib.eithers` library
  with `either`, `map`, `mapList`, `mapMaybe`, `bimap`, `bind`, `fromLeft`, `fromRight`,
  `isLeft`, `isRight`, `lefts`, `rights`, `partitionEithers`.
- Pair type as a core construct ([#211](https://github.com/CategoricalData/hydra/issues/211)).
- Void type ([#237](https://github.com/CategoricalData/hydra/issues/237)): uninhabited `void` type as the dual of `unit`; type-level only.
- Native JSON parser and writer ([#188](https://github.com/CategoricalData/hydra/issues/188), [#242](https://github.com/CategoricalData/hydra/issues/242)):
  bidirectional JSON encoder/decoder; replaces the Aeson dependency;
  cross-checked via a special test runner.
- Typeclass inference ([#164](https://github.com/CategoricalData/hydra/issues/164)):
  type schemes with typeclass constraints via `TypeVariableMetadata`;
  typeclass metadata on primitive definitions; Haskell coder uses typeclass info.
- Binary/`ByteString` support ([#172](https://github.com/CategoricalData/hydra/issues/172)): binary data via `ByteString` instead of `String`;
  Haskell coder uses `Data.ByteString`; new `binaryToBytes` primitive.
- Parser combinators: new `hydra.parsing` module with combinator types and DSL.
- Higher-order primitive interpreter ([#198](https://github.com/CategoricalData/hydra/issues/198)): all primitives are fully interpretable
  across all three implementations; legacy `requiresInterp` tag removed.
- Let hoisting and flattening: hoisting polymorphic let terms;
  refined hoisting for monomorphic bindings inside polymorphic ones;
  more efficient `liftLambdaAboveLet` ([#202](https://github.com/CategoricalData/hydra/issues/202));
  case-statement hoisting ([#241](https://github.com/CategoricalData/hydra/issues/241)); subterm hoisting helpers.
- Provisional language targets: Go ([#65](https://github.com/CategoricalData/hydra/issues/65)), Rust (with experimental type-level coder),
  JavaScript (syntax model, serde, DSL, generated sources), and Java/Python syntax DSLs.
- New adapter framework ([#236](https://github.com/CategoricalData/hydra/issues/236)): all external coders refactored
  (Haskell, YAML, Scala, JSON Schema, C++, PDL, Protobuf, GraphQL).
- Compact labeled records and variants ([#122](https://github.com/CategoricalData/hydra/issues/122)).
- New primitives:
  `hydra.lib.lists.find`, `hydra.lib.lists.partition`,
  `hydra.lib.flows.withDefault`, `hydra.lib.flows.foldl`,
  `hydra.lib.math.{max,min,abs,pred,signum,succ,even,odd}`,
  `hydra.lib.pairs.bimap`, `hydra.lib.eithers.bind`,
  `hydra.lib.literals.binaryToBytes`, complete `readXxx`/`showXxx` families,
  `hydra.lexical.chooseUniqueName`, `neg` renamed to `negate`,
  full floating-point math primitives in Python ([#208](https://github.com/CategoricalData/hydra/issues/208)).
- DeepCore DSL: a layer one level deeper than the Meta DSLs,
  for programs that construct programs that construct terms.
- `typeOf` implementation ([#168](https://github.com/CategoricalData/hydra/issues/168)).
- Implement `Comparable` for generated Java classes ([#131](https://github.com/CategoricalData/hydra/issues/131)); update of Hydra-Java ([#166](https://github.com/CategoricalData/hydra/issues/166)).
- Compact labeled records exploration ([#122](https://github.com/CategoricalData/hydra/issues/122)).
- Comparison primitives without typeclasses ([#187](https://github.com/CategoricalData/hydra/issues/187)).
- Investigated core support for Either and Flow monads ([#200](https://github.com/CategoricalData/hydra/issues/200)).

### Improvements

- Type system:
  - Refactored `hydra.inference` to create new module `hydra.checking`.
  - Inference checks after unification to prevent schema names from being unified
    with inferred type variables.
  - Type application terms preserved when preparing application terms for Python.
  - Sanity-check tuple projection index against arity during inference.
  - Bind lost type variables in term annotations.
  - Made `extendTypeContextForLet` tolerant of untyped bindings.
- Code generation:
  - Python: support for deeply-nested match statements; more complete `let` support;
    Pythonic syntax for polymorphic function definitions; refinements for complex
    case-hoisting scenarios; performance improvements ([#209](https://github.com/CategoricalData/hydra/issues/209), [#239](https://github.com/CategoricalData/hydra/issues/239), [#240](https://github.com/CategoricalData/hydra/issues/240)); inline
    let/case rewriting for Python targets ([#203](https://github.com/CategoricalData/hydra/issues/203)).
  - Java: method declaration style consistent with calling style.
  - Haskell: standardized typeclass names.
  - Increased maximum trace depth to support more complex sources.
- Common test suite promotion: all tests promoted into a shared, implementation-independent
  test kernel ([#213](https://github.com/CategoricalData/hydra/issues/213)).
- Statically-compiled test suite in Haskell ([#207](https://github.com/CategoricalData/hydra/issues/207)).
- Included all Hydra kernel types in test schema ([#205](https://github.com/CategoricalData/hydra/issues/205)).
- Reorganized Haskell DSL definitions; standardized imports.
- Added TODO comments to all unsafe primitives ([#201](https://github.com/CategoricalData/hydra/issues/201)).
- New `rewriteAndFoldTerm` utility for simultaneous rewriting and folding.
- Lexical helper for dereferencing schema types through aliases.
- Made JSON parser lazy with new `lazy` parser combinator.
- More efficient substitution helpers for empty substitutions.
- Precomputed type/inference context in kernel test runner for performance.
- Property graph encoding and decoding modules promoted into DSL.
- `hydra.tabular` and `hydra.pg.graphson` modules promoted into DSL.
- `hydra.show.*` modules generated into Java and Python.
- Aligned epsilon encoding with the LambdaGraph spec ([#89](https://github.com/CategoricalData/hydra/issues/89)).
- Fixed asymmetry of introductions and eliminations ([#86](https://github.com/CategoricalData/hydra/issues/86), [#134](https://github.com/CategoricalData/hydra/issues/134)).
- Minimized inferred type annotations ([#114](https://github.com/CategoricalData/hydra/issues/114)).
- Investigated combining products with records, sums with unions ([#196](https://github.com/CategoricalData/hydra/issues/196), [#212](https://github.com/CategoricalData/hydra/issues/212)).
- Refined type reduction in adapters ([#144](https://github.com/CategoricalData/hydra/issues/144)).
- Investigation of unification issues ([#163](https://github.com/CategoricalData/hydra/issues/163)).
- Refactored RowType ([#82](https://github.com/CategoricalData/hydra/issues/82)).
- LLM-assisted PG schemas and mappings ([#171](https://github.com/CategoricalData/hydra/issues/171)).
- Simplified unit-valued variants in Python ([#206](https://github.com/CategoricalData/hydra/issues/206)).
- Checked for inconsistencies between Term/TTerm and Type/TType DSLs ([#218](https://github.com/CategoricalData/hydra/issues/218)).
- GenPG demo: refactored to support Haskell, Python, and Java; Python runner;
  Java generation; finalized output format.
- Testing: extended test runners for let hoisting (Haskell, Python, Java);
  floating-point precision tests; ordering tests for `hydra.lib.maps`/`hydra.lib.sets`;
  inference tests for inferred System F terms; case-hoisting test cases; new JSON coder tests;
  thorough test cases for all primitives ([#199](https://github.com/CategoricalData/hydra/issues/199)).

### Bug fixes

- Fixed interpreter bugs causing test failures ([#235](https://github.com/CategoricalData/hydra/issues/235)).
- Fixed bug in type checking in connection with dead code.
- Fixed bug in `removeTypesFromTerm`.
- Fixed bug in `typeOfMap`.
- Fixed 32-bit max int value in `hydra.constants`.
- Fixed numeric precision in Python.
- Added `BigDecimal` support and fixed `uint8` (short) support in Java.
- Fixed Haskell coder with respect to imports for binary literals.
- Excluded `Prelude.encodeFloat` and `Prelude.decodeFloat` from generated Haskell
  due to name collisions.
- Fixed consistency issues in Python primitives:
  `hydra.lib.sets.toList`, `hydra.lib.maps.union` (precedence),
  `hydra.lib.strings.lines`, `hydra.lib.strings.readString` (dequoting),
  `hydra.lib.lists.apply`, `hydra.lib.maps.toList`.
- Fixed Haskell type signature of `hydra.lists.span`.
- Fixed issue with eta expansion of typed terms.
- Fixed shadowing issue in generated encoding modules.
- Added missing alternatives in `hydra.rewriting.rewriteTermM`.

### Documentation

- Updated main README with complete implementation status.
- Added Documentation sections to all implementation READMEs.
- Fixed outdated Code-organization.md (Python now uses `src/gen-main`).
- Fixed incorrect code paths in Implementation.md.
- Added Java section to Testing wiki.
- Comprehensive developer recipes in `docs/recipes/`.
- Documentation for tail-call optimization implementation.
- Added Haddock comments to Haskell primitives.
- Created wiki page on the release process ([#194](https://github.com/CategoricalData/hydra/issues/194)).

### Community

- Accepted babeloff's `isTrivialTerm` changes.

---

## [0.12.0] - 2025-08-28

A consolidation release: significant Python progress, kernel-level cleanup,
and a wave of bookkeeping closures for Hydra-Java module ports.

### Breaking changes

- Removed `hydra.decoding` module ([#190](https://github.com/CategoricalData/hydra/issues/190)).
- Flattened dependency tiers; eliminated tier-1, tier-2, tier-3 organization ([#184](https://github.com/CategoricalData/hydra/issues/184)).
- Removed `typed` term variant ([#173](https://github.com/CategoricalData/hydra/issues/173), [#162](https://github.com/CategoricalData/hydra/issues/162)).
- Module naming changes: moved tabular and relational modules into kernel ([#152](https://github.com/CategoricalData/hydra/issues/152)).
- Standardized on uncurried helper functions in term-level DSLs ([#174](https://github.com/CategoricalData/hydra/issues/174)).
- Renamed `hydra.lib.flows.traverseOptional` to `hydra.lib.flows.mapOptional`.
- Eliminated camel-cased namespace parts from the kernel ([#152](https://github.com/CategoricalData/hydra/issues/152)).
- Replaced list and optional eliminations with primitive functions ([#150](https://github.com/CategoricalData/hydra/issues/150)).
- Moved `Hydra.Ext` to hydra-ext ([#178](https://github.com/CategoricalData/hydra/issues/178)); moved 'ext' sources into hydra-ext ([#189](https://github.com/CategoricalData/hydra/issues/189)).

### New features

- Significant progress toward Hydra-Python completion:
  generated all kernel types and terms into Python;
  added `Decimal` support for bigfloat values;
  environment tracking for variable scoping;
  generated `hydra.languages` into Python;
  `__hash__` and `__eq__` on serializable generated classes ([#160](https://github.com/CategoricalData/hydra/issues/160)).
- Designated `unit` term and type variants ([#186](https://github.com/CategoricalData/hydra/issues/186)).
- `hydra.lib.flows.mapElems` and `hydra.lib.flows.mapKeys` utilities.
- Promoted JSON coder into the DSL ([#181](https://github.com/CategoricalData/hydra/issues/181)).
- Promoted JSON utilities into the DSL ([#181](https://github.com/CategoricalData/hydra/issues/181)).
- C# coder ([#139](https://github.com/CategoricalData/hydra/issues/139)).
- GQL model and parser ([#140](https://github.com/CategoricalData/hydra/issues/140)).
- Tabular adapters ([#142](https://github.com/CategoricalData/hydra/issues/142)).
- Type-level C++ coder ([#170](https://github.com/CategoricalData/hydra/issues/170)).
- Variable-types-to-Haskell-typedefs transformation ([#70](https://github.com/CategoricalData/hydra/issues/70)).
- A newline at the end of each generated file ([#154](https://github.com/CategoricalData/hydra/issues/154)).
- Refactor kernel modules and primitives by connectivity ([#177](https://github.com/CategoricalData/hydra/issues/177)).

### Improvements

- Condensed repeated elements in error traces ([#165](https://github.com/CategoricalData/hydra/issues/165)).
- Fixed transitive schema dependencies ([#185](https://github.com/CategoricalData/hydra/issues/185)).
- Improved adapter solution with respect to literals.
- Better handling of type variables in Python coder.
- Enhanced term-level Python generation with proper ordering to minimize forward references.
- Topo-sort helper function for dependency management.
- Updated all Python libraries with latest changes.
- Distinguished between deannotation and detyping of terms.
- Adapted primitives when applying language constraints to a graph.
- Use unqualified names where possible in generated code ([#153](https://github.com/CategoricalData/hydra/issues/153)).
- Eliminated `Data.Graph` (containers) dependency ([#167](https://github.com/CategoricalData/hydra/issues/167)).
- "Close the loop" demonstration ([#175](https://github.com/CategoricalData/hydra/issues/175)).
- Implemented `typeOf` ([#168](https://github.com/CategoricalData/hydra/issues/168)).

### Bug fixes

- Fixed System F terms for records and case statements ([#168](https://github.com/CategoricalData/hydra/issues/168)).
- Fixed issue with transitive type-level module dependencies ([#185](https://github.com/CategoricalData/hydra/issues/185)).
- Corrected handling of variable references in Python application terms.
- Fixed nullary functions in Python (added empty parens).
- Added missing alternatives in `hydra.rewriting.rewriteTermM`.

### Bookkeeping

- Closed long-standing Hydra-Java module-port issues
  ([#9](https://github.com/CategoricalData/hydra/issues/9), [#11](https://github.com/CategoricalData/hydra/issues/11), [#15](https://github.com/CategoricalData/hydra/issues/15), [#16](https://github.com/CategoricalData/hydra/issues/16), [#18](https://github.com/CategoricalData/hydra/issues/18), [#19](https://github.com/CategoricalData/hydra/issues/19), [#20](https://github.com/CategoricalData/hydra/issues/20), [#21](https://github.com/CategoricalData/hydra/issues/21), [#22](https://github.com/CategoricalData/hydra/issues/22), [#23](https://github.com/CategoricalData/hydra/issues/23), [#25](https://github.com/CategoricalData/hydra/issues/25), [#44](https://github.com/CategoricalData/hydra/issues/44), [#45](https://github.com/CategoricalData/hydra/issues/45));
  most of the underlying work shipped in earlier releases.
- Closed Hydra-Java DSL-port issues ([#24](https://github.com/CategoricalData/hydra/issues/24), [#26](https://github.com/CategoricalData/hydra/issues/26), [#27](https://github.com/CategoricalData/hydra/issues/27), [#28](https://github.com/CategoricalData/hydra/issues/28), [#29](https://github.com/CategoricalData/hydra/issues/29)) and
  the Scala/Avro tracking issues ([#77](https://github.com/CategoricalData/hydra/issues/77), [#81](https://github.com/CategoricalData/hydra/issues/81)); these had been resolved
  by earlier releases (Scala in 0.14.0, Avro bidi in 0.14.0).

### Documentation

- Updated Hydra-Haskell README.
- Updated Hydra-Java README with corrected links.
- Updated JavaDocs.

---

## [0.11.0] - 2025-03-16

Untagged in-repository version.
A type-inference rebase release: major restructuring of inference internals
to align with Algorithm W, plus a sweeping removal of explicit type annotations
across the kernel.

### Highlights

- **Inference rebased on Algorithm W** ([#118](https://github.com/CategoricalData/hydra/issues/118)): unify-early strategy ([#146](https://github.com/CategoricalData/hydra/issues/146)),
  infinite-type checks restored ([#116](https://github.com/CategoricalData/hydra/issues/116)), recursive-element inference ([#90](https://github.com/CategoricalData/hydra/issues/90)),
  unified handling of elements and let bindings ([#112](https://github.com/CategoricalData/hydra/issues/112)), minimized manual
  type annotations ([#119](https://github.com/CategoricalData/hydra/issues/119)).
- **`hydra.inference` module** introduced for inference type definitions;
  substitution and unification moved up a level.

### Breaking changes

- Argument order of `Logic.ifElse` changed ([#147](https://github.com/CategoricalData/hydra/issues/147)).

### New features

- Inference test cases promoted into the common test suite ([#148](https://github.com/CategoricalData/hydra/issues/148));
  added "kernel examples" inference tests; more recursion / mutual recursion cases.

### Improvements

- Removed type annotations across all kernel sources ([#119](https://github.com/CategoricalData/hydra/issues/119)).
- Migrated to fully-applied primitive library DSLs ([#157](https://github.com/CategoricalData/hydra/issues/157)).
- Aligned `Graph` and `Element` with `LetBinding` ([#159](https://github.com/CategoricalData/hydra/issues/159)).
- `hydra.core.Unit` recognized by the Haskell coder.
- Refactored inference tests.

### Bug fixes

- Fix for underdetermined type in `hydra.decode`.

---

## [0.10.0] - 2025-02-19

Untagged in-repository version.
Adds Hydra-Python (initial completion), a GraphSON coder,
provisional Hydra-Go support, and dot-separated namespace conventions.

### Highlights

- **Hydra-Python initial completion** ([#80](https://github.com/CategoricalData/hydra/issues/80), [#66](https://github.com/CategoricalData/hydra/issues/66)): Python coder, generated kernel
  modules in Python, and full library implementations across kernel namespaces.
- **GraphSON coder** ([#79](https://github.com/CategoricalData/hydra/issues/79)).
- **Provisional Hydra-Go** ([#65](https://github.com/CategoricalData/hydra/issues/65)).
- **Namespace formatting** standardized to dot-separated form ([#151](https://github.com/CategoricalData/hydra/issues/151)).

### Breaking changes

- All namespaces use dot-separated form ([#151](https://github.com/CategoricalData/hydra/issues/151)).

### New features

- Hydra-Python: Python coder; generated `hydra.coreEncoding`, `hydra.strip`,
  `hydra.literals` and other kernel modules in Python; tuple-expression support;
  updated `hydra.constants` in Python.
- GraphSON coder ([#79](https://github.com/CategoricalData/hydra/issues/79)).
- Provisional Hydra-Go ([#65](https://github.com/CategoricalData/hydra/issues/65)).
- `string-to-char-list` and `char-list-to-string` primitives ([#149](https://github.com/CategoricalData/hydra/issues/149)).
- Restored normalization for generated term variables ([#145](https://github.com/CategoricalData/hydra/issues/145)).

### Improvements

- Re-leveled dependency tiers ([#135](https://github.com/CategoricalData/hydra/issues/135)).
- Updated all Java, Haskell, hydra-ext, and Python primitives to current sources.

---

## [0.9.0] - 2025-01-06

Untagged in-repository version.
Bootstraps Hydra-Python and the JSON Schema coder; introduces a fluent Java DSL for flows.

### Highlights

- **JSON Schema coder** ([#141](https://github.com/CategoricalData/hydra/issues/141)).
- **Python coder scaffold**: serde, language constraints, and generated Haskell
  sources for Python.
- **Fluent-style Java DSL for flows** ([#143](https://github.com/CategoricalData/hydra/issues/143)).

### New features

- JSON Schema coder ([#141](https://github.com/CategoricalData/hydra/issues/141)).
- Python coder scaffold and serde.
- Provisional Python language constraints module.
- Fluent-style Java DSL for flows ([#143](https://github.com/CategoricalData/hydra/issues/143)); moved Flows DSL into `hydra.dsl`.
- New `hydra.coders` DSL.
- New DSL functions for core variants.

### Improvements

- Generalized `Namespaces` so it can be used for languages other than Haskell.
- Local-name-only Python type aliases (the directory tree provides the namespace).
- Coders DSL used to simplify Java language-constraint sources.
- Removed redundant productions in the Python grammar.
- Added unit tests for fluent flows.

### Documentation

- Updated JavaDocs for 0.9.0.

---

## [0.8.1] - 2024-09-24

Patch release.

### Improvements

- Generalized the property graph merging utility to accept type systems other than Hydra Core.

---

## [0.8.0] - 2024-09-09

C# support ([#139](https://github.com/CategoricalData/hydra/issues/139)), Graphviz DOT coder, term accessors, and a small breaking change in Java
to support Spark.

### Breaking changes

- Made `FlowException` serializable for Spark support (may affect existing error handling).

### New features

- C# syntax module based on the Microsoft ANTLR grammar ([#139](https://github.com/CategoricalData/hydra/issues/139)).
- Graphviz DOT coder ([#136](https://github.com/CategoricalData/hydra/issues/136)):
  full support for lambdas and recursive `let` statements;
  compact 'accessor graph' visualization option;
  customizable label styles;
  highlighted let-bound terms; edge labels.
- Term accessor type to facilitate lenses and path-aware transformations.
- OpenGQL grammar and generated Haskell for the OpenGQL model ([#140](https://github.com/CategoricalData/hydra/issues/140)).

### Improvements

- Added `</>` as alternative to `@@` application operator in Haskell DSLs.
- Utilities for working with term accessors.
- More specific exception class for JSON decoding.
- Additional convenience methods on `JsonDecoding.java`.
- Used transitive dependencies in hydra-java and hydra-ext.
- Upgraded ANTLR to address a vulnerability.
- Moved TinkerPop utilities from `hydra/ext/org/apache/tinkerpop` into `hydra/pg`.

---

## [0.7.0] - 2024-08-21

Major refactoring of module organization and namespace management.
Establishes the DNS-style namespace convention in hydra-ext
and lifts property-graph models into a top-level `hydra/pg` namespace.

### Breaking changes

- Namespace reorganization: migrated to DNS-based module naming in hydra-ext ([#138](https://github.com/CategoricalData/hydra/issues/138)):
  `hydra/ext/avro` → `hydra/ext/org/apache/avro`,
  `hydra/ext/graphql` → `hydra/ext/org/graphql`,
  `hydra/ext/yaml` → `hydra/ext/org/yaml`,
  `hydra/ext/json/decoding` → `hydra/ext/org/json/decoding`,
  `hydra/ext/rdf` → `hydra/ext/org/w3/rdf`,
  `hydra/ext/shacl` → `hydra/ext/org/w3/shacl`,
  `hydra/ext/tinkerpop` → `hydra/ext/org/apache/tinkerpop`.
- Property graphs: promoted property graph modules from
  `hydra/ext/org/apache/tinkerpop` to `hydra/pg`.
- Project structure: renamed `hydra-extensions` to `hydra-ext`;
  renamed `hydra/langs` to `hydra/ext` ([#138](https://github.com/CategoricalData/hydra/issues/138));
  made hydra-ext into a Gradle subproject alongside hydra-java;
  added top-level Gradle build.

### Improvements

- Moved TinkerPop modules from hydra-haskell into hydra-ext for better separation.
- Moved miscellaneous models to `Hydra.Ext.Other` ([#138](https://github.com/CategoricalData/hydra/issues/138)).
- Moved XML Schema, SQL, ShEx, and OWL models to hydra-ext.
- Moved KQL, Parquet, and Python modules to hydra-ext.
- Separated JavaDocs for hydra-java and hydra-ext.
- Updated publishing configuration in `build.gradle`.

---

## [0.6.0] - 2024-08-19

Type-system improvements (System F type abstraction/application; eliminated polymorphic
types from Hydra Core), expanded language support (Graphviz DOT model, generated Java
field-name constants), and revamped annotation handling.

### Breaking changes

- Eliminated `InferenceContext` helper type ([#103](https://github.com/CategoricalData/hydra/issues/103)).
- Changed typing environment from `Map Name Type` to `Map Name TypeScheme` in graphs ([#76](https://github.com/CategoricalData/hydra/issues/76)).
- Updated annotations and flows to use `map<Name, Term>` instead of `map<string, Term>` ([#133](https://github.com/CategoricalData/hydra/issues/133)).
- Unified `Name` and `FieldName`; aliased to `string` ([#121](https://github.com/CategoricalData/hydra/issues/121)).
- Renamed `hydra/core.UnitType` to `hydra/core.Unit` ([#123](https://github.com/CategoricalData/hydra/issues/123)).
- Eliminated annotation classes; Hydra has a single built-in notion of annotations ([#113](https://github.com/CategoricalData/hydra/issues/113)).
- Unified the `Nominal` type with `RowType` ([#115](https://github.com/CategoricalData/hydra/issues/115)).
- Removed the `extends` parameter for row types ([#132](https://github.com/CategoricalData/hydra/issues/132)).
- Replaced `hydra/Kv` with `hydra/Annotations` (rename revisited).

### New features

- System F support: `typeAbstraction` and `typeApplication` term constructors.
- Python3 syntax model based on the official Python BNF ([#80](https://github.com/CategoricalData/hydra/issues/80)).
- Graphviz DOT model and initial support ([#136](https://github.com/CategoricalData/hydra/issues/136)).
- Field-name constants generated in Java code generation ([#137](https://github.com/CategoricalData/hydra/issues/137)).
- Provisional Algorithm W implementation in Haskell (originally from @wisnesky), kept for reference.

### Improvements

- Primitive organization: Hydra primitives organized into libraries for namespace management.
- Code generation:
  - Escaped field name constants and `with_` methods in Java coder.
  - Generated field name constants in Java ([#137](https://github.com/CategoricalData/hydra/issues/137)).
  - Avoided duplicated comments in wrapper classes in generated Java.
  - Removed superfluous newtype comments in Haskell coder.
- Updated Delta Parquet model to follow the Java API more closely.
- Enriched OpenCypher features module with the complete list of standard Cypher functions.
- Added convenience methods to `JsonEncoding.java`.
- Restored `Hydra.Inference` subdirectory.
- Allowed arbitrary case for element names ([#7](https://github.com/CategoricalData/hydra/issues/7)).
- Corrected `uint8` representation in Java to `Short` ([#120](https://github.com/CategoricalData/hydra/issues/120)).

### Bug fixes

- Fixed handling of type annotations in term adapters.
- Fixed decoding of encoded terms annotated with a type.
- Corrected encoding of universal types in `showTerm` ([#117](https://github.com/CategoricalData/hydra/issues/117)).

---

## [0.5.3] - 2024-08-06

Patch release.
Preserves order of map key/value pairs and set elements during JSON serialization.

### Bug fixes

- Preserved ordering of key/value pairs and set elements in JSON output.

### New features

- Polymorphic `equal` primitive in Haskell and Java.
- Generated Hydra type definitions together with native instantiations of Hydra types
  in Haskell (a stepping stone toward generated coders, [#47](https://github.com/CategoricalData/hydra/issues/47)).

---

## [0.5.1] - 2024-08-01

Patch release.
Dependency updates and minor improvements following the 0.5.0 Hydra Core overhaul.

### Improvements

- Updated dependencies.

---

## [0.5.0] - 2024-07-26

Untagged in-repository version.
Major Hydra Core overhaul: eliminates polymorphic types in favor of explicit
TypeScheme; consolidates the annotation system; revamps `Let` term encoding.

### Breaking changes

- Eliminated polymorphic types from Hydra Core in Haskell ([#125](https://github.com/CategoricalData/hydra/issues/125)):
  removed type parameters from `hydra/core` types in Hydra-Java; replaced type schemes
  with forall types ([#76](https://github.com/CategoricalData/hydra/issues/76)).
- Revamped core encoding and `Let`: bindings are now a list of
  `(name, term, optional type scheme)` triplets instead of a map.
- Unified typed-term types and moved `TypeScheme` into Hydra Core.
- Eliminated `AnnotationClass`; Hydra has a single built-in notion of annotations now ([#113](https://github.com/CategoricalData/hydra/issues/113)).
- Renamed and refactored phantom-type wrappers ([#128](https://github.com/CategoricalData/hydra/issues/128)); removed deprecated `Reference`
  type from `hydra/phantoms`.
- Removed incomplete support for stream types and stream terms ([#88](https://github.com/CategoricalData/hydra/issues/88)).

### New features

- `LiteralTypes` DSL module so literals can be built independently of terms.
- `Hydra.Minimal` module to facilitate collaboration on Hydra application prototypes.
- `TypeConstraint` type added to the kernel.
- `foldl` primitive in Haskell and Java; DSL uses `foldl` instead of the built-in
  list elimination term by default.

### Improvements

- Improvements to Java and JSON coders; improved untyped JSON coder.
- Pre-order and post-order term-traversal test cases added.
- Refactored type inference tests.
- Restored generated Java (Core through Tier 3) with only a few manual tweaks.
- Element names allow arbitrary case ([#7](https://github.com/CategoricalData/hydra/issues/7)).

---

## [0.4.0] - 2024-07-10

Untagged in-repository version.
Adds null checks to generated Java (breaking, but justified — Hydra-Java disallows nulls),
the Gremlin model, and Protobuf annotation refinements.

### Breaking changes

- Null checks added to constructors and `withXXX()` methods in generated Java;
  null values are no longer permitted in Hydra-Java.
- Removed Java 8 restriction (raised minimum Java version).

### New features

- Gremlin model ([#127](https://github.com/CategoricalData/hydra/issues/127)); generated Haskell for Gremlin.
- `hydra/lib/lists.safeHead` primitive in Haskell (necessary in Java due to eager evaluation).
- Support for Protobuf `deprecated=true` annotation; non-string Protobuf options.
- Java options in generated Protobuf.

### Improvements

- Used `java.util.Objects.requireNonNull` instead of a custom null check in generated Java.
- Added 'automatically generated file' disclaimer to generated Java files.
- Added autogen comments and null checks to (most) generated Java.
- Added necessary schema-level dependencies for built-in modules.
- Added null checks to the Flows DSL in Java.

---

## [0.3.0] - 2024-06-05

Untagged in-repository version.
Adds an OpenCypher parser based on an ANTLR grammar ([#124](https://github.com/CategoricalData/hydra/issues/124)),
a KQL (Kusto Query Language) model and serializer,
a Delta Parquet model,
and an early property graph queries model in the style of Cypher.

### New features

- OpenCypher parser based on the OpenCypher M23 ANTLR grammar ([#124](https://github.com/CategoricalData/hydra/issues/124));
  ANTLR added as a build dependency;
  large Cypher test suite drawn from the Cypher Manual.
- Cypher-to-PG-query transformer (Java; partially implemented).
- Property graph queries DSL and generated property graph query classes in Java.
- Property graph queries model (Cypher / future GQL style).
- KQL (Kusto Query Language) initial model and basic serializer.
- Delta Parquet model with generated sources.

### Improvements

- Java options and `deprecated=true` support in generated Protobuf.
- Convenience methods in Java.

---

## [0.2.0] - 2024-01-10

Hydra-Java 0.2.0 release.
Significant Java tooling and DSL build-out, Protobuf bidirectional coder,
Python3 syntax model ([#80](https://github.com/CategoricalData/hydra/issues/80)), property graph validation ([#100](https://github.com/CategoricalData/hydra/issues/100)),
JSON bidirectional support in Java ([#104](https://github.com/CategoricalData/hydra/issues/104)),
OpenCypher initial model ([#108](https://github.com/CategoricalData/hydra/issues/108)),
Cypher unification, and the start of the typed-DSL story in Haskell.

### New features

- Property graph validation: validator type, optionality on property types,
  customizable id/property handling in the merging adapter, fine-tuned validation,
  comprehensive unit tests in Java ([#100](https://github.com/CategoricalData/hydra/issues/100)).
- Bidirectional JSON serialization/deserialization in Java ([#104](https://github.com/CategoricalData/hydra/issues/104)).
- Checkstyle configuration based on Google style guide ([#111](https://github.com/CategoricalData/hydra/issues/111)).
- Python3 syntax model based on the official Python BNF ([#80](https://github.com/CategoricalData/hydra/issues/80)).
- Initial OpenCypher model and parser scaffold ([#108](https://github.com/CategoricalData/hydra/issues/108)).
- Protobuf coder ([#99](https://github.com/CategoricalData/hydra/issues/99)): type encoder, field numbering, formatting, deprecated annotation,
  Java options, non-string options.
- Adapter framework tolerance of recursive types ([#58](https://github.com/CategoricalData/hydra/issues/58)); default branch for case
  statements ([#60](https://github.com/CategoricalData/hydra/issues/60)).
- Topological sort in type inference ([#67](https://github.com/CategoricalData/hydra/issues/67)).
- `hydra/query` model ([#73](https://github.com/CategoricalData/hydra/issues/73)).
- Records-to-TinkerPop-elements coder ([#64](https://github.com/CategoricalData/hydra/issues/64)).
- Equality primitives for all literal types ([#92](https://github.com/CategoricalData/hydra/issues/92)); polymorphic logic primitives ([#95](https://github.com/CategoricalData/hydra/issues/95)).
- `mapKeys` primitive ([#93](https://github.com/CategoricalData/hydra/issues/93)).
- Tuple support in Java ([#94](https://github.com/CategoricalData/hydra/issues/94)).
- Java 8 as an optional target ([#96](https://github.com/CategoricalData/hydra/issues/96)); optionally generate Java classes as `Serializable` ([#97](https://github.com/CategoricalData/hydra/issues/97)).
- Bidirectional "merged vertex" and "merged edge" coders ([#106](https://github.com/CategoricalData/hydra/issues/106)).
- Nullability and parameterization in the tabular model ([#107](https://github.com/CategoricalData/hydra/issues/107)).
- Term-level / type-level dependency distinction for modules ([#109](https://github.com/CategoricalData/hydra/issues/109)).
- Flatten nested `let` terms for transformation into Java ([#110](https://github.com/CategoricalData/hydra/issues/110)).
- Various `Flow` utilities in Java: `bind3`; convenience methods for consuming results;
  `fromFlow` variants including one that throws.

### Improvements

- Hydra-Java module ports ([#10](https://github.com/CategoricalData/hydra/issues/10), [#12](https://github.com/CategoricalData/hydra/issues/12), [#13](https://github.com/CategoricalData/hydra/issues/13), [#14](https://github.com/CategoricalData/hydra/issues/14), [#17](https://github.com/CategoricalData/hydra/issues/17), [#30](https://github.com/CategoricalData/hydra/issues/30), [#31](https://github.com/CategoricalData/hydra/issues/31), [#32](https://github.com/CategoricalData/hydra/issues/32), [#33](https://github.com/CategoricalData/hydra/issues/33), [#34](https://github.com/CategoricalData/hydra/issues/34), [#35](https://github.com/CategoricalData/hydra/issues/35),
  [#38](https://github.com/CategoricalData/hydra/issues/38), [#39](https://github.com/CategoricalData/hydra/issues/39), [#41](https://github.com/CategoricalData/hydra/issues/41), [#43](https://github.com/CategoricalData/hydra/issues/43), [#48](https://github.com/CategoricalData/hydra/issues/48), [#71](https://github.com/CategoricalData/hydra/issues/71), [#72](https://github.com/CategoricalData/hydra/issues/72), [#75](https://github.com/CategoricalData/hydra/issues/75), [#83](https://github.com/CategoricalData/hydra/issues/83)):
  `Common`, `CoreEncoding`, `CoreLanguage`, `Kernel`, `Flows`, `Lists`, `Literals`,
  `Math`, `Sets`, `Strings`, `Optionals`, `Maps`, `Flows` primitives, `Sets` primitives.
- Common DSLs in Haskell: Flow support ([#68](https://github.com/CategoricalData/hydra/issues/68)), Let support ([#69](https://github.com/CategoricalData/hydra/issues/69)).
- Renamed annotation type parameter `m` to `a` ([#41](https://github.com/CategoricalData/hydra/issues/41)).
- Updated wrapper, element, and variable types ([#75](https://github.com/CategoricalData/hydra/issues/75)).
- Removed `schema` field from `Element` ([#83](https://github.com/CategoricalData/hydra/issues/83)).
- GraphQL coder ([#78](https://github.com/CategoricalData/hydra/issues/78)).
- Code quality:
  - Updated main and test sources to conform to Checkstyle configuration ([#111](https://github.com/CategoricalData/hydra/issues/111)).
  - Extended line-length limit from 100 to 120 characters.
  - Disabled overly restrictive indentation and import-order rules.
  - Disabled `RightCurlyAlone` for compact inline map definitions.
- Streamlined property-graph element merging to unify properties with identical
  keys and types ([#106](https://github.com/CategoricalData/hydra/issues/106)).
- Convenience functions for printing literals and literal types in the Java DSL.
- Minor addition to JSON decoding.
- DSL convenience: alphabetic-case flexibility for element names ([#7](https://github.com/CategoricalData/hydra/issues/7)).

### Bug fixes

- Fixed type annotations for the Sets Java port ([#48](https://github.com/CategoricalData/hydra/issues/48)).
- Various minor fixes during the Java module ports.

---

## [0.1.1] - 2022-12-04

Patch release the same day as 0.1.0.
README polish only.

---

## [0.1.0] - 2022-12-04

First packaged release of Hydra (Hackage).
This release contains the complete foundation of Hydra.

### Core language

- Hydra's core type and data languages (`hydra.core`).
- Core models for graphs and modules.
- Computation model with the `Flow` monad.
- BNF grammars support.
- Phantom types.
- Basic operations on types and terms.

### Type system

- Hindley–Milner-style type inference.
- Type schemes with polymorphism.
- Type/term validation.

### Transformations

- Adapter system: type-to-type rewriting and transformation.
- Coders:
  Haskell coder (types and terms),
  Java coder (types and terms),
  partial Scala coder (terms only),
  Avro coder,
  JSON coder,
  PDL (Pegasus Data Language) coder,
  RDF + SHACL coder,
  YAML coder.

### Language support

- Models for GraphQL, OWL (Web Ontology Language), ShEx (Shape Expressions),
  and TinkerPop-style property graphs.

### Developer tools

- Type-construction DSL and term-construction DSL.
- QuickCheck property-based tests.

### Implementations

- Hydra-Haskell: bootstrapping implementation with full kernel.
- Hydra-Java: Java implementation with mature tooling.

### Community

- Set up the LambdaGraph Discord server ([#2](https://github.com/CategoricalData/hydra/issues/2)).

---

## Contributing

We welcome contributions! Please see:

- [Developer recipes](https://github.com/CategoricalData/hydra/tree/main/docs/recipes)
- [LambdaGraph Discord](https://bit.ly/lg-discord)

## Release process

See the [Release process](https://github.com/CategoricalData/hydra/wiki/Release-process)
wiki page for information on how Hydra releases are created.
