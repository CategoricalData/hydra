# Update Java JSON (originally issue #344 "self-host demo")

User-facing entry point: `bin/generate-hydra-java-from-java.sh`.

Thin wrapper: `bin/update-java-json.sh`.

Driver class: `heads/java/src/main/java/hydra/UpdateJavaJson.java`.

## What this is

The Java analogue of `bin/update-python-json.py`. Updates
`dist/json/hydra-java/` from Java-language DSL sources under
`packages/hydra-java/src/main/java/hydra/sources/java/`, instead of the
legacy Haskell-driven pipeline (which generated from
`packages/hydra-java/src/main/haskell/Hydra/Sources/Java/`). Originally
introduced for issue #344 as a "self-host demo"; now the canonical
Java DSL → JSON step in the regular sync pipeline (Phase 5).

The workflow:

```
1. Load the kernel universe from dist/json/hydra-kernel/.
2. Reflectively discover hydra.sources.java.{Coder,Environment,...,Utils}
   classes and pull their static `module_ : Module` fields.
3. Run Codegen.inferModulesGiven over (universe ∪ sources).
4. Build a graph + schema_map.
5. Encode each module to JSON via Codegen.moduleToJson.
6. Write each output to dist/json/hydra-java/src/main/json/hydra/java/<m>.json.
```

## Quick start

```bash
# Build the Java host if needed, then generate hydra-java JSON from Java DSL sources:
bin/generate-hydra-java-from-java.sh --out-root /tmp/hj-from-java

# Also byte-compare against the Haskell-generated canonical:
bin/generate-hydra-java-from-java.sh --out-root /tmp/hj-from-java --compare

# Force a full Java host rebuild before running:
bin/generate-hydra-java-from-java.sh --force-rebuild
```

## Current state (2026-05-11)

Infrastructure complete; **5 of 8 modules byte-identical** (Environment,
Language, Names, Syntax, Testing). The remaining 3 are stubs: Coder
(5453 LoC), Serde (1800 LoC), Utils (1289 LoC). See the byte-comparison
table from a recent run:

```
          module  status            ours       canon     delta  diff lines
---------------------------------------------------------------------------
           coder  DIFFER             173     4346816  -4346643       40597
     environment  BYTE-EQ          10194       10194        +0           0
        language  BYTE-EQ          27981       27981        +0           0
           names  BYTE-EQ           8554        8554        +0           0
           serde  DIFFER             173      524899   -524726        8734
          syntax  BYTE-EQ         112184      112184        +0           0
         testing  BYTE-EQ          62424       62424        +0           0
           utils  DIFFER             173      470503   -470330        7052
---------------------------------------------------------------------------
Summary: 5 / 8 byte-identical
```

## Why not 8/8

Status by module (Haskell LoC → Java port state):

```
  Coder.hs        5453   STUB    (huge; deep term DSL + JavaDsl helpers)
  Serde.hs        1800   STUB    (term DSL + JavaDsl helpers)
  Utils.hs        1289   STUB    (135 term defs, heavy JavaDsl helper use)
  Syntax.hs       2558   ✓ BYTE-EQ  (275 type defs; script-generated)
  Language.hs      225   ✓ BYTE-EQ
  Testing.hs       271   ✓ BYTE-EQ
  Names.hs         200   ✓ BYTE-EQ
  Environment.hs   144   ✓ BYTE-EQ
  Manifest.hs       34   N/A     (build-tooling glue)
```

The 3 remaining modules (Coder, Serde, Utils) total ~8500 lines of
Haskell DSL. Each relies heavily on the per-package DSL helper module
(equivalent of Haskell's `JavaDsl.*`). **As of this session, those
DSL helpers now exist on the Java side too**: see
`dist/java/hydra-java/src/main/java/hydra/dsl/java/Syntax.java`
(~1200 methods covering all hydra-java syntax types). Each one mirrors
the Haskell helper and produces the same encoded AST.

The hand-written augmentation layer
`packages/hydra-java/src/main/haskell/Hydra/Dsl/Java/Helpers.hs` (433
lines, 79 smart constructors) has also been ported to
`packages/hydra-java/src/main/java/hydra/sources/java/JavaHelpers.java`.
Together, Syntax and JavaHelpers cover every `JavaDsl.*` call site
used by the remaining Haskell source modules.

The enabling change was adding `hydraJavaModules` to `dslInputMods`
in three places that previously only included `hydraPythonModules`:

- `heads/haskell/src/exec/update-json-main/Main.hs`
- `heads/haskell/src/exec/update-json-manifest/Main.hs`
- `heads/haskell/src/exec/transform-haskell-dsl-to-json/Main.hs`
  (`packageDslInputModules` clause for `"hydra-java"`)

With these, the kernel codegen now produces:
- `dist/json/hydra-java/src/main/json/hydra/dsl/java/{syntax,environment}.json`
- `dist/java/hydra-java/src/main/java/hydra/dsl/java/{Syntax,Environment}.java`
- `dist/haskell/hydra-java/src/main/haskell/Hydra/Dsl/Java/{Syntax,Environment}.hs`

Use them from Java DSL source code like the Haskell side uses
`JavaDsl.literalInteger`: e.g. `JavaSyntax.literalInteger(intLit)`
returns a `TTerm<Literal>` that encodes to the same `{"inject": ...}`
AST node.

For Syntax (type defs only), a Python translator script (see
`/tmp/port_syntax.py` in this branch's history) handled 275 type defs
mechanically. A more elaborate translator that recognises
`JavaDsl.xxx` references and emits `JavaSyntax.xxx` calls would
similarly mechanise the term-level port.

## Files

- `bin/generate-hydra-java-from-java.sh` — user-callable wrapper
- `bin/update-java-json.sh` — thin wrapper (compile + run driver)
- `bin/update-java-json.md` — this README
- `heads/java/src/main/java/hydra/UpdateJavaJson.java` — driver class
- `packages/hydra-java/src/main/java/hydra/sources/java/SourceDsl.java` — small
  DSL helpers (`doc`, `typeref`, `typeDef`, `termDef`, `docTerm`)
- `packages/hydra-java/src/main/java/hydra/sources/java/{Coder,Environment,
  Language,Manifest,Names,Serde,Syntax,Testing,Utils}.java` — per-module
  Java DSL sources (Environment is byte-equivalent; others are stubs)

## How to port a module

Pattern that worked for Environment.java (modeled on the Python equivalent):

```java
public class Environment {
    public static final Namespace NS = new Namespace("hydra.java.environment");

    private static hydra.core.Type env(String local)    { return typeref(NS, local); }
    private static hydra.core.Type syntax(String local) { return typeref(SYNTAX_NS, local); }
    private static hydra.core.Type core(String local)   { return typeref(CORE_NS, local); }
    // ...

    private static Definition javaSymbolClass() {
        return typeDef(NS, "JavaSymbolClass",
            doc("Classification of a Java symbol for code generation",
                Types.union(
                    Types.field("constant", doc("A constant value", Types.unit())),
                    // ...
                )));
    }

    // List the per-def methods in the same order as the Haskell `definitions = [...]`:
    private static final List<Definition> DEFINITIONS = Arrays.asList(
        javaSymbolClass(),
        javaFeatures(),
        // ...
    );

    public static final Module module_ = new Module(
        Maybe.just("Environment types for Java code generation"),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
```

Key things to get right for byte-equivalence (lessons from the Python port —
all 6 categories will likely apply to Java):

1. **DSL helpers**: Build out helpers in `SourceDsl.java` (or a per-source-class
   inner) when a missing combinator surfaces. The current minimum: `doc`,
   `typeref`, `typeDef`, `termDef`, `docTerm`.
2. **Bigfloat removal (#330)**: If any Haskell `Type_FloatType_bigfloat`
   reference survives, replace with `Float64`. The Haskell sources have
   already been swept; the port should follow.
3. **Module dependencies**: Names.hs deps include `JavaSyntax.ns :
   kernelTypesNamespaces`. Make sure `hydra.validation` is in the kernel
   types list.
4. **let-chain encoding**: Haskell `<~` chains produce nested singleton lets;
   `lets [...]` produces flat. Don't conflate them.
5. **Optimization-tracking fixes**: extendMetaForType exponential recursion
   (#b7d29d9b4), unconditional makeLazy wrap, inlineVars in
   encodeApplicationInner/encodeVariable, encodeTermAssignment topLevel arg,
   ConsList.of / PersistentMap.of_entries / PersistentSet.of for
   collection term encoding, Sequence/Mapping/Set names, extendMetaForTerm
   list/set arms, extendMetaForType set arm, moduleStandardImports rewrite.
   See feature_344_self_hosting_coders-plan.md.
6. **Definition order**: must match the Haskell `definitions = [...]` list
   exactly, not alphabetical.

Iterate with `--compare` until each module reads BYTE-EQ.

## Key encoding gotchas (lessons from Environment + Language ports)

These caused inference failures or byte-mismatch and took time to
diagnose. Future ports will hit the same patterns.

1. **DSL constants expand inline, not as variable references**. The
   Haskell `Variants.eliminationVariantRecord` looks like an external
   binding, but it's a generated DSL constant defined as
   `inject(EliminationVariant, record, unit())`. The canonical JSON
   shows the inlined form. So Java code should use
   `Terms.injectUnit("hydra.variants.EliminationVariant", "record")`,
   not `Terms.variable("hydra.dsl.variants.eliminationVariantRecord")`.
   The latter fails inference with "no such binding".

   Same applies to `Coders.language(...)`, `Coders.languageName2(...)`,
   `Coders.languageConstraints2(...)` — these are record/wrap
   constructors that expand inline, not callable functions.

2. **Primitive function references need `tyapp` for monomorphism**. The
   canonical JSON encodes `Sets.fromList xs` as
   `apply(tyapp(var("hydra.lib.sets.fromList"), <elementType>), xs)`,
   NOT as `apply(var("hydra.lib.sets.fromList"), xs)`. Same for
   `Lists.concat`, etc. Without the `tyapp`, inference might still
   succeed but the encoding differs.

3. **Record-field names are camelCase, not snake_case**. The Haskell
   `_EliminationVariant_record` field name encodes as `"record"` in
   JSON. So pass `"record"` (lowercase) to `injectUnit`, not `"Record"`.

4. **Empty type-scheme metadata**: pass
   `Maybe.<Map<Name, TypeVariableMetadata>>nothing()` for the third
   arg to `new TypeScheme(variables, body, metadata)`. The metadata
   field is a `Maybe<Map<Name, TypeVariableMetadata>>`, NOT a
   `Maybe<PersistentMap<Name, Term>>` — easy to get wrong.

5. **`Generation.loadModulesFromJson` needs a schemaMap argument**.
   Pre-build via `Generation.bootstrapSchemaMap()` and pass it; don't
   try to call the (non-existent) two-arg version.

6. **Definition order matters**. The `definitions` list ordering must
   exactly match the Haskell `definitions = [...]` ordering. Sort by
   the explicit Haskell list, NOT alphabetical.
