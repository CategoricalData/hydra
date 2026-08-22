<!-- NOTE: hand-authored spec chapter (not a generated module page). Drafted under #676 as the
     policy companion to #595 (the hydra.diff engine that mechanically applies these rules) and
     #675 (the effect-sequencing contract, a behavioral instance). -->

# Backward compatibility and deprecation

**Status: Draft** · Applies from: Hydra 1.0

Hydra 1.0 freezes its consumed surfaces and promises API stability across the 1.x series.
This chapter states, normatively, **what "compatible" means after 1.0**: which changes a 1.x
release MAY make, which changes are 2.0 events, and how deprecations and renames are expressed so
that a frozen surface can still evolve.
It is the *policy*; the schema-diff engine that mechanically classifies and enforces it is
specified and implemented separately ([#595](https://github.com/CategoricalData/hydra/issues/595)).

The key words MUST, MUST NOT, SHOULD, and MAY are to be interpreted as described in RFC 2119.

## 1. The two-axis model

Every consumed surface is frozen on one of two axes.

- **Structural** — the shape of data a consumer's generated code depends on: the JSON wire format,
  the `hydra.core` and `hydra.packaging` types, the `hydra.error.*` types, and primitive
  signatures. Structural compatibility is governed by the **additivity calculus** (§2) and is
  mechanically checkable from the schema alone.
- **Behavioral** — meaning that is not visible in a signature: the semantics of a primitive at an
  unchanged signature, type-inference results, the host-authoring DSLs, and the `hydra.build.*`
  build orchestration. Behavioral compatibility is governed by golden/characterization and
  conformance suites, tracked separately
  ([#420](https://github.com/CategoricalData/hydra/issues/420),
  [#543](https://github.com/CategoricalData/hydra/issues/543),
  [#675](https://github.com/CategoricalData/hydra/issues/675), and the DSL freeze).

This chapter specifies the **structural** policy and the **deprecation mechanism**, which spans
both axes. It does not restate the behavioral suites; it states where the boundary lies (§2.3).

## 2. The additivity calculus

A change between two consecutive 1.x releases is classified, per delta, as either **compatible**
(C-class, allowed within 1.x) or an **incompatible 2.0 event** (X-class, which the 1.x release
pipeline MUST refuse). The classification is mechanical and follows only from the type-level and
signature-level shape of the change.

### 2.1 Compatible changes (C-class)

A 1.x release MAY:

- **Add an optional record field.** Existing consumers that do not read it are unaffected.
- **Add a new type or module.** Nothing existing references it.
- **Widen a literal or numeric type** where the carrier tolerates the widening losslessly
  (e.g. a smaller integer type to a larger one of the same signedness). This is C-class **only
  when it does not change the wire-format literal-type variant a consumer matches on**; a widening
  that changes that variant is the variant-add case of §2.2 (X-class). See the literal-type rules in
  [json-format.md](json-format.md).
- **Add a new primitive.**
- **Deprecate — never remove — a primitive, term, field, type, or module** (§3). Any surface may be
  deprecated; only primitive and term renames additionally get the forwarding-term alias of §3.2.

Each C-class change requires its enabling mechanism to exist before it may be used; the
deprecation and rename mechanisms are specified in §3.

### 2.2 Incompatible changes (X-class — a 2.0 event)

A 1.x release MUST NOT:

- **Add a variant to a union.** This is the bootstrap-breaking class: a new variant breaks every
  exhaustive match over that union across all hosts' generated code.
- **Remove or rename a field, or rename a type.** These are X-class with **no aliasing escape**: the
  forwarding-term mechanism of §3.2 aliases a *primitive or term* name, and cannot forward a field or
  a type. A field or type that must change name within 1.x takes the additive path instead — add the
  new optional field / new type and deprecate the old one (§3.1) — which is a deprecation plus an
  addition, not a rename. (A primitive or term rename *does* have the §3.2 escape and is therefore not
  X-class; see the next bullet.)
- **Make an optional field required.**
- **Narrow a type** (the inverse of the widening allowed in §2.1).
- **Remove or re-semanticize a primitive.** Removal is an X-class change; a *rename* goes through
  §3.2; a *semantic change at an unchanged signature* is a behavioral break (§2.3), not visible to
  the structural calculus, and is likewise disallowed within 1.x.

### 2.3 Two notes on the boundary of the calculus

**The calculus is stricter than the wire-format bump rule, deliberately.**
[json-format.md](json-format.md) classifies a variant-add as a non-bump because a reader
encountering an unknown variant fails loudly rather than mis-parsing; that rule tracks *parser
correctness*. This calculus tracks *consumer survival* — a downstream exhaustive match over the
union. Both rules are correct at their own layer. The layering is stated so that neither is
weakened to reconcile them: a variant-add is a non-bump for the wire format **and** an X-class
event for consumer compatibility, simultaneously.

**A stable signature is not a sufficient compatibility check.**
Some breaks are invisible to a schema diff: a primitive whose *meaning* changes while its
signature does not (for example the `math.range` inclusivity flip,
[#647](https://github.com/CategoricalData/hydra/issues/647), or a change to `compareTo` ordering,
[#612](https://github.com/CategoricalData/hydra/issues/612)). These are governed by the behavioral
axis (golden and conformance suites), not by this structural calculus. A green schema-diff report
therefore certifies only the structural axis; it is **not** a full green. No release may treat a
clean structural diff as evidence of behavioral compatibility.

## 3. Deprecation and rename

### 3.1 Deprecation

A primitive, field, type, or module is deprecated by stamping
`LifecycleInfo.deprecatedSince = <version>` in its entity metadata, paired with a `Use:` pointer to
its replacement in the entity's documentation. A deprecated surface:

- MUST remain available for the remainder of 1.x — deprecation never removes anything in a 1.x
  release;
- SHOULD NOT be depended upon by new code;
- MAY be removed only at the next major (2.0) event.

Additions are stamped symmetrically with `LifecycleInfo.availableSince = <version>`, recording the
release in which the surface was introduced.

Deprecation applies to every surface, but only **primitive and term** surfaces have the
forwarding-term rename path of §3.2. A field, type, or module that must change name deprecates the
old surface and adds a new one (per §2.1's additive rules); there is no forwarding for non-term
surfaces.

`LifecycleInfo` is defined in `hydra.packaging` and attaches through `EntityMetadata.lifecycle`,
which every `PrimitiveDefinition`, `TypeDefinition`, and `TermDefinition` carries. The status
badges on the per-primitive specification pages (`Deprecated since: <v>. Use: <target>.`) are the
prose face of this data; the `deprecatedSince` stamp is its machine-checkable form.

### 3.2 Pure rename via a forwarding term

A pure rename `old → new` — same signature, same semantics, different name — is expressed as a
**forwarding term definition** that references the new name:

```
old = new
```

The forwarding term lives in a **mixed module** (a module carrying term definitions alongside, or
instead of, primitive or type definitions). This is Hydra-native by construction: the forward
resolves through ordinary reference and expansion, so it is translingual and requires no coder
special-casing and no per-host shim. The old name continues to typecheck and evaluate for every
consumer, on every host, with no bespoke compatibility code.

**Prerequisite.** Mixed-module support does not exist yet — modules are homogeneous by kind today.
This rename mechanism is gated on that support, tracked as its own dependency.

### 3.3 Aliasing is valid iff the change is a pure rename

A forwarding term is a valid alias **only** when the change is a pure rename. If the signature or
semantics change, the result is **not the same primitive**: there is nothing to alias. Such a
change is a *new* primitive plus a *deprecation* of the old one (§3.1), not a rename.

This rule is self-policing. A forwarding term `old = new` written across a signature change would
not typecheck — the old name's declared signature and the new definition's inferred type would
disagree — so the type system refuses "a hard break wearing an alias costume" by construction. The
alias **is** the `Use:` pointer, expressed mechanically rather than in prose: `deprecatedSince`
marks the old name, and the forwarding term is its machine-checkable replacement.

## 4. Enforcement

The additivity calculus (§2) is the **classification schema** for the schema-diff engine
([#595](https://github.com/CategoricalData/hydra/issues/595)). Every type-level delta between two
consecutive release tags classifies mechanically as C-class or X-class by the calculus; primitive
signatures ride the same engine, since a signature is data in `PrimitiveDefinition.signature`.

- The classifier runs over consecutive release tags as part of the **release workflow** (beside
  `prepare-release.sh`).
- A report containing **any** X-class delta means the candidate release is a 2.0. The 1.x release
  pipeline MUST refuse it.
- This is the release-gating schema-drift reporter that #595 lists as an explicit non-goal of its
  engine: the *engine* is #595's; the *gate* is this policy's requirement, built on that engine.

This is the "enforce the policy with Hydra itself" move — Hydra reads its own schema diff to gate
its own releases. Because the structural diff is not a behavioral check (§2.3), the gate is
necessary but not sufficient: a release is compatible only when the structural gate is green **and**
the behavioral suites are green.

## 5. Overlay and error-model scope

### 5.1 Overlays

Overlay surfaces decompose into three tiers (the per-tier mechanisms are follow-up work; this
chapter states the tiering):

- **Tier 1 — primitive implementations.** Derived, not independently promised: correct exactly
  when the conformance suite passes. Their stability falls out of the conformance work
  ([#420](https://github.com/CategoricalData/hydra/issues/420)).
- **Tier 2 — host-native surfaces.** Governed by a declared public/internal manifest expressed as
  data on `hydra.build.Registry`'s `LanguageProfile` (the names-in-data pattern of
  [#416](https://github.com/CategoricalData/hydra/issues/416)). Only the declared-public surface is
  frozen.
- **Tier 3 — third-party integrations.** Track their upstream libraries; a documented carve-out,
  not frozen by this policy.

### 5.2 Error types

The `hydra.error.*` types are structural surface and are governed by the additivity calculus (§2).
In particular, **adding an error variant is an X-class change**, because downstream code
exhaustively matches error unions. Error **text** — the human-readable message strings — is
explicitly unpromised and MAY change in any release.

## 6. Relationship to other specifications and to the release policy

- **[#595](https://github.com/CategoricalData/hydra/issues/595)** implements the schema-diff
  *mechanism* (`TypeDiff`, migrators, fusion); this chapter defines the *policy* it enforces. §2 is
  precisely #595's classification table; §4 is the release-gate #595 lists as a non-goal.
- **[#675](https://github.com/CategoricalData/hydra/issues/675)** ([effects](primitives/effects.md))
  is a behavioral-axis instance: the effect-sequencing contract that a stable signature cannot
  express, exactly the kind of guarantee §2.3 assigns to the behavioral suites.
- The [Release policy](https://github.com/CategoricalData/hydra/wiki/Release-policy) wiki page
  states the release *process* and references this chapter for the 1.x compatibility rules.
