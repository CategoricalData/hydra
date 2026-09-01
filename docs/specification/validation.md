<!-- NOTE: hand-authored spec chapter (not a generated module page). First draft under #579; the
     §4 rule catalog is derived from Validate/{Core,Packaging}.hs and will be reconciled against the
     live checks by a future generator run. The value-constraint scope (§5) will be adjusted on the
     basis of #712 (with #345). Related: #721 (no-prefix rule), #722 (dead rule ids / isValidName). -->

# Validation

**Status: Draft** · Part of the [Hydra specification](index.md)

Validation is Hydra's mechanism for checking that a value **conforms** to a type. It is a
representation-generic framework: the same machinery validates core terms against core types,
property-graph data against property-graph types, and — as Hydra grows target representations — the
data of any target against that target's types.

## 0. Conformance and its preservation

A value **conforms** to a type — written `a : t` — when it satisfies every rule that makes it a
well-formed inhabitant of that type: in the core representation, every variable is bound, every
nominal reference resolves to a type of the right kind, every record carries exactly its declared
fields, every case covers its union, and so on.

Conformance is not confined to `hydra.core`. Hydra maps paired representations of types and instances
across languages and models in a **semantics-preserving** way, and conformance is a relation that
exists in *each* representation:

- **Core**: core terms against core types.
- **Property graphs**: property-graph data (vertices, edges, properties) against property-graph types
  (vertex types, edge types).
- **Host languages and interchange formats** (Java classes and objects, RDF, JSON, Avro, …): each
  carries its own type-side and instance-side representation, hence its own conformance relation.

Hydra's central correctness law is that a transformation `F` from one representation to another
**preserves conformance**:

> **if `a : t` then `F(a) : F(t)`**

That is, a coder maps a type `t` and an instance `a` such that the mapped instance conforms to the
mapped type *under the target's own conformance relation*. "Semantics-preserving" means exactly this.

**Validation is the checkable specification of conformance, and therefore the instrument for verifying
preservation.** Because conformance is decidable in each representation, a coder `F` can be checked by
validating `a : t` on the source side and `F(a) : F(t)` on the target side; a transformation that
takes a conformant value to a non-conformant one has a bug, and validation is how it is caught — in
the kernel and in the hydra-kernel test suite.

## 1. The three roles of validation

Validation serves three purposes, all built on the same framework:

1. **Define conformance.** It is the decision procedure for `a : t` within a representation — the
   well-formedness rules that go beyond what type inference tracks (§4).
2. **Verify cross-representation preservation.** Validating both sides of a coder checks the law
   `if a:t then F(a):F(t)` (§0). Each representation supplies its own conformance rules; validating
   before and after a transformation is how preservation is established.
3. **Enforce constraints the type system does not express.** An optional, rule-based pass for
   declared constraints — length, cardinality, value range, and other refinements — that Hydra's
   decidable type system deliberately does not encode (§5). This is distinct from inference-enforced
   structural typing and the constraint classes ([classes.md](classes.md)).

## 2. The representation-generic framework

The framework lives in `hydra.validation` and is instantiated per representation:

- `hydra.validate.core` — conformance rules for the **core** representation (this page's §4 catalog).
- `hydra.validate.pg` — conformance rules for the **property-graph** representation: `validateVertex`,
  `validateEdge`, `validateProperties`, `validateGraph` check property-graph data against
  property-graph types, with their own profile and error types.
- Future `hydra.validate.<target>` — one instance per target representation.

Each instance is a set of *checks* selected and classified by a **validation profile**
(`hydra.validation.ValidationProfile`):

| Field | Meaning |
|---|---|
| `errorRules` | The set of fully-qualified rule names whose findings are **errors**. A rule name has the form `hydra.error.<package>.<UnionType>.<variant>`, e.g. `hydra.error.core.InvalidTermError.duplicateBinding`. |
| `warningRules` | The set of rule names whose findings are **warnings** (informational; do not fail the pass). |
| `maxErrors` | Hard bound: the pass terminates once this many errors are collected. `1` reproduces "first error wins". |
| `maxWarnings` | Soft bound: further warnings are dropped once reached, but the pass continues. |

A rule whose name is in **neither** set is never evaluated — this is how a project opts out of a
rule. A pass produces a `hydra.validation.ValidationResult` (ordered error and warning findings); it
**succeeds** iff the error list is empty. Hydra ships strict reference profiles
(`kernelDefaultCoreProfile`, `kernelDefaultPackagingProfile`, `defaultPgProfile`) that enable the
full rule set for the corresponding representation.

## 3. The two classes of rule

Within any representation, rules fall into two classes:

- **MUST-class.** Violating the rule breaks conformance: the value is not a well-formed inhabitant of
  its type, so no transformation could legitimately have produced it and no conforming implementation
  can process it. A conforming implementation **MUST NOT** accept a value that violates a MUST-class
  rule. Normative for *every* Hydra application.
- **SHOULD-class.** The rule expresses a consistency or quality convention that Hydra enforces
  strictly on its own kernel and codebase but which does not bear on conformance — violating it does
  not break anything. External code **SHOULD NOT** violate a SHOULD-class rule, but a downstream
  project MAY disable it (omit its name from the profile). The kernel default profiles enforce every
  SHOULD-class rule as an error; that strict profile is one selectable configuration, not a universal
  law.

This is the specification's own view: some rules are normative for any Hydra application (they *are*
conformance); others are normative only for Hydra itself and opt-in elsewhere.

## 4. Core-representation rules (`hydra.validate.core`)

The conformance rules for the core representation. MUST-class rules define conformance; SHOULD-class
rules are the kernel's house style.

### 4.1 MUST-class (define conformance)

**Terms.** undefinedTermVariable (every variable resolves to a binding, lambda parameter, or
primitive) · unresolvedNominalType · nominalTypeKindMismatch (resolved type is record/union/wrapper as
the site requires) · missing/extraRecordFields (record fields exactly match the declared type) ·
undeclaredVariant · unknownProjectedField · unknownCaseAlternative · missingCaseBranches (a defaultless
case covers every variant) · duplicateBinding · duplicate record/case fields · literalTypeMismatch ·
emptyLetBindings · emptyCaseStatement · emptyTypeNameInTerm.

**Types.** emptyUnionType (an empty union is uninhabited) · duplicate record fields / union variants ·
undefinedTypeVariable (bound by an enclosing `∀`/`forall`) · voidInNonBottomPosition (`void` only in
bottom position; see [syntax.md](syntax.md)) · nonComparableMapKeyType / nonComparableSetElementType
(key/element types are comparable — see [ordering-and-equality.md](ordering-and-equality.md)).

**Packages and modules.** duplicate module names · duplicate definition names · conflictingModuleNames
(no lowercased collision) · conflictingVariantNames · definition-name prefixing (every definition name
has its module namespace as a dotted prefix) · undeclaredDependencies (one hop, not transitive).

### 4.2 SHOULD-class (kernel-strict, opt-in elsewhere)

module no-prefix (no module namespace is a strict dotted-prefix of another; unenforced today —
[#721](https://github.com/CategoricalData/hydra/issues/721)) · definition alphabetical ordering ·
definition documentation required · naming conventions (camelCase / PascalCase / dotted-lowercase /
hyphenated; regexes in `hydra.constants`; see
[#722](https://github.com/CategoricalData/hydra/issues/722)) · the redundancy/suspicion lints
(constantCondition, redundantWrapUnwrap, selfApplication, unnecessaryIdentityApplication — all
well-typed) · variable shadowing · annotation nesting (**nested annotations are supported** — see
[syntax.md](syntax.md); avoiding them is a convention, not an error) · degenerate-type warnings
(emptyRecordType, singleVariantUnion).

## 5. Constraints beyond the type system (declared today, enforcement under investigation)

Hydra's type system enforces structural typing and the constraint classes; validation (§4) enforces
well-formedness. A third category — **value constraints** the type system cannot express (length,
cardinality, numeric range, patterns, refinements) — is validation's natural home (role 3, §1), and
is where Hydra approaches, without adopting, dependent types.

**Current state.** The intent for some of these constraints is already *declarable* via type
annotations: the authoring DSL provides `nonemptyList`, `minLengthList n`, `twoOrMoreList`,
`boundedList min max`, `nonemptyMap`, `boundedMap min max`, and `setMinLength` / `setMaxLength`, which
attach `minLength` / `maxLength` annotations to list, set, and map types. **These annotations are not
yet enforced** — no validation check reads them — so they express intent only, which is why the
helpers are rarely used.

**Scope.** Which value-constraint varieties Hydra should support formally for 1.0 — and, per variety,
whether they belong to the type system (the dependent-types route,
[#345](https://github.com/CategoricalData/hydra/issues/345)) or to this validation layer as data —
is under investigation in [#712](https://github.com/CategoricalData/hydra/issues/712). This section
will be made normative once that scope is decided; enforcing the already-declared length/cardinality
annotations through a validation check is the concrete first step in that arc.

## 6. Open items

- `unknownPrimitiveName` and `untypedTermVariable` appear in the kernel default profile but are not
  yet wired to a check; term-layer name validation is narrower than the naming-convention regexes
  imply. Both are tracked in [#722](https://github.com/CategoricalData/hydra/issues/722) and will be
  reconciled before this page is considered Canonical.

