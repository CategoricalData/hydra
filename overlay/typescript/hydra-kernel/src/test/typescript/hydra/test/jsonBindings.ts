// Load kernel term bindings from dist/json/hydra-kernel/ into a map
// suitable for testGraph.boundTerms.
//
// Mirrors Java's `Generation.loadModulesFromJson()` and Python's
// `_load_kernel_term_bindings`. The kernel JSON uses Aeson's adjacently
// tagged encoding (e.g. `{typeLambda: {...}}` for `Term_typeLambda`)
// while the TypeScript runtime expects internally-tagged objects
// (`{tag: "typeLambda", value: {...}}`). This module performs that
// conversion.
//
// Only the evaluator-essential namespaces are loaded by default — the
// same minimal set Java and Python load. Other namespaces can be added
// via `loadNamespaces`.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
// Name lives in both runtime and core; Term/TypeScheme are kernel-generated.
import type { Name, Term, TypeScheme } from "../../../../main/typescript/hydra/core.js";
import * as libMaps from "../../../../main/typescript/hydra/overlay/typescript/lib/maps.js";
import * as libSets from "../../../../main/typescript/hydra/overlay/typescript/lib/sets.js";

// ESM equivalent of CommonJS __dirname.
const HERE = dirname(fileURLToPath(import.meta.url));

// The arm names of Hydra's union types (Term, Type, Literal, IntegerValue,
// FloatValue, IntegerType, FloatType, etc.). Each name, when it appears as
// the sole key of an object in the kernel JSON, signals a discriminated-
// union value and is rewritten to `{tag: <name>, value: <converted-rhs>}`.
//
// Built by extracting every arm name from the kernel's union types.
// Keep alphabetized for diff readability.
const UNION_ARMS = new Set<string>([
  // Term arms
  "annotated", "application", "cases", "either", "inject", "lambda",
  "let", "list", "literal", "map", "optional", "pair", "primitive",
  "product", "project", "record", "set", "stream", "sum", "typeAbstraction",
  "typeApplication", "typeLambda", "tyabs", "tyapp", "unit", "unwrap",
  "variable", "wrap",
  // Type arms (overlap with Term where the syntax differs)
  "annotatedType", "applicationType", "forall", "function", "intersection",
  "lambdaType", "schema", "union",
  // Literal arms
  "binary", "boolean", "float", "integer", "string",
  // IntegerType / FloatType
  "bigint", "float32", "float64", "int8", "int16", "int32", "int64",
  "uint8", "uint16", "uint32", "uint64",
  // Either
  "left", "right",
  // Optional
  "given", "none",
]);

// Locate the dist/json/hydra-kernel directory. Searches upward from the
// current file, then falls back to the env var HYDRA_JSON_DIR.
const findKernelJsonDir = (): string | null => {
  const envDir = process.env.HYDRA_JSON_DIR;
  if (envDir) return envDir;
  // From this file at heads/typescript/src/test/typescript/hydra/test/
  // (or the dist copy), dist/json is found by walking up.
  let dir = HERE;
  for (let i = 0; i < 12; i++) {
    const candidate = join(dir, "dist", "json", "hydra-kernel", "src", "main", "json");
    try {
      readFileSync(join(candidate, "manifest.json"), "utf-8");
      return candidate;
    } catch { /* try parent */ }
    const parent = resolve(dir, "..");
    if (parent === dir) break;
    dir = parent;
  }
  return null;
};

// Convert a CaseStatement JSON record to the runtime shape, with the
// `default_` field normalized to a Hydra `Optional<Term>`:
//   absent or null → { tag: "none" }
//   present value  → { tag: "given", value: <converted> }
// Also applies the same field-name normalization (`type` → `type_`,
// Name-wrapping for `typeName`) as the main `convert`.
const convertCases = (rhs: unknown): unknown => {
  if (rhs === null || typeof rhs !== "object" || Array.isArray(rhs)) {
    return convert(rhs);
  }
  const obj = rhs as Record<string, unknown>;
  const out: Record<string, unknown> = {};
  for (const k of Object.keys(obj)) {
    if (k === "default") continue;
    const ck = k === "type" ? "type_" : k === "function" ? "function_" : k;
    out[ck] = convert(obj[k]);
  }
  // Name-wrap fields. Checks the original raw value (obj[k], not out[k]) —
  // see the matching comment in convert() for why. Confirmed via bug_564.
  for (const k of ["typeName", "name"]) {
    if (typeof obj[k] === "string") out[k] = { value: obj[k] };
  }
  if ("default" in obj && obj.default !== null) {
    out.default_ = { tag: "given", value: convert(obj.default) };
  } else {
    out.default_ = { tag: "none" };
  }
  return out;
};

// Compact string form: a union variant whose payload is Unit encodes as a
// bare JSON string (the variant name) instead of a single-key object — see
// docs/json-format.md "Tagged unions" § Compact string form for unit-valued
// variants. Decoders must accept both forms.
//
// Scoped narrowly to "unit" and "void" — the two arm names that are
// zero-payload Type variants (Type.unit, Type.void) and can ONLY appear as
// ordinary JSON data in this converter's untyped, position-agnostic walk if
// something coincidentally used the bare word as string content, which
// doesn't happen anywhere in the kernel's own JSON.
//
// Width-only LiteralType/IntegerType/FloatType arms (string, boolean,
// binary, decimal, int32, float32, ...) are NOT included here even though
// they're also zero-payload in their own unions: those same words are
// common as ordinary string-typed *data* elsewhere (Literal.string content,
// Name values, map keys), so blindly treating every occurrence as a
// compact tag produces false positives — confirmed by a full test-suite
// regression when "string"/"boolean" were added (bug_564 investigation).
// convert() has no type context to disambiguate; only extend this set for
// arm names proven collision-free by the full suite.
const NULLARY_UNION_ARMS = new Set<string>(["unit", "void"]);

// Type-directed compact-form resolution (#572): a bare string can ONLY be a
// valid compact-form encoding of a nullary (Unit-payload) union arm — never
// of ordinary string data — at a JSON position whose declared type is a
// union with a matching nullary arm. `NULLARY_UNION_ARMS` above handles the
// position-independent case (arm names that are nullary in every union they
// ever appear in, so no schema lookup is needed). This table handles arm
// names that are nullary in SOME enclosing union but not others (the #564
// collision class: `literal: "string"` compact-encodes `LiteralType.string`
// when the enclosing union is Type, but `literal: {string: "..."}` never
// compact-encodes when the enclosing union is Term, because `Literal` --
// the payload of `Term.literal` -- has no nullary arms of its own and so is
// never bare-string-encoded). Because of that asymmetry, seeing a bare
// string as the rhs of a recognized `armName` unambiguously selects the
// nullary-arm interpretation: build the map by reading each candidate
// union's own reflected schema from the loaded kernel JSON (hydra.core,
// etc.) rather than hand-enumerating names, so new unions/arms need no
// converter change (the "type-directed" fix requested by #572).
//
// Maps: outer armName -> Set of nullary arm names of that armName's
// payload union (only entries where the payload IS a union with at least
// one nullary arm are recorded; non-union or all-non-nullary payloads are
// omitted, since they can never legitimately appear as a bare string).
//
// Two passes over every union type def in the type-bearing kernel
// namespaces (see TYPE_NAMESPACES below): first collect each union's own
// nullary arm names (by full type name), then scan every arm of every
// union for payloads referencing one of those types and union the nullary
// set into that arm's table entry (an arm name like "literal" can be
// produced by more than one defining union, e.g. both Term and Type).
const buildArmNullaryPayloadTable = (kernelJsonDir: string | null): ReadonlyMap<string, ReadonlySet<string>> => {
  const table = new Map<string, Set<string>>();
  if (!kernelJsonDir) return table;
  const unionsByType = new Map<string, ReadonlyArray<{ name: string; type: unknown }>>();
  for (const ns of TYPE_NAMESPACES) {
    const filePath = join(kernelJsonDir, ns.replace(/\./g, "/") + ".json");
    let raw: string;
    try { raw = readFileSync(filePath, "utf-8"); }
    catch { continue; }
    let parsed: { definitions?: Array<{ type?: { name?: string; body?: unknown } }> };
    try { parsed = JSON.parse(raw); }
    catch { continue; }
    for (const def of parsed.definitions ?? []) {
      const typeName = def.type?.name;
      const schemeBody = def.type?.body as { body?: unknown } | undefined;
      const unionArms = typeName && schemeBody ? unwrapUnionArms(schemeBody.body) : null;
      if (typeName && unionArms) unionsByType.set(typeName, unionArms);
    }
  }
  const nullaryArmsOf = new Map<string, Set<string>>();
  for (const [typeName, arms] of unionsByType) {
    const nullary = new Set(arms.filter((a) => unwrapArmPayload(a.type) === "unit").map((a) => a.name));
    if (nullary.size > 0) nullaryArmsOf.set(typeName, nullary);
  }
  for (const arms of unionsByType.values()) {
    for (const arm of arms) {
      const payload = unwrapArmPayload(arm.type);
      const ref = payload !== null && typeof payload === "object" ? (payload as { variable?: unknown }).variable : undefined;
      const nullary = typeof ref === "string" ? nullaryArmsOf.get(ref) : undefined;
      if (!nullary) continue;
      const existing = table.get(arm.name) ?? new Set<string>();
      for (const n of nullary) existing.add(n);
      table.set(arm.name, existing);
    }
  }
  return table;
};

// Unwrap a type-scheme body down to its `union` arm list, if the type is a
// (possibly annotated) union. Returns null for non-union types.
const unwrapUnionArms = (body: unknown): ReadonlyArray<{ name: string; type: unknown }> | null => {
  if (body === null || typeof body !== "object") return null;
  const b = body as { annotated?: { body?: unknown }; union?: unknown };
  const inner = b.annotated?.body ?? body;
  if (inner === null || typeof inner !== "object") return null;
  const u = (inner as { union?: unknown }).union;
  return Array.isArray(u) ? (u as Array<{ name: string; type: unknown }>) : null;
};

// Unwrap a (possibly annotated) arm payload type to its raw form: the
// string "unit" for a Unit payload, or an object for anything else.
const unwrapArmPayload = (armType: unknown): unknown => {
  if (armType === null || typeof armType !== "object") return armType;
  const a = armType as { annotated?: { body?: unknown } };
  return a.annotated?.body ?? armType;
};

// Populated lazily on first use (after TYPE_NAMESPACES is defined below and
// findKernelJsonDir() can resolve dist/json).
let _armNullaryPayloadCache: ReadonlyMap<string, ReadonlySet<string>> | null = null;
const getArmNullaryPayloadTable = (): ReadonlyMap<string, ReadonlySet<string>> => {
  if (_armNullaryPayloadCache === null) {
    _armNullaryPayloadCache = buildArmNullaryPayloadTable(findKernelJsonDir());
  }
  return _armNullaryPayloadCache;
};

// Convert one JSON value from the adjacently-tagged kernel encoding to
// the internally-tagged runtime encoding. Recursive.
//
// Heuristic for "is this a union arm value?":
//   - Object with exactly one key whose name is in UNION_ARMS.
//   - We also normalize a few field-name differences:
//       Term_application's "function" field renames to "function_"
//         (because `function` is a reserved word in TS).
//       Term_let's "default" field stays as "default_".
const convert = (j: unknown): unknown => {
  if (j === null || j === undefined) return j;
  if (typeof j === "string" && NULLARY_UNION_ARMS.has(j)) return { tag: j };
  if (typeof j !== "object") return j;
  if (Array.isArray(j)) return j.map(convert);
  const obj = j as Record<string, unknown>;
  const keys = Object.keys(obj);
  // Single-key objects that match a union-arm name encode as {tag,value}.
  // Special case: `{none: null}` should become `{tag: "none"}` (no value).
  if (keys.length === 1 && UNION_ARMS.has(keys[0]!)) {
    const armName = keys[0]!;
    const rhs = obj[armName];
    // Nullary arms (Unit, None) — the value is meaningless; drop it.
    if (armName === "unit" || armName === "none") {
      return { tag: armName };
    }
    // Width-only types (no payload).
    if (["int8","int16","int32","int64","uint8","uint16","uint32","uint64","bigint","float32","float64","boolean","string","binary"].includes(armName)) {
      if (rhs === undefined || rhs === null || (typeof rhs === "object" && Object.keys(rhs as object).length === 0)) {
        return { tag: armName };
      }
      // Otherwise the rhs is the literal value (e.g. {"int32": 5}).
      return { tag: armName, value: convert(rhs) };
    }
    // `variable` and `primitive` arms carry a Hydra Name. In the JSON
    // a Name is a bare string; the runtime expects `{value: "..."}`.
    if ((armName === "variable" || armName === "primitive") && typeof rhs === "string") {
      return { tag: armName, value: { value: rhs } };
    }
    // Hydra optional in a Term position: `{optional: null}` = None,
    // `{optional: <v>}` = Given v. The runtime expects:
    //   None → { tag: "optional", value: { tag: "none" } }
    //   Given v  → { tag: "optional", value: { tag: "given", value: <v> } }
    if (armName === "optional") {
      if (rhs === null) {
        return { tag: armName, value: { tag: "none" } };
      }
      return { tag: armName, value: { tag: "given", value: convert(rhs) } };
    }
    // Term_cases: convert and normalize the `default` field to a Optional<Term>.
    // Aeson omits None fields, so an absent "default" means no default;
    // a present "default" is the un-Given'd Term.
    if (armName === "cases") {
      const cs = convertCases(rhs);
      return { tag: armName, value: cs };
    }
    // Term_map (and Term_optional also handled above): the JSON
    // serializes a Map<K,V> as a list of {key, value} objects. The runtime
    // expects a ReadonlyMap. Convert here so primitives like Maps.toList
    // receive the right shape. (#443)
    if (armName === "map") {
      const entries = Array.isArray(rhs)
        ? (rhs as Array<{ key: unknown; value: unknown }>).map((e) => [convert(e.key), convert(e.value)] as const)
        : [];
      return { tag: armName, value: libMaps.fromList(entries as ReadonlyArray<readonly [unknown, unknown]>) };
    }
    // Term_set: JSON serializes a Set as a list. Convert to a ReadonlySet.
    if (armName === "set") {
      const entries = Array.isArray(rhs) ? (rhs as unknown[]).map(convert) : [];
      return { tag: armName, value: libSets.fromList(entries) };
    }
    // Term_pair: JSON serializes Hydra Pair<a,b> as {first: ..., second: ...}.
    // The runtime expects a [Term, Term] tuple so `hydra.lib.pairs.first(p)
    // = p[0]` works. (#443)
    if (armName === "pair"
        && rhs !== null && typeof rhs === "object" && !Array.isArray(rhs)) {
      const r = rhs as { first?: unknown; second?: unknown };
      if ("first" in r && "second" in r) {
        return { tag: armName, value: [convert(r.first), convert(r.second)] as const };
      }
    }
    // Type-directed compact-form decoding (#572, generalizing the #564 fix):
    // a bare string as `armName`'s rhs is only ever a valid encoding when
    // `armName`'s payload type is itself a union with a matching nullary
    // arm — e.g. `{literal: "string"}` = Type.literal(LiteralType.string),
    // since Type.literal's payload is LiteralType (which has nullary arms:
    // string, boolean, binary, decimal). It can never mean Term.literal
    // (payload Literal, which has NO nullary arms — `Literal` is always
    // object-encoded, e.g. `{literal: {string: "..."}}`), so there is no
    // ambiguity to resolve at runtime: seeing a bare string here already
    // proves which enclosing union we're in. getArmNullaryPayloadTable()
    // derives this from the kernel's own reflected type schemas (hydra.core
    // et al.) instead of a hand-maintained name list, so it covers every
    // nullary arm of every union without needing a new entry per case.
    if (typeof rhs === "string") {
      const nullaryArms = getArmNullaryPayloadTable().get(armName);
      if (nullaryArms?.has(rhs)) {
        return { tag: armName, value: { tag: rhs } };
      }
    }
    return { tag: armName, value: convert(rhs) };
  }
  // Otherwise it's a record. Recurse over values; rewrite a few field names
  // that collide with reserved TS keywords (matching what the coder emits).
  const out: Record<string, unknown> = {};
  for (const k of keys) {
    // Rename a few record-field names that collide with TS reserved
    // words. Must match what the coder emits via sanitizeWithUnderscores.
    const ck = k === "function" ? "function_"
             : k === "default" ? "default_"
             : k === "type" ? "type_"
             : k;
    out[ck] = convert(obj[k]);
  }
  // Wrap Name-valued fields. In the JSON, Hydra Names serialize as bare
  // strings; the runtime expects `{value: "..."}`. The kernel's record
  // types use these field names for Name positions.
  //
  // Check the ORIGINAL raw value (obj[k]), not the post-convert() value
  // (out[k]): convert()'s NULLARY_UNION_ARMS compact-tag check turns a raw
  // "unit"/"void" string into {tag: "unit"}/{tag: "void"} regardless of
  // position, so a Name that happens to equal "unit" or "void" (e.g.
  // hydra.core.Type's own `unit`/`void` union-arm field names) would no
  // longer read as a string post-convert() and would silently skip the
  // Name-wrap below. Testing obj[k] instead is immune, since Name-position
  // fields are always plain strings in the raw JSON. Confirmed via bug_564.
  for (const k of ["name", "parameter", "typeName", "field", "fieldName"]) {
    if (typeof obj[k] === "string") {
      out[k] = { value: obj[k] };
    }
  }
  return out;
};

// Recursively strip all `typeApplication` and `typeLambda` wrappers from
// a Term, leaving only the term-level structure. Python and Java do this
// at JSON load time (`strip_all_term_types`) so the evaluator works at
// the simply-typed level — the System F encoding is only needed for
// inference, not evaluation.
const stripTypeAbstractions = (t: unknown): unknown => {
  if (t === null || typeof t !== "object" || Array.isArray(t)) {
    return Array.isArray(t) ? t.map(stripTypeAbstractions) : t;
  }
  const obj = t as Record<string, unknown>;
  if (obj.tag === "typeApplication" || obj.tag === "typeLambda") {
    const v = obj.value as { body?: unknown } | undefined;
    if (v?.body !== undefined) return stripTypeAbstractions(v.body);
    return t;
  }
  // Recurse into all values; preserve structure otherwise.
  const out: Record<string, unknown> = {};
  for (const k of Object.keys(obj)) out[k] = stripTypeAbstractions(obj[k]);
  return out;
};

// Convert a JSON TypeScheme to runtime form. The body Type is converted
// recursively; `variables` is a list of bare strings that need to be
// wrapped as {value: ...} Names; `constraints` is a mandatory
// `Map TypeVariable ConstraintSet` (an absent/empty map means no
// constraints), which the untyped/term-level encoder always emits as an
// entry-array (see docs/specification/json-format.md "Maps" — the
// compact string-keyed object form is limited to the typed,
// schema-directed coders):
//   - omitted, `null`, or `[]` → empty map
//   - `[{key, value: {classes}}]` → map with entries
const convertTypeScheme = (ts: unknown): unknown => {
  if (ts === null || typeof ts !== "object") return ts;
  const obj = ts as Record<string, unknown>;
  const out: Record<string, unknown> = {};
  const varsRaw = obj.variables;
  out.variables = Array.isArray(varsRaw)
    ? varsRaw.map((v) => (typeof v === "string" ? { value: v } : convert(v)))
    : convert(varsRaw);
  out.body = obj.body === undefined ? undefined : convert(obj.body);
  const csRaw = obj.constraints;
  if (csRaw === undefined || csRaw === null) {
    out.constraints = libMaps.empty;
  } else if (Array.isArray(csRaw)) {
    // List of {key, value: {classes: [...]}} pairs
    const pairs: Array<readonly [Name, unknown]> = [];
    for (const item of csRaw) {
      if (item && typeof item === "object") {
        const kv = item as { key?: unknown; value?: { classes?: unknown } };
        const k = typeof kv.key === "string" ? { value: kv.key } as Name : (kv.key as Name);
        const classesArr = (kv.value && typeof kv.value === "object" ? (kv.value as { classes?: unknown }).classes : []) as unknown;
        const classNames = Array.isArray(classesArr)
          ? classesArr.map((c) => (typeof c === "string" ? { value: c } as Name : (c as Name)))
          : [];
        pairs.push([k, { classes: libSets.fromList(classNames) }] as const);
      }
    }
    out.constraints = libMaps.fromList(pairs);
  } else {
    out.constraints = convert(csRaw);
  }
  return out;
};

// Read a kernel JSON file and return both term and type bindings.
//
// The kernel JSON encodes each module definition as either:
//   - `{term: {name, signature, body}}`  — a term binding whose `body` is
//     the Term value and whose `signature` (Optional FunctionSignature)
//     describes its type. The Term value is stored in boundTerms; the
//     signature isn't promoted to a TypeScheme here because reduceTerm
//     only consults `binding.term` during evaluation.
//   - `{type:  {name, body}}`            — a type definition whose `body`
//     is already the kernel `TypeScheme` shape (`{variables, body}`).
interface ModuleParts {
  readonly terms: ReadonlyArray<readonly [Name, Term]>;
  readonly types: ReadonlyArray<readonly [Name, TypeScheme]>;
}

const loadFile = (kernelJsonDir: string, ns: string): ModuleParts => {
  const filePath = join(kernelJsonDir, ns.replace(/\./g, "/") + ".json");
  let raw: string;
  try { raw = readFileSync(filePath, "utf-8"); }
  catch { return { terms: [], types: [] }; }
  const parsed = JSON.parse(raw) as { definitions?: Array<{
    term?: { name?: string; body?: unknown };
    type?: { name?: string; body?: unknown };
  }> };
  const terms: Array<readonly [Name, Term]> = [];
  const types: Array<readonly [Name, TypeScheme]> = [];
  for (const def of parsed.definitions ?? []) {
    if (def.term) {
      const inner = def.term;
      if (!inner.name || inner.body === undefined) continue;
      const name: Name = { value: inner.name } as Name;
      // Convert first, then strip type-level wrappers — the evaluator
      // works at the simply-typed level and chokes on typeApp heads.
      const term = stripTypeAbstractions(convert(inner.body)) as Term;
      terms.push([name, term] as const);
    } else if (def.type) {
      const inner = def.type;
      if (!inner.name || inner.body === undefined) continue;
      const name: Name = { value: inner.name } as Name;
      const scheme = convertTypeScheme(inner.body) as TypeScheme;
      types.push([name, scheme] as const);
    }
  }
  return { terms, types };
};

// Evaluator-essential namespaces (term bindings) — same as Java's
// TestSuiteRunner and Python's _load_kernel_term_bindings.
export const EVALUATOR_NAMESPACES: readonly string[] = [
  "hydra.annotations",
  "hydra.constants",
  "hydra.decode.core",
  "hydra.dependencies",
  "hydra.encode.core",
  "hydra.extract.core",
  "hydra.lexical",
  "hydra.rewriting",
  "hydra.scoping",
  "hydra.print.core",
  "hydra.strip",
  "hydra.variables",
];

// Kernel type-bearing modules — these provide the schema that the
// evaluator's `extract.*` / `cases _Term` operations consult to resolve
// field/arm names. Loading the full type graph isn't strictly required
// for primitives but is needed for any kernel function that decodes
// nominal types via the graph (e.g. `hydra.annotations.setAnnotation`
// uses `Term.annotated`/`AnnotatedTerm.body`).
export const TYPE_NAMESPACES: readonly string[] = [
  "hydra.coders",
  "hydra.core",
  "hydra.compute",
  "hydra.equality",
  "hydra.errors",
  "hydra.error.checking",
  "hydra.error.core",
  "hydra.error.packaging",
  "hydra.graph",
  "hydra.json.model",
  "hydra.json.parsing",
  "hydra.kv",
  "hydra.module",
  "hydra.packaging",
  "hydra.path",
  "hydra.testing",
  "hydra.tools",
  "hydra.util",
];

export interface LoadedBindings {
  readonly terms: ReadonlyArray<readonly [Name, Term]>;
  readonly types: ReadonlyArray<readonly [Name, TypeScheme]>;
}

export const loadKernelBindings = (
  namespaces: readonly string[] = EVALUATOR_NAMESPACES,
): readonly (readonly [Name, Term])[] => {
  const all = loadAll(namespaces);
  return all.terms;
};

export const loadAll = (
  namespaces: readonly string[] = [...EVALUATOR_NAMESPACES, ...TYPE_NAMESPACES],
): LoadedBindings => {
  const dir = findKernelJsonDir();
  if (!dir) return { terms: [], types: [] };
  const terms: Array<readonly [Name, Term]> = [];
  const types: Array<readonly [Name, TypeScheme]> = [];
  for (const ns of namespaces) {
    const parts = loadFile(dir, ns);
    terms.push(...parts.terms);
    types.push(...parts.types);
  }
  return { terms, types };
};
