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
    // TypeLiteral (`{literal: <LiteralType>}`): the payload is a LiteralType.
    // Its unit-valued arms (string, boolean, binary, decimal) serialize in
    // Aeson's compact string form as a bare word, e.g. `{literal: "string"}`
    // = TypeLiteral(LiteralType.string). convert()'s NULLARY_UNION_ARMS set
    // deliberately excludes these words (they collide with ordinary string
    // data elsewhere — see bug_564), so the bare word survives unconverted
    // and reaches show/inference as a malformed `{tag:"literal", value:"string"}`
    // instead of `{tag:"literal", value:{tag:"string"}}`. Here we have type
    // context (we know rhs is a LiteralType), so promoting a bare-string
    // payload to `{tag: rhs}` is safe. Non-string payloads (integer/float,
    // which carry a width) fall through to the generic convert(rhs). (#564)
    if (armName === "literal" && typeof rhs === "string") {
      return { tag: armName, value: { tag: rhs } };
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
// wrapped as {value: ...} Names; `constraints` is in Aeson's encoding of
// `Optional (Map TypeVariable ConstraintSet)`:
//   - omitted or `null`     → None → {tag: "none"}
//   - `[]` (empty map)      → Given emptyMap → {tag: "given", value: <empty CanonMap>}
//   - `[{key, value: {classes}}]` → Given map → {tag: "given", value: <CanonMap>}
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
    out.constraints = { tag: "none" };
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
    out.constraints = { tag: "given", value: libMaps.fromList(pairs) };
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
