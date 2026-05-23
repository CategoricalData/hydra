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
import * as libMaps from "../../../../main/typescript/hydra/lib/maps.js";
import * as libSets from "../../../../main/typescript/hydra/lib/sets.js";

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
  "let", "list", "literal", "map", "maybe", "optional", "pair", "primitive",
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
  // Maybe
  "just", "nothing",
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
// `default_` field normalized to a Hydra `Maybe<Term>`:
//   absent or null → { tag: "nothing" }
//   present value  → { tag: "just", value: <converted> }
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
  // Name-wrap fields.
  for (const k of ["typeName", "name"]) {
    if (typeof out[k] === "string") out[k] = { value: out[k] };
  }
  if ("default" in obj && obj.default !== null) {
    out.default_ = { tag: "just", value: convert(obj.default) };
  } else {
    out.default_ = { tag: "nothing" };
  }
  return out;
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
  if (typeof j !== "object") return j;
  if (Array.isArray(j)) return j.map(convert);
  const obj = j as Record<string, unknown>;
  const keys = Object.keys(obj);
  // Single-key objects that match a union-arm name encode as {tag,value}.
  // Special case: `{nothing: null}` should become `{tag: "nothing"}` (no value).
  if (keys.length === 1 && UNION_ARMS.has(keys[0]!)) {
    const armName = keys[0]!;
    const rhs = obj[armName];
    // Nullary arms (Unit, Nothing) — the value is meaningless; drop it.
    if (armName === "unit" || armName === "nothing") {
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
    // Hydra Maybe in a Term position: `{maybe: null}` = Nothing,
    // `{maybe: <v>}` = Just v. The runtime expects:
    //   Nothing → { tag: "maybe", value: { tag: "nothing" } }
    //   Just v  → { tag: "maybe", value: { tag: "just", value: <v> } }
    if (armName === "maybe" || armName === "optional") {
      if (rhs === null) {
        return { tag: armName, value: { tag: "nothing" } };
      }
      return { tag: armName, value: { tag: "just", value: convert(rhs) } };
    }
    // Term_cases: convert and normalize the `default` field to a Maybe<Term>.
    // Aeson omits Nothing fields, so an absent "default" means no default;
    // a present "default" is the un-Just'd Term.
    if (armName === "cases") {
      const cs = convertCases(rhs);
      return { tag: armName, value: cs };
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
  // types use these field names for Name positions:
  for (const k of ["name", "parameter", "typeName", "field", "fieldName"]) {
    if (typeof out[k] === "string") {
      out[k] = { value: out[k] };
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
// `Maybe (Map TypeVariable ConstraintSet)`:
//   - omitted or `null`     → Nothing → {tag: "nothing"}
//   - `[]` (empty map)      → Just emptyMap → {tag: "just", value: <empty CanonMap>}
//   - `[{key, value: {classes}}]` → Just map → {tag: "just", value: <CanonMap>}
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
    out.constraints = { tag: "nothing" };
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
    out.constraints = { tag: "just", value: libMaps.fromList(pairs) };
  } else {
    out.constraints = convert(csRaw);
  }
  return out;
};

// Read a kernel JSON file and return both term and type bindings.
// Each module definition is either `{type: {name, typeScheme}}` (a type
// definition) or `{term: {name, term, typeScheme?}}` (a term binding).
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
    term?: { name?: string; term?: unknown; typeScheme?: unknown };
    type?: { name?: string; typeScheme?: unknown };
  }> };
  const terms: Array<readonly [Name, Term]> = [];
  const types: Array<readonly [Name, TypeScheme]> = [];
  for (const def of parsed.definitions ?? []) {
    if (def.term) {
      const inner = def.term;
      if (!inner.name || inner.term === undefined) continue;
      const name: Name = { value: inner.name } as Name;
      // Convert first, then strip type-level wrappers — the evaluator
      // works at the simply-typed level and chokes on typeApp heads.
      const term = stripTypeAbstractions(convert(inner.term)) as Term;
      terms.push([name, term] as const);
    } else if (def.type) {
      const inner = def.type;
      if (!inner.name || inner.typeScheme === undefined) continue;
      const name: Name = { value: inner.name } as Name;
      const scheme = convertTypeScheme(inner.typeScheme) as TypeScheme;
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
  "hydra.show.core",
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
  "hydra.context",
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
