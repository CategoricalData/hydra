// M3 survey: run a range of first-order kernel functions and classify outcomes.
//
// Each test specifies the kernel function, how to build inputs, and expected
// output. Failures are classified as WRONG (ran but wrong result), ERROR
// (trap/runtime error), or LOAD (module-instantiation failure).
//
// The goal is to map the "frontier" of what works — which encoder idioms
// produce correct kernel code vs. which are still broken. Outcomes inform
// the next round of encoder fixes.
//
// Usage: node heads/wasm/m3-survey.mjs

import { compileWat, instantiateWithStubs, readString, alloc, allocVariant, allocRecord, writeI32 } from "./runtimes/node/harness.js";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..", "..");

// Variant tags for hydra.core.Type (in declaration order).
const TyT = {
  Annotated: 0, Application: 1, Either: 2, Forall: 3, Function: 4,
  List: 5, Literal: 6, Map: 7, Maybe: 8, Pair: 9,
  Record: 10, Set: 11, Union: 12, Unit: 13, Variable: 14,
  Void: 15, Wrap: 16,
};

// Variant tags for hydra.core.Term (in declaration order).
const TmT = {
  Annotated: 0, Application: 1, Cases: 2, Either: 3, Inject: 4,
  Lambda: 5, Let: 6, List: 7, Literal: 8, Map: 9,
  Maybe: 10, Pair: 11, Project: 12, Record: 13, Set: 14,
  TypeApplication: 15, TypeLambda: 16, Unit: 17, Unwrap: 18, Variable: 19,
  Wrap: 20,
};

// Stub primitive imports — wide enough to cover what survey targets use.
// Every fn is first-order (no closure-carrying args).
const imports = {
  "hydra.lib.math": {
    "hydra.lib.math.add": (a, b) => (a + b) | 0,
    "hydra.lib.math.sub": (a, b) => (a - b) | 0,
    "hydra.lib.math.mul": (a, b) => Math.imul(a, b),
  },
  "hydra.lib.lists": {
    "hydra.lib.lists.cons": (_x, _xs) => 0,
    "hydra.lib.lists.null": (_xs) => 1,
    "hydra.lib.lists.length": (_xs) => 0,
  },
  "hydra.lib.equality": {
    "hydra.lib.equality.equal": (a, b) => (a === b) ? 1 : 0,
  },
};

// Helper: build a TermAnnotated around a given inner Term.
function mkAnnotatedTerm(ctx, innerTerm) {
  // AnnotatedTerm record: {subject: Term, annotation: ...} — encoded as [innerTerm, 0].
  const ann = ctx.allocRecord([innerTerm, 0]);
  return ctx.allocVariant(TmT.Annotated, ann);
}

// Helper: build a plain TermUnit.
function mkUnitTerm(ctx) {
  return ctx.allocVariant(TmT.Unit, 0);
}

// Helper: build a TypeUnit.
function mkUnitType(ctx) {
  return ctx.allocVariant(TyT.Unit, 0);
}

// Helper: build a TypeFunction (a -> b).
function mkFunctionType(ctx, dom, cod) {
  const ft = ctx.allocRecord([dom, cod]);
  return ctx.allocVariant(TyT.Function, ft);
}

// Survey cases. Each has {module, export, args(ctx), expected, kind} and an
// optional `describe` note explaining what's being exercised.
const cases = [
  // ----- hydra.strip.deannotate_term -----
  {
    module: "hydra/strip.wat",
    export: "hydra.strip.deannotate_term",
    describe: "strip: TermUnit → TermUnit (default arm)",
    args: (ctx) => [mkUnitTerm(ctx)],
    kind: "i32-truthy-same",  // expect exact same pointer back
    check: (actual, info) => actual === info.inputs[0],
  },
  {
    module: "hydra/strip.wat",
    export: "hydra.strip.deannotate_term",
    describe: "strip: TermAnnotated(TermUnit) → TermUnit (unwrap)",
    args: (ctx) => {
      const unit = mkUnitTerm(ctx);
      const ann = mkAnnotatedTerm(ctx, unit);
      return { inputs: [ann], unit };
    },
    kind: "i32-equals",
    check: (actual, info) => actual === info.unit,
  },
  {
    module: "hydra/strip.wat",
    export: "hydra.strip.deannotate_term",
    describe: "strip: nested TermAnnotated(TermAnnotated(TermUnit)) → TermUnit",
    args: (ctx) => {
      const unit = mkUnitTerm(ctx);
      const a1 = mkAnnotatedTerm(ctx, unit);
      const a2 = mkAnnotatedTerm(ctx, a1);
      return { inputs: [a2], unit };
    },
    kind: "i32-equals",
    check: (actual, info) => actual === info.unit,
  },

  // ----- hydra.strip.deannotate_type -----
  {
    module: "hydra/strip.wat",
    export: "hydra.strip.deannotate_type",
    describe: "strip: TypeUnit → TypeUnit (default arm)",
    args: (ctx) => [mkUnitType(ctx)],
    kind: "i32-equals",
    check: (actual, info) => actual === info.inputs[0],
  },

  // ----- hydra.predicates.is_unit_term -----
  {
    module: "hydra/predicates.wat",
    export: "hydra.predicates.is_unit_term",
    describe: "predicates: is_unit_term TermUnit = true (1)",
    args: (ctx) => [mkUnitTerm(ctx)],
    kind: "i32-equals",
    expected: 1,
  },
  {
    module: "hydra/predicates.wat",
    export: "hydra.predicates.is_unit_term",
    describe: "predicates: is_unit_term TermAnnotated(TermUnit) = false (0, no auto-deannotate)",
    args: (ctx) => {
      const unit = mkUnitTerm(ctx);
      return [mkAnnotatedTerm(ctx, unit)];
    },
    kind: "i32-equals",
    expected: 0,
  },

  // ----- hydra.predicates.is_unit_type -----
  {
    module: "hydra/predicates.wat",
    export: "hydra.predicates.is_unit_type",
    describe: "predicates: is_unit_type TypeUnit = true (1)",
    args: (ctx) => [mkUnitType(ctx)],
    kind: "i32-equals",
    expected: 1,
  },
  {
    module: "hydra/predicates.wat",
    export: "hydra.predicates.is_unit_type",
    describe: "predicates: is_unit_type (Unit -> Unit) = false (0)",
    args: (ctx) => {
      const u1 = mkUnitType(ctx);
      const u2 = mkUnitType(ctx);
      return [mkFunctionType(ctx, u1, u2)];
    },
    kind: "i32-equals",
    expected: 0,
  },

  // ----- hydra.arity.type_arity (already validated in M2 — regression guard) -----
  {
    module: "hydra/arity.wat",
    export: "hydra.arity.type_arity",
    describe: "arity: type_arity (Unit -> Unit -> Unit) = 2 [M2 regression]",
    args: (ctx) => {
      const u1 = mkUnitType(ctx), u2 = mkUnitType(ctx), u3 = mkUnitType(ctx);
      const inner = mkFunctionType(ctx, u2, u3);
      return [mkFunctionType(ctx, u1, inner)];
    },
    kind: "i32-equals",
    expected: 2,
  },

  // ----- hydra.encoding.is_unit_type — another is_unit_type? -----
  {
    module: "hydra/encoding.wat",
    export: "hydra.encoding.is_unit_type",
    describe: "encoding: is_unit_type TypeUnit = true",
    args: (ctx) => [mkUnitType(ctx)],
    kind: "i32-equals",
    expected: 1,
  },

  // ----- hydra.arity.type_scheme_arity (projection on record) -----
  {
    module: "hydra/arity.wat",
    export: "hydra.arity.type_scheme_arity",
    describe: "arity: type_scheme_arity [TypeScheme {_, Unit}] = 0",
    args: (ctx) => {
      // TypeScheme {variables: [], type: TypeUnit} — 2-field record.
      const ty = mkUnitType(ctx);
      const ts = ctx.allocRecord([0, ty]);   // vars=empty-ptr, type=Unit
      return [ts];
    },
    kind: "i32-equals",
    expected: 0,
  },
  {
    module: "hydra/arity.wat",
    export: "hydra.arity.type_scheme_arity",
    describe: "arity: type_scheme_arity [TypeScheme {_, Unit->Unit}] = 1",
    args: (ctx) => {
      const u1 = mkUnitType(ctx), u2 = mkUnitType(ctx);
      const ft = mkFunctionType(ctx, u1, u2);
      const ts = ctx.allocRecord([0, ft]);
      return [ts];
    },
    kind: "i32-equals",
    expected: 1,
  },

  // ----- hydra.arity.uncurry_type (has a Just $ list [var "t"] default) -----
  {
    module: "hydra/arity.wat",
    export: "hydra.arity.uncurry_type",
    describe: "arity: uncurry_type Unit returns 1-elem list (not empty, not null)",
    args: (ctx) => [mkUnitType(ctx)],
    kind: "i32-equals",
    check: (actual, info) => actual !== 0,  // non-null pointer
  },

  // ----- hydra.hoisting.is_application_function -----
  {
    module: "hydra/hoisting.wat",
    export: "hydra.hoisting.is_application_function",
    describe: "hoisting: is_application_function SubtermStepApplicationFunction = 1",
    args: (ctx) => [ctx.allocVariant(1, 0)],  // tag 1 = ApplicationFunction
    kind: "i32-equals",
    expected: 1,
  },
  {
    module: "hydra/hoisting.wat",
    export: "hydra.hoisting.is_application_function",
    describe: "hoisting: is_application_function SubtermStepAnnotatedBody = 0",
    args: (ctx) => [ctx.allocVariant(0, 0)],  // tag 0 = AnnotatedBody
    kind: "i32-equals",
    expected: 0,
  },
  {
    module: "hydra/hoisting.wat",
    export: "hydra.hoisting.is_lambda_body",
    describe: "hoisting: is_lambda_body SubtermStepLambdaBody = 1",
    args: (ctx) => [ctx.allocVariant(3, 0)],  // tag 3 = LambdaBody
    kind: "i32-equals",
    expected: 1,
  },
  {
    module: "hydra/hoisting.wat",
    export: "hydra.hoisting.is_union_elimination",
    describe: "hoisting: is_union_elimination TermCases = 1",
    args: (ctx) => [ctx.allocVariant(TmT.Cases, 0)],  // term tag 2 = Cases
    kind: "i32-equals",
    expected: 1,
  },
  {
    module: "hydra/hoisting.wat",
    export: "hydra.hoisting.is_union_elimination",
    describe: "hoisting: is_union_elimination TermUnit = 0",
    args: (ctx) => [mkUnitTerm(ctx)],
    kind: "i32-equals",
    expected: 0,
  },

  // ----- hydra.strip.deannotate_and_detype_term (recursive) -----
  {
    module: "hydra/strip.wat",
    export: "hydra.strip.deannotate_and_detype_term",
    describe: "strip: deannotate_and_detype_term TermUnit = TermUnit",
    args: (ctx) => [mkUnitTerm(ctx)],
    kind: "i32-equals",
    check: (actual, info) => actual === info.inputs[0],
  },
];

// ---- Runner ----

function summarize(results) {
  const total = results.length;
  const pass = results.filter((r) => r.verdict === "PASS").length;
  const wrong = results.filter((r) => r.verdict === "WRONG").length;
  const errored = results.filter((r) => r.verdict === "ERROR").length;
  const loaded = results.filter((r) => r.verdict === "LOAD-FAIL").length;

  console.log("");
  console.log(`=== Summary: ${pass}/${total} passed ===`);
  if (wrong > 0) console.log(`  WRONG:     ${wrong}`);
  if (errored > 0) console.log(`  ERROR:     ${errored}`);
  if (loaded > 0) console.log(`  LOAD-FAIL: ${loaded}`);
  return pass === total ? 0 : 1;
}

const results = [];
const modCache = new Map();

async function loadModule(relPath) {
  if (!modCache.has(relPath)) {
    const wat = join(ROOT, "dist/wasm/hydra-kernel/src/main/wat", relPath);
    try {
      const wasm = compileWat(wat);
      const instance = await instantiateWithStubs(wasm, imports);
      modCache.set(relPath, { instance, memory: instance.exports.memory });
    } catch (e) {
      modCache.set(relPath, { loadError: e.message });
    }
  }
  return modCache.get(relPath);
}

for (const c of cases) {
  const mod = await loadModule(c.module);
  if (mod.loadError) {
    results.push({ describe: c.describe, verdict: "LOAD-FAIL", detail: mod.loadError });
    console.log(`  LOAD  ${c.describe}  (${mod.loadError})`);
    continue;
  }
  const { instance, memory } = mod;
  const ctx = {
    instance, memory,
    alloc: (sz) => alloc(instance, sz),
    allocVariant: (tag, payload) => allocVariant(instance, memory, tag, payload),
    allocRecord: (vals) => allocRecord(instance, memory, vals),
    writeI32: (ptr, v) => writeI32(memory, ptr, v),
  };
  const fn = instance.exports[c.export];
  if (typeof fn !== "function") {
    results.push({ describe: c.describe, verdict: "LOAD-FAIL", detail: `no export ${c.export}` });
    console.log(`  LOAD  ${c.describe}  (no export ${c.export})`);
    continue;
  }
  try {
    const raw = c.args(ctx);
    // args(ctx) may return either [args] or { inputs: [args], ...info }
    const info = Array.isArray(raw) ? { inputs: raw } : raw;
    const inputs = info.inputs;
    const actual = fn(...inputs) | 0;
    let ok;
    if (c.check) {
      ok = c.check(actual, info);
    } else {
      ok = actual === c.expected;
    }
    if (ok) {
      results.push({ describe: c.describe, verdict: "PASS", actual });
      console.log(`  PASS  ${c.describe}`);
    } else {
      const exp = c.check ? `(custom check)` : c.expected;
      results.push({ describe: c.describe, verdict: "WRONG", actual, expected: exp });
      console.log(`  WRONG ${c.describe}  (got ${actual}, want ${exp})`);
    }
  } catch (e) {
    results.push({ describe: c.describe, verdict: "ERROR", detail: e.message });
    console.log(`  ERROR ${c.describe}  (${e.message})`);
  }
}

process.exit(summarize(results));
