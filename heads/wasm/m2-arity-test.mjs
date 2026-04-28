// M2: exercise case-dispatch via hydra.arity.type_arity.
//
// This is a JS-defined test file rather than a JSON manifest because M2
// tests construct live Wasm values as input (tagged unions, records) —
// JSON can't describe the construction step. Once closures land (M4) and
// kernel values can be built from Hydra code rather than JS, we'll revisit
// whether programmatic tests like this one are still needed.
//
// Usage: node heads/wasm/m2-arity-test.mjs

import { compileWat, instantiate, runTests } from "./runtimes/node/harness.js";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..", "..");
const WAT = join(ROOT, "dist/wasm/hydra-kernel/src/main/wat/hydra/arity.wat");

// Variant tags for hydra.core.Type (ordered as declared in Core.hs):
const T = {
  Annotated: 0,  Application: 1, Either: 2,  Forall: 3,
  Function: 4,   List: 5,        Literal: 6, Map: 7,
  Maybe: 8,      Pair: 9,        Record: 10, Set: 11,
  Union: 12,     Unit: 13,       Variable: 14, Void: 15,
  Wrap: 16,
};

// Stub primitive imports. type_arity uses math.add (increments the arity
// on each function arrow). We don't care about the i32 values' correctness
// beyond "math.add returns a+b" for the arrow-count test.
const imports = {
  "hydra.lib.math": {
    "hydra.lib.math.add": (a, b) => (a + b) | 0,
    "hydra.lib.math.sub": (a, b) => (a - b) | 0,
  },
  "hydra.lib.lists": {
    "hydra.lib.lists.cons": (_x, _xs) => 0,
  },
};

const wasm = compileWat(WAT);
const instance = await instantiate(wasm, imports);
const memory = instance.exports.memory;

// Construct a TypeUnit: variant with tag=13, payload=0.
//   type_arity TypeUnit should hit the default arm → 0.
// Construct a TypeFunction with tag=4, payload = ptr to FunctionType record.
//   FunctionType layout: {domain: Type, codomain: Type} — 2 i32 pointers.
//   type_arity (TypeFunction (FunctionType Unit Unit)) = 1 + type_arity Unit = 1.
// Construct a nested function: TypeFunction (Unit -> (Unit -> Unit)).
//   Arity should be 2.
// Construct a TypeForall with tag=3, payload = ptr to ForallType record.
//   ForallType: {parameter: Name, body: Type}. type_arity recurses into body.
//   For ForallType _ (Function Unit Unit), arity = 1.
// Construct a TypeList (tag=5) with payload = TypeUnit. No case handles List,
//   so it falls through to default → 0.

const tests = [
  {
    name: "type_arity TypeUnit = 0",
    export: "hydra.arity.type_arity",
    kind: "i32",
    expected: 0,
    args: (ctx) => {
      const unit = ctx.allocVariant(T.Unit, 0);
      return [unit];
    },
  },
  {
    name: "type_arity TypeList _ = 0 (default arm, not explicit case)",
    export: "hydra.arity.type_arity",
    kind: "i32",
    expected: 0,
    args: (ctx) => {
      const unitInner = ctx.allocVariant(T.Unit, 0);
      const listTy = ctx.allocVariant(T.List, unitInner);
      return [listTy];
    },
  },
  {
    name: "type_arity (Unit -> Unit) = 1",
    export: "hydra.arity.type_arity",
    kind: "i32",
    expected: 1,
    args: (ctx) => {
      const dom = ctx.allocVariant(T.Unit, 0);
      const cod = ctx.allocVariant(T.Unit, 0);
      // FunctionType { domain, codomain } — 2-field record.
      const ft = ctx.allocRecord([dom, cod]);
      const fnTy = ctx.allocVariant(T.Function, ft);
      return [fnTy];
    },
  },
  {
    name: "type_arity (Unit -> Unit -> Unit) = 2",
    export: "hydra.arity.type_arity",
    kind: "i32",
    expected: 2,
    args: (ctx) => {
      const u1 = ctx.allocVariant(T.Unit, 0);
      const u2 = ctx.allocVariant(T.Unit, 0);
      const u3 = ctx.allocVariant(T.Unit, 0);
      const innerFt = ctx.allocRecord([u2, u3]);
      const innerFn = ctx.allocVariant(T.Function, innerFt);
      const outerFt = ctx.allocRecord([u1, innerFn]);
      const outerFn = ctx.allocVariant(T.Function, outerFt);
      return [outerFn];
    },
  },
  {
    name: "type_arity TypeEither _ = 0 (Either was misrouted before br_table fix)",
    export: "hydra.arity.type_arity",
    kind: "i32",
    expected: 0,
    args: (ctx) => {
      // EitherType {left: Type, right: Type}
      const u1 = ctx.allocVariant(T.Unit, 0);
      const u2 = ctx.allocVariant(T.Unit, 0);
      const et = ctx.allocRecord([u1, u2]);
      const eitherTy = ctx.allocVariant(T.Either, et);
      return [eitherTy];
    },
  },
];

const code = runTests(instance, memory, tests);
process.exit(code);
