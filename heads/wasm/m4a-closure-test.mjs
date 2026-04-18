// M4a: validate the call_indirect closure mechanism end-to-end.
//
// Against the hand-written heads/wasm/m4a-closure-demo.wat:
//   1. Build a closure value in linear memory: [tableIdx=0, env=42].
//   2. Call apply_closure(cloPtr, 10).
//   3. The Wasm side loads tableIdx + env from the record, pushes env + arg,
//      and call_indirects via type $__closure_sig to $__closure_add_env_arg.
//   4. Expected: 42 + 10 = 52.
//
// This validates the runtime shape. Coder changes that emit the same
// pattern from DSL-level lambdas are M4b.
//
// Usage: node heads/wasm/m4a-closure-test.mjs

import { compileWat, instantiate, allocRecord } from "./runtimes/node/harness.js";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));

const wasm = compileWat(join(__dirname, "m4a-closure-demo.wat"));
const instance = await instantiate(wasm);
const memory = instance.exports.memory;

// Construct a closure: {tableIdx: 0, env: 42}. 8 bytes, 2 i32 fields.
const cloPtr = allocRecord(instance, memory, [0, 42]);

// Apply the closure to arg=10. Expected: 42 + 10 = 52.
const result = instance.exports.apply_closure(cloPtr, 10);

const pass = result === 52;
console.log(`  ${pass ? "PASS" : "FAIL"}  apply_closure({tableIdx=0, env=42}, 10) -> ${result} (want 52)`);

// Second case: different env.
const cloPtr2 = allocRecord(instance, memory, [0, 100]);
const result2 = instance.exports.apply_closure(cloPtr2, 7);
const pass2 = result2 === 107;
console.log(`  ${pass2 ? "PASS" : "FAIL"}  apply_closure({tableIdx=0, env=100}, 7) -> ${result2} (want 107)`);

// Third case: zero env.
const cloPtr3 = allocRecord(instance, memory, [0, 0]);
const result3 = instance.exports.apply_closure(cloPtr3, 99);
const pass3 = result3 === 99;
console.log(`  ${pass3 ? "PASS" : "FAIL"}  apply_closure({tableIdx=0, env=0}, 99) -> ${result3} (want 99)`);

const passed = [pass, pass2, pass3].filter(Boolean).length;
console.log("");
console.log(`${passed}/3 passed`);
process.exit(passed === 3 ? 0 : 1);
