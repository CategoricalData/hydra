// Minimal Node harness for running Hydra kernel WAT modules.
//
// Loads a .wasm (pre-compiled) or .wat (compiled at runtime via wabt/wat2wasm),
// invokes a named export, and returns the raw i32 result. Helper functions
// decode Hydra kernel values (strings, records, variants) from the module's
// linear memory.
//
// No npm dependencies. Requires Node >= 20.

import { readFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

// -----------------------------------------------------------------------
// Module loading
// -----------------------------------------------------------------------

// Load a .wat file by shelling out to wat2wasm and reading the binary.
// Returns a Uint8Array of the .wasm bytes.
export function compileWat(watPath) {
  const result = spawnSync("wat2wasm", [watPath, "--output=/dev/stdout"], {
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.status !== 0) {
    throw new Error(`wat2wasm failed on ${watPath}:\n${result.stderr.toString()}`);
  }
  return new Uint8Array(result.stdout);
}

// Instantiate a .wasm Uint8Array with the given imports object.
// Imports: { moduleName: { funcName: jsFunction, ... }, ... }
export async function instantiate(wasmBytes, imports = {}) {
  const module = await WebAssembly.compile(wasmBytes);
  const instance = await WebAssembly.instantiate(module, imports);
  return instance;
}

// -----------------------------------------------------------------------
// Kernel value decoding
// -----------------------------------------------------------------------

// Read a Hydra String from linear memory at `ptr`.
// Layout: [len:i32 little-endian][bytes...] (UTF-8).
export function readString(memory, ptr) {
  const view = new DataView(memory.buffer);
  const len = view.getUint32(ptr, /*littleEndian*/ true);
  const bytes = new Uint8Array(memory.buffer, ptr + 4, len);
  return new TextDecoder("utf-8").decode(bytes);
}

// -----------------------------------------------------------------------
// Test harness (M1)
// -----------------------------------------------------------------------

// A test case is:
//   { name: string, export: string, kind: "i32" | "string", expected: any }
// Runs one test and returns { name, pass, actual, expected, error? }.
export function runTest(instance, memory, test) {
  try {
    const fn = instance.exports[test.export];
    if (typeof fn !== "function") {
      return {
        name: test.name,
        pass: false,
        error: `export not found: ${test.export}`,
      };
    }
    const raw = fn();
    let actual;
    if (test.kind === "i32") {
      actual = raw | 0;
    } else if (test.kind === "string") {
      actual = readString(memory, raw);
    } else {
      throw new Error(`unknown kind: ${test.kind}`);
    }
    const pass = actual === test.expected;
    return { name: test.name, pass, actual, expected: test.expected };
  } catch (e) {
    return { name: test.name, pass: false, error: e.message };
  }
}

// Run every test in `tests` and print a summary. Returns exit code (0 pass, 1 fail).
export function runTests(instance, memory, tests) {
  let passed = 0;
  let failed = 0;
  for (const t of tests) {
    const r = runTest(instance, memory, t);
    if (r.pass) {
      passed++;
      console.log(`  PASS  ${r.name}`);
    } else {
      failed++;
      if (r.error) {
        console.log(`  FAIL  ${r.name}  (error: ${r.error})`);
      } else {
        console.log(`  FAIL  ${r.name}  (got ${JSON.stringify(r.actual)}, want ${JSON.stringify(r.expected)})`);
      }
    }
  }
  console.log("");
  console.log(`${passed}/${passed + failed} passed`);
  return failed === 0 ? 0 : 1;
}

// -----------------------------------------------------------------------
// CLI entrypoint (when invoked directly)
// -----------------------------------------------------------------------

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

if (process.argv[1] === __filename) {
  const [, , watPath, manifestPath] = process.argv;
  if (!watPath || !manifestPath) {
    console.error("Usage: harness.js <wat-file> <manifest.json>");
    process.exit(2);
  }
  const wasm = compileWat(watPath);
  const manifest = JSON.parse(readFileSync(manifestPath, "utf-8"));
  const instance = await instantiate(wasm);
  const memory = instance.exports.memory;
  const code = runTests(instance, memory, manifest.tests);
  process.exit(code);
}
