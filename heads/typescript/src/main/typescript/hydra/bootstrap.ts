// TypeScript bootstrapping entry point: loads Hydra modules from JSON
// and generates code for a target language. This is the TS analog of
// hydra.bootstrap in Python (heads/python/src/main/python/hydra/bootstrap.py)
// and the Haskell bootstrap-from-json executable.
//
// Usage:
//   tsx hydra/bootstrap.ts --target <lang> --json-dir <dist/json> [OPTIONS]
//
// Options:
//   --output <dir>      Output base directory (default: /tmp/hydra-bootstrapping-demo)
//   --include-coders    Also load coder packages (hydra-typescript, hydra-python, ...)
//   --include-tests     Also load and generate kernel test modules
//   --kernel-only       Only generate kernel modules
//
// As a head, this file lives under src/main/typescript/hydra/ so it
// gets copied alongside the runtime by copy-kernel-runtime.sh and ships
// inside dist/typescript/hydra-kernel/. The script imports kernel
// modules via `./` relative paths so it resolves cleanly both in the
// repo source tree and inside the dist tree.

import { readFileSync, readdirSync, writeFileSync, mkdirSync, existsSync, statSync } from "node:fs";
import { dirname, join, resolve } from "node:path";

import * as jsonDecode from "./json/decode.js";
import * as jsonBootstrap from "./json/bootstrap.js";
import * as decodePackaging from "./decode/packaging.js";
import * as lexical from "./lexical.js";
import * as maps from "./lib/maps.js";
import * as codegen from "./codegen.js";
import * as libraries from "./lib/libraries.js";

// Imported via dynamic import below since these come from sibling
// packages that only exist when the corresponding hydra-<lang>/ tree
// has been assembled into dist/typescript/.
//
//   typescript -> hydra-typescript/.../typeScript/coder.ts
//   python     -> hydra-python/.../python/coder.ts
//   etc.

type Module = unknown;

// Parse argv into a simple options bag.
const parseArgs = (argv: string[]): {
  target: string;
  jsonDir: string;
  output: string;
  includeCoders: boolean;
  includeTests: boolean;
  kernelOnly: boolean;
} => {
  let target = "";
  let jsonDir = "";
  let output = "/tmp/hydra-bootstrapping-demo";
  let includeCoders = false;
  let includeTests = false;
  let kernelOnly = false;
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i]!;
    switch (a) {
      case "--target":         target = argv[++i]!; break;
      case "--json-dir":       jsonDir = argv[++i]!; break;
      case "--output":         output = argv[++i]!; break;
      case "--include-coders": includeCoders = true; break;
      case "--include-tests":  includeTests = true; break;
      case "--kernel-only":    kernelOnly = true; break;
      default:                  // ignore unknown
    }
  }
  if (!target) { console.error("error: --target is required"); process.exit(1); }
  if (!jsonDir) { console.error("error: --json-dir is required"); process.exit(1); }
  return { target, jsonDir, output, includeCoders, includeTests, kernelOnly };
};

// Read every .json under <root> and return its (relative-namespace, parsed-JSON) pairs.
const readJsonTree = (root: string): Array<{ path: string; json: unknown }> => {
  const out: Array<{ path: string; json: unknown }> = [];
  const walk = (dir: string): void => {
    for (const entry of readdirSync(dir)) {
      const full = join(dir, entry);
      const st = statSync(full);
      if (st.isDirectory()) { walk(full); continue; }
      if (!entry.endsWith(".json")) continue;
      if (entry === "manifest.json") continue;
      try {
        const raw = readFileSync(full, "utf-8");
        out.push({ path: full, json: JSON.parse(raw) });
      } catch (e) {
        console.error(`  warning: failed to parse ${full}: ${(e as Error).message}`);
      }
    }
  };
  walk(root);
  return out;
};

// Coder dispatch: map --target lang to (importPath, exportName).
// importPath is resolved from `dist/typescript/hydra-kernel/src/main/typescript/hydra/bootstrap.ts`,
// so going up 5 levels lands at `dist/typescript/`, then dispatching into each sibling package.
const CODER_DISPATCH: Record<string, { importPath: string; exportName: string }> = {
  typescript: { importPath: "../../../../../hydra-typescript/src/main/typescript/hydra/typeScript/coder.js", exportName: "moduleToTypeScript" },
  python:     { importPath: "../../../../../hydra-python/src/main/typescript/hydra/python/coder.js",         exportName: "moduleToPython" },
  java:       { importPath: "../../../../../hydra-java/src/main/typescript/hydra/java/coder.js",             exportName: "moduleToJava" },
  haskell:    { importPath: "../../../../../hydra-haskell/src/main/typescript/hydra/haskell/coder.js",       exportName: "moduleToHaskell" },
  scala:      { importPath: "../../../../../hydra-scala/src/main/typescript/hydra/scala/coder.js",           exportName: "moduleToScala" },
};

const main = async (): Promise<void> => {
  const opts = parseArgs(process.argv.slice(2));
  const dispatch = CODER_DISPATCH[opts.target];
  if (!dispatch) {
    console.error(`error: unsupported --target ${opts.target}. Available: ${Object.keys(CODER_DISPATCH).join(", ")}`);
    process.exit(1);
  }

  console.log("==========================================");
  console.log("TypeScript bootstrap-from-json");
  console.log("==========================================");
  console.log(`  Target:       ${opts.target}`);
  console.log(`  JSON dir:     ${opts.jsonDir}`);
  console.log(`  Output base:  ${opts.output}`);
  console.log(`  Include tests:${opts.includeTests}`);
  console.log("");

  const t0 = Date.now();

  // Step 1: load the target-language coder.
  let coderMod: Record<string, unknown>;
  try {
    coderMod = await import(dispatch.importPath);
  } catch (e) {
    console.error(`error: failed to load coder ${dispatch.importPath}: ${(e as Error).message}`);
    console.error(`       (target=${opts.target} requires the corresponding hydra-${opts.target} package to be assembled into dist/typescript/)`);
    process.exit(1);
  }
  const moduleTo = coderMod[dispatch.exportName] as ((m: unknown) => (defs: unknown[]) => (cx: unknown) => (g: unknown) => { tag: "left"; value: unknown } | { tag: "right"; value: Map<string, string> });
  if (typeof moduleTo !== "function") {
    console.error(`error: ${dispatch.exportName} not found in ${dispatch.importPath}`);
    process.exit(1);
  }

  // Step 2: collect JSON files. The kernel JSON layout is per-namespace:
  // hydra/core.json, hydra/lib/maps.json, etc. — under src/main/json/.
  // Test modules live in a parallel src/test/json/ tree.
  // Caller passes --json-dir pointing at the main tree; we also look for
  // a sibling test tree when --include-tests is set.
  const mainJsonFiles = readJsonTree(opts.jsonDir);
  const mainNames = new Set(mainJsonFiles.map((f) => f.path));
  let testJsonFiles: Array<{ path: string; json: unknown }> = [];
  if (opts.includeTests) {
    // jsonDir is .../src/main/json; the test tree is .../src/test/json
    const testJsonDir = opts.jsonDir.replace(/src\/main\/json$/, "src/test/json");
    if (testJsonDir !== opts.jsonDir && existsSync(testJsonDir)) {
      testJsonFiles = readJsonTree(testJsonDir);
      console.log(`  Loaded ${mainJsonFiles.length} main + ${testJsonFiles.length} test JSON files`);
    } else {
      console.log(`  Loaded ${mainJsonFiles.length} JSON files (no test tree at ${testJsonDir})`);
    }
  } else {
    console.log(`  Loaded ${mainJsonFiles.length} JSON files`);
  }
  void mainNames;
  const jsonFiles = [...mainJsonFiles.map((f) => ({ ...f, isTest: false })),
                     ...testJsonFiles.map((f) => ({ ...f, isTest: true }))];

  // Step 3: decode JSON values into Hydra Module records. Each JSON file
  // contains a serialized hydra.packaging.Module. We use the bootstrap
  // schema map (typesByName from hydra/json/bootstrap.ts) so the decoder
  // knows about hydra.packaging.Module without needing a populated graph.
  // Pipeline:
  //   raw JSON → hydra.json.model.Value (tagged-union shape) via toHydraJson
  //   Value → Term via jsonDecode.fromJson(schemaMap, ...)
  //   Term → Module via decode.packaging.module_(graph, term)
  // Same approach as Python's decode_module in hydra/generation.py.
  const toHydraJson = (v: unknown): unknown => {
    if (v === null) return { tag: "null" };
    if (typeof v === "boolean") return { tag: "boolean", value: v };
    if (typeof v === "number") return { tag: "number", value: v };
    if (typeof v === "string") return { tag: "string", value: v };
    if (Array.isArray(v)) return { tag: "array", value: v.map(toHydraJson) };
    if (typeof v === "object") {
      const m = new Map<string, unknown>();
      for (const [k, val] of Object.entries(v as Record<string, unknown>)) {
        m.set(k, toHydraJson(val));
      }
      return { tag: "object", value: m };
    }
    return { tag: "null" };
  };
  // Build a bootstrap graph that includes the hand-coded standard
  // primitives library. Coders for richer targets (Python, Java, ...)
  // dereference these primitives by name when emitting method bodies;
  // without them, every module that calls `hydra.lib.maps.empty` etc.
  // fails with `Unknown variable`. Mirrors Python's bootstrap_graph()
  // which calls standard_library() to populate `primitives`.
  const stdPrims = (libraries as { standardPrimitives: () => ReadonlyArray<{ name: { value: string } }> }).standardPrimitives();
  const primMap = (maps as { fromList: (entries: ReadonlyArray<readonly [unknown, unknown]>) => unknown }).fromList(
    stdPrims.map((p) => [p.name, p] as const)
  );
  const bsGraph = { ...(lexical.emptyGraph as object), primitives: primMap };
  const schemaMap = (jsonBootstrap as { typesByName: unknown }).typesByName;
  const modName = { value: "hydra.packaging.Module" };
  const modType = { tag: "variable", value: modName };
  const fromJson = (jsonDecode as { fromJson: (s: unknown) => (n: unknown) => (t: unknown) => (v: unknown) => { tag: "left"; value: unknown } | { tag: "right"; value: unknown } }).fromJson;
  const modDecoder = (decodePackaging as { module_: (g: unknown) => (t: unknown) => { tag: "left"; value: unknown } | { tag: "right"; value: Module } }).module_;
  const modulesByPath: Array<{ path: string; module: Module; isTest: boolean }> = [];

  for (const { path, json, isTest } of jsonFiles) {
    try {
      const hydraJson = toHydraJson(json);
      const termResult = fromJson(schemaMap)(modName)(modType)(hydraJson);
      if (termResult.tag === "left") {
        console.error(`  warning: JSON-to-Term decode failed for ${path}: ${JSON.stringify(termResult.value).slice(0, 200)}`);
        continue;
      }
      const modResult = modDecoder(bsGraph)(termResult.value);
      if (modResult.tag === "left") {
        console.error(`  warning: Term-to-Module decode failed for ${path}: ${JSON.stringify(modResult.value).slice(0, 200)}`);
        continue;
      }
      modulesByPath.push({ path, module: modResult.value, isTest });
    } catch (e) {
      console.error(`  warning: exception decoding ${path}: ${(e as Error).message}`);
    }
  }
  console.log(`  Decoded ${modulesByPath.length} modules`);

  // Step 4: build a schema-populated graph from all decoded modules.
  // Coders for richer targets (Python, Java, ...) need the graph to know
  // about every nominal type referenced in any module — e.g.
  // `hydra.core.Term`, `hydra.core.Type` — so they can resolve field
  // types when emitting class/record definitions. Without this, the
  // Python coder fails with `noSuchBinding: hydra.core.Type` etc.
  // Mirrors heads/python's modules_to_graph(bs_graph, kernel_modules).
  const allModules = modulesByPath.map((m) => m.module);
  const modulesToGraph = (codegen as { modulesToGraph: (g: unknown) => (universe: unknown[]) => (mods: unknown[]) => unknown }).modulesToGraph;
  const populatedGraph = modulesToGraph(bsGraph)(allModules)(allModules);

  // Step 5: for each module, call moduleTo(module)(defs)(cx)(graph),
  // and write to src/main/<target>/ or src/test/<target>/ depending on
  // which JSON tree the module came from.
  const cx = lexical.emptyContext;
  const outMain = join(opts.output, "src/main", opts.target);
  const outTest = join(opts.output, "src/test", opts.target);
  let mainFileCount = 0;
  let testFileCount = 0;
  for (const { path, module, isTest } of modulesByPath) {
    try {
      const defs = (module as { definitions: unknown[] }).definitions;
      const result = moduleTo(module)(defs)(cx)(populatedGraph);
      if (result.tag === "left") {
        console.error(`  warning: codegen failed for ${path}: ${JSON.stringify(result.value).slice(0, 200)}`);
        continue;
      }
      const fileMap = result.value as Map<string, string>;
      const entries = maps.toList(fileMap as never);
      const targetBase = isTest ? outTest : outMain;
      for (const [relPath, content] of entries) {
        const outPath = join(targetBase, relPath as string);
        mkdirSync(dirname(outPath), { recursive: true });
        writeFileSync(outPath, content as string);
        if (isTest) testFileCount++; else mainFileCount++;
      }
    } catch (e) {
      const err = e as Error;
      console.error(`  warning: exception in codegen for ${path}: ${err.message}`);
      if (process.env.HYDRA_DEBUG) {
        console.error(`    stack: ${err.stack?.split("\n").slice(0, 6).join("\n      ")}`);
      }
    }
  }
  const fileCount = mainFileCount + testFileCount;

  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  console.log("");
  console.log("==========================================");
  console.log(`Done: ${mainFileCount} main + ${testFileCount} test files (${fileCount} total)`);
  console.log(`  Main: ${outMain}`);
  if (testFileCount > 0) console.log(`  Test: ${outTest}`);
  console.log(`Elapsed: ${elapsed}s`);
  console.log("==========================================");

};

main().catch((e) => { console.error(e); process.exit(1); });
