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

// Resolve dist-json-root from the kernel JSON dir. The CLI takes
// --json-dir <root>/hydra-kernel/src/main/json; the root is two levels
// up from "hydra-kernel". Used to locate sibling packages' JSON trees.
const distJsonRootFromKernelDir = (kernelDir: string): string | null => {
  // Match …/hydra-kernel/src/main/json
  const m = kernelDir.match(/^(.*)\/hydra-kernel\/src\/main\/json\/?$/);
  return m ? m[1]! : null;
};

// Packages whose main JSON tree we always co-load when target=haskell or
// when a target-specific coder package needs cross-package type resolution
// (e.g. hydra-typescript depends on hydra-haskell.syntax for ts-syntax
// reuse). Mirrors the dependency order in `heads/haskell/src/exec/
// bootstrap-from-json/Main.hs` Step 1: baseline packages first
// (hydra-kernel always loaded; hydra-haskell as baseline coder package).
const BASELINE_EXTRA_PACKAGES: readonly string[] = ["hydra-haskell"];

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

// Coder dispatch: map --target lang to (coder, language, codegen flags) loaded
// from `dist/typescript/hydra-<lang>/src/main/typescript/hydra/<lang>/{coder,language}.js`.
// importPath is resolved from `dist/typescript/hydra-kernel/src/main/typescript/hydra/bootstrap.ts`,
// so going up 5 levels lands at `dist/typescript/`, then dispatching into each sibling package.
// Flags mirror the Haskell bootstrap-from-json switch (Main.hs): doInfer/doExpand/doHoistCase/doHoistPoly.
// `lispDialect` and `lispFileExt` are set for the four Lisp dialects, which
// share a single hydra-lisp coder package (moduleToLisp takes a dialect arg).
const CODER_DISPATCH: Record<string, {
  coderPath: string; coderExport: string;
  languagePath: string; languageExport: string;
  doInfer: boolean; doExpand: boolean; doHoistCase: boolean; doHoistPoly: boolean;
  lispDialect?: string; lispFileExt?: string;
}> = {
  typescript: {
    coderPath:    "../../../../../hydra-typescript/src/main/typescript/hydra/typeScript/coder.js",
    coderExport:  "moduleToTypeScript",
    languagePath: "../../../../../hydra-typescript/src/main/typescript/hydra/typeScript/language.js",
    languageExport: "typeScriptLanguage",
    doInfer: false, doExpand: true, doHoistCase: true, doHoistPoly: false,
  },
  python:     {
    coderPath:    "../../../../../hydra-python/src/main/typescript/hydra/python/coder.js",
    coderExport:  "moduleToPython",
    languagePath: "../../../../../hydra-python/src/main/typescript/hydra/python/language.js",
    languageExport: "pythonLanguage",
    doInfer: false, doExpand: true, doHoistCase: true, doHoistPoly: false,
  },
  java:       {
    coderPath:    "../../../../../hydra-java/src/main/typescript/hydra/java/coder.js",
    coderExport:  "moduleToJava",
    languagePath: "../../../../../hydra-java/src/main/typescript/hydra/java/language.js",
    languageExport: "javaLanguage",
    doInfer: false, doExpand: true, doHoistCase: false, doHoistPoly: true,
  },
  haskell:    {
    coderPath:    "../../../../../hydra-haskell/src/main/typescript/hydra/haskell/coder.js",
    coderExport:  "moduleToHaskell",
    languagePath: "../../../../../hydra-haskell/src/main/typescript/hydra/haskell/language.js",
    languageExport: "haskellLanguage",
    doInfer: false, doExpand: false, doHoistCase: false, doHoistPoly: false,
  },
  scala:      {
    coderPath:    "../../../../../hydra-scala/src/main/typescript/hydra/scala/coder.js",
    coderExport:  "moduleToScala",
    languagePath: "../../../../../hydra-scala/src/main/typescript/hydra/scala/language.js",
    languageExport: "scalaLanguage",
    // doInfer=false because the kernel JSON already carries TypeSchemes;
    // running full inference in TS-host (where it does in the Haskell
    // host) trips V8's stack ceiling on deeply-polymorphic modules and
    // takes hours. The other targets that use the JSON path also skip it.
    doInfer: false, doExpand: true, doHoistCase: false, doHoistPoly: false,
  },
  // The four Lisp dialects share a single coder (moduleToLisp) and language
  // (lispLanguage). The dialect arg selects per-target syntax (constructor
  // prefix, lambda keyword, etc.); fileExt selects the output extension.
  // Mirrors heads/haskell/.../ExtGeneration.hs writeClojure/writeScheme/etc.
  clojure: {
    coderPath:    "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/coder.js",
    coderExport:  "moduleToLisp",
    languagePath: "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/language.js",
    languageExport: "lispLanguage",
    // doInfer: false because the kernel JSON already carries per-binding
    // TypeSchemes; running full inference is redundant in TS-host and trips
    // the V8 stack ceiling (the same issue as TS->Java). Haskell host uses
    // doInfer=True because its bootstrap path doesn't always carry schemes,
    // but the JSON path always does. Verified empirically: TS->Clojure with
    // doInfer=true ran 10+ hours and emitted only 7 .clj files; with
    // doInfer=false it completes in minutes (see #126 plan).
    doInfer: false, doExpand: false, doHoistCase: false, doHoistPoly: false,
    lispDialect: "clojure", lispFileExt: "clj",
  },
  scheme: {
    coderPath:    "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/coder.js",
    coderExport:  "moduleToLisp",
    languagePath: "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/language.js",
    languageExport: "lispLanguage",
    // doInfer: false because the kernel JSON already carries per-binding
    // TypeSchemes; running full inference is redundant in TS-host and trips
    // the V8 stack ceiling (the same issue as TS->Java). Haskell host uses
    // doInfer=True because its bootstrap path doesn't always carry schemes,
    // but the JSON path always does. Verified empirically: TS->Clojure with
    // doInfer=true ran 10+ hours and emitted only 7 .clj files; with
    // doInfer=false it completes in minutes (see #126 plan).
    doInfer: false, doExpand: false, doHoistCase: false, doHoistPoly: false,
    lispDialect: "scheme", lispFileExt: "scm",
  },
  "common-lisp": {
    coderPath:    "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/coder.js",
    coderExport:  "moduleToLisp",
    languagePath: "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/language.js",
    languageExport: "lispLanguage",
    // doInfer: false because the kernel JSON already carries per-binding
    // TypeSchemes; running full inference is redundant in TS-host and trips
    // the V8 stack ceiling (the same issue as TS->Java). Haskell host uses
    // doInfer=True because its bootstrap path doesn't always carry schemes,
    // but the JSON path always does. Verified empirically: TS->Clojure with
    // doInfer=true ran 10+ hours and emitted only 7 .clj files; with
    // doInfer=false it completes in minutes (see #126 plan).
    doInfer: false, doExpand: false, doHoistCase: false, doHoistPoly: false,
    lispDialect: "commonLisp", lispFileExt: "lisp",
  },
  "emacs-lisp": {
    coderPath:    "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/coder.js",
    coderExport:  "moduleToLisp",
    languagePath: "../../../../../hydra-lisp/src/main/typescript/hydra/lisp/language.js",
    languageExport: "lispLanguage",
    // doInfer: false because the kernel JSON already carries per-binding
    // TypeSchemes; running full inference is redundant in TS-host and trips
    // the V8 stack ceiling (the same issue as TS->Java). Haskell host uses
    // doInfer=True because its bootstrap path doesn't always carry schemes,
    // but the JSON path always does. Verified empirically: TS->Clojure with
    // doInfer=true ran 10+ hours and emitted only 7 .clj files; with
    // doInfer=false it completes in minutes (see #126 plan).
    doInfer: false, doExpand: false, doHoistCase: false, doHoistPoly: false,
    lispDialect: "emacsLisp", lispFileExt: "el",
  },
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

  // Step 1: load the target-language coder + language record.
  let coderMod: Record<string, unknown>;
  let langMod: Record<string, unknown>;
  try {
    coderMod = await import(dispatch.coderPath);
    langMod = await import(dispatch.languagePath);
  } catch (e) {
    console.error(`error: failed to load coder/language for target=${opts.target}: ${(e as Error).message}`);
    console.error(`       (target=${opts.target} requires the corresponding hydra-${opts.target} package to be assembled into dist/typescript/)`);
    process.exit(1);
  }
  const rawModuleTo = coderMod[dispatch.coderExport] as (...args: unknown[]) => { tag: "left"; value: unknown } | { tag: "right"; value: unknown };
  const language = langMod[dispatch.languageExport];
  if (typeof rawModuleTo !== "function") {
    console.error(`error: ${dispatch.coderExport} not found in ${dispatch.coderPath}`);
    process.exit(1);
  }
  if (!language) {
    console.error(`error: ${dispatch.languageExport} not found in ${dispatch.languagePath}`);
    process.exit(1);
  }

  // For Lisp dialects, wrap the raw coder so that:
  //   1. The dialect tag is prepended to the args (moduleToLisp(dialect, mod, ...))
  //   2. The returned LispProgram is post-processed via programToExpr → parenthesize →
  //      printExpr to a single string, then keyed under the module's per-dialect file
  //      path. Mirrors heads/haskell/.../ExtGeneration.hs moduleToLispDialect.
  let moduleTo: ((m: unknown, defs: unknown[], cx: unknown, g: unknown) => { tag: "left"; value: unknown } | { tag: "right"; value: Map<string, string> });
  if (dispatch.lispDialect) {
    const dialectArg = { tag: dispatch.lispDialect } as const;
    const fileExt = dispatch.lispFileExt!;
    // clojure uses Camel case for namespace components; the other three use lower-snake
    // (matches heads/haskell/.../ExtGeneration.hs moduleToLispDialect).
    const caseConvention = dispatch.lispDialect === "clojure"
      ? { tag: "camel" }
      : { tag: "lowerSnake" };
    const lispSerde = await import("../../../../../hydra-lisp/src/main/typescript/hydra/lisp/serde.js") as { programToExpr: (p: unknown) => unknown };
    const serialization = await import("./serialization.js") as { parenthesize: (e: unknown) => unknown; printExpr: (e: unknown) => string };
    const namesMod = await import("./names.js") as { moduleNameToFilePath: (cc: unknown, ext: unknown, ns: unknown) => string };
    moduleTo = (mod, defs, cx, g) => {
      const result = rawModuleTo(dialectArg, mod, defs, cx, g);
      if (result.tag === "left") return result;
      const program = result.value;
      const exprStr = serialization.printExpr(serialization.parenthesize(lispSerde.programToExpr(program)));
      const modName = (mod as { name?: { value?: string } } | null)?.name;
      const filePath = namesMod.moduleNameToFilePath(caseConvention, { value: fileExt }, modName);
      const out = new Map<string, string>();
      out.set(filePath, exprStr);
      return { tag: "right" as const, value: out };
    };
  } else {
    moduleTo = rawModuleTo as typeof moduleTo;
  }

  // Step 2: collect JSON files. The kernel JSON layout is per-namespace:
  // hydra/core.json, hydra/lib/maps.json, etc. — under src/main/json/.
  // Test modules live in a parallel src/test/json/ tree.
  // Caller passes --json-dir pointing at the main tree; we also look for
  // a sibling test tree when --include-tests is set.
  // Load kernel JSON (always) plus any baseline-extra packages whose
  // syntax/language/coder modules are imported by the chosen target's
  // hand-written runtime. For target=haskell the generated DSL wrappers
  // import `Hydra.Haskell.Syntax`, which lives in hydra-haskell's JSON
  // tree, not hydra-kernel's; without loading it, the bootstrap output
  // is missing files that the hand-written `Hydra.Dsl.Haskell.Syntax`
  // imports. Mirrors `bootstrap-from-json/Main.hs` Step 1 — baseline
  // packages always loaded into the universe; emission is filtered to
  // only the packages the target needs (see step 4 below).
  const distJsonRoot = distJsonRootFromKernelDir(opts.jsonDir);
  const mainJsonFiles: Array<{ path: string; json: unknown; pkg: string }> =
    readJsonTree(opts.jsonDir).map((f) => ({ ...f, pkg: "hydra-kernel" }));
  if (distJsonRoot) {
    for (const pkg of BASELINE_EXTRA_PACKAGES) {
      const pkgDir = join(distJsonRoot, pkg, "src", "main", "json");
      if (existsSync(pkgDir)) {
        const extra = readJsonTree(pkgDir).map((f) => ({ ...f, pkg }));
        mainJsonFiles.push(...extra);
        console.log(`  Loaded ${extra.length} additional JSON files from ${pkg}`);
      }
    }
  }
  let testJsonFiles: Array<{ path: string; json: unknown; pkg: string }> = [];
  if (opts.includeTests) {
    // jsonDir is .../src/main/json; the test tree is .../src/test/json
    const testJsonDir = opts.jsonDir.replace(/src\/main\/json$/, "src/test/json");
    if (testJsonDir !== opts.jsonDir && existsSync(testJsonDir)) {
      testJsonFiles = readJsonTree(testJsonDir).map((f) => ({ ...f, pkg: "hydra-kernel" }));
      console.log(`  Loaded ${mainJsonFiles.length} main + ${testJsonFiles.length} test JSON files`);
    } else {
      console.log(`  Loaded ${mainJsonFiles.length} JSON files (no test tree at ${testJsonDir})`);
    }
  } else {
    console.log(`  Loaded ${mainJsonFiles.length} JSON files`);
  }
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
  const stdPrims = (libraries as { standardPrimitives: () => ReadonlyArray<{ definition: { name: { value: string } } }> }).standardPrimitives();
  const primMap = (maps as { fromList: (entries: ReadonlyArray<readonly [unknown, unknown]>) => unknown }).fromList(
    stdPrims.map((p) => [p.definition.name, p] as const)
  );
  const bsGraph = { ...(lexical.emptyGraph as object), primitives: primMap };
  const schemaMap = (jsonBootstrap as { typesByName: unknown }).typesByName;
  const modName = { value: "hydra.packaging.Module" };
  const modType = { tag: "variable", value: modName };
  const fromJson = (jsonDecode as { fromJson: (s: unknown, n: unknown, t: unknown, v: unknown) => { tag: "left"; value: unknown } | { tag: "right"; value: unknown } }).fromJson;
  const modDecoder = (decodePackaging as { module_: (g: unknown, t: unknown) => { tag: "left"; value: unknown } | { tag: "right"; value: Module } }).module_;
  const modulesByPath: Array<{ path: string; module: Module; isTest: boolean; pkg: string }> = [];

  for (const { path, json, isTest, pkg } of jsonFiles) {
    try {
      const hydraJson = toHydraJson(json);
      const termResult = fromJson(schemaMap, modName, modType, hydraJson);
      if (termResult.tag === "left") {
        console.error(`  warning: JSON-to-Term decode failed for ${path}: ${JSON.stringify(termResult.value).slice(0, 200)}`);
        continue;
      }
      const modResult = modDecoder(bsGraph, termResult.value);
      if (modResult.tag === "left") {
        console.error(`  warning: Term-to-Module decode failed for ${path}: ${JSON.stringify(modResult.value).slice(0, 200)}`);
        continue;
      }
      modulesByPath.push({ path, module: modResult.value, isTest, pkg });
    } catch (e) {
      console.error(`  warning: exception decoding ${path}: ${(e as Error).message}`);
    }
  }
  console.log(`  Decoded ${modulesByPath.length} modules`);

  // Step 4: drive code generation through `codegen.generateSourceFiles`,
  // which applies adapt + eta-expand + hoist before calling the coder.
  // Calling moduleTo directly skips eta-expansion, leaving partial
  // applications like `capitalize = mapFirstLetter toUpper` emitted as
  // `const capitalize = mapFirstLetter(toUpper);` which fails under the
  // TS coder's flat-call ABI.
  const allModules = modulesByPath.map((m) => m.module);
  const cx = lexical.emptyInferenceContext;
  const outMain = join(opts.output, "src/main", opts.target);
  const outTest = join(opts.output, "src/test", opts.target);

  // `generateSourceFiles` calls printDefinitions flat (mod, defs, cx, g),
  // so the wrapper here just forwards. The result entries are
  // [filePath, content] tuples from `maps.toList`.
  const printDefinitions = (m: unknown, defs: unknown, c: unknown, g: unknown) =>
    moduleTo(m, defs as unknown[], c, g);

  const writeOne = (isTest: boolean, mods: unknown[]): number => {
    if (mods.length === 0) return 0;
    const generateSourceFiles = (codegen as { generateSourceFiles: (...args: unknown[]) => { tag: "left"; value: unknown } | { tag: "right"; value: ReadonlyArray<readonly [string, string]> } }).generateSourceFiles;
    const targetBase = isTest ? outTest : outMain;
    let count = 0;
    // Process modules one at a time. Per-module isolation means a
    // stack overflow on one module (the kernel TS type-checker
    // recurses through deeply-nested Hydra terms; see analysis.ts
    // bootstrap patch) doesn't abort the whole codegen — we record
    // a warning and continue.
    for (const m of mods) {
      const modName = (m as { name?: { value?: string } } | null)?.name?.value ?? "<unknown>";
      try {
        const result = generateSourceFiles(
          printDefinitions, language,
          dispatch.doInfer, dispatch.doExpand, dispatch.doHoistCase, dispatch.doHoistPoly,
          bsGraph, allModules, [m], cx);
        if (result.tag === "left") {
          console.error(`  warning: codegen failed for module ${modName}: ${JSON.stringify(result.value).slice(0, 200)}`);
          continue;
        }
        for (const entry of result.value) {
          const [relPath, content] = entry;
          const outPath = join(targetBase, relPath);
          mkdirSync(dirname(outPath), { recursive: true });
          writeFileSync(outPath, content);
          count++;
        }
      } catch (e) {
        const err = e as Error;
        console.error(`  warning: ${isTest ? "test" : "main"} codegen exception on module ${modName}: ${err.message}`);
        if (process.env.HYDRA_DEBUG) console.error(err.stack);
      }
    }
    return count;
  };

  // Filter emission to packages the target consumes. Other baseline-extra
  // packages were loaded only into the universe (for type resolution); we
  // do not emit them. Target=haskell additionally needs hydra-haskell
  // emitted because the Haskell host's hand-written DSL wrappers
  // (`Hydra.Dsl.Haskell.*`) import the corresponding generated modules
  // (`Hydra.Haskell.*`).
  const emitPkgs = new Set<string>(["hydra-kernel"]);
  if (opts.target === "haskell") emitPkgs.add("hydra-haskell");
  const mainMods = modulesByPath.filter((m) => !m.isTest && emitPkgs.has(m.pkg)).map((m) => m.module);
  const testMods = modulesByPath.filter((m) => m.isTest && emitPkgs.has(m.pkg)).map((m) => m.module);
  const mainFileCount = writeOne(false, mainMods);
  const testFileCount = writeOne(true, testMods);

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
