// Vitest runner for the Hydra common test suite.
//
// Walks the generated `allTests` tree (a `TestGroup` rooted in
// `dist/typescript/hydra-kernel/src/test/typescript/hydra/test/testSuite.ts`)
// and registers one Vitest `describe` per `TestGroup`, one `it` per
// `TestCaseWithMetadata`. Each universal case calls `actual()` and
// `expected()` and asserts string-equality.
//
// The runner is the TypeScript-side equivalent of
// `heads/python/src/test/python/test_suite_runner.py`.

import { describe, expect, it } from "vitest";
import { mkdirSync, rmSync } from "node:fs";

import { allTests } from "../../../../../dist/typescript/hydra-kernel/src/test/typescript/hydra/test/testSuite.js";

interface TestCaseUniversal {
  readonly actual: (_: void) => string;
  readonly expected: (_: void) => string;
}

// An effectful test case: actual/expected are unit-thunks. In TypeScript the
// effect type is transparent (effect<t> = t), so forcing the actual thunk *is*
// running the effect (mirroring the Python runner). File-I/O cases write under
// the canonical temp dir prepared before each case.
interface TestCaseEffectful {
  readonly actual: (_: void) => string;
  readonly expected: (_: void) => string;
}

type TestCase =
  | { readonly tag: "universal"; readonly value: TestCaseUniversal }
  | { readonly tag: "effectful"; readonly value: TestCaseEffectful };

// Canonical root directory for effectful (file I/O) test cases. Must match the
// testDir constant in Hydra.Sources.Test.Lib.Files and the EFFECTFUL_TEST_DIR
// in the Python runner / effectfulTestDir in the Haskell runner. Hard-coded
// *nix path for now (#494).
const EFFECTFUL_TEST_DIR = "/tmp/hydra-testing";

// Prepare a guaranteed-empty canonical temp directory before an effectful test
// case. Mirrors prepare_effectful_temp_dir in the Python runner.
function prepareEffectfulTempDir(): void {
  rmSync(EFFECTFUL_TEST_DIR, { recursive: true, force: true });
  mkdirSync(EFFECTFUL_TEST_DIR, { recursive: true });
}

interface Tag {
  readonly value: string;
}

interface TestCaseWithMetadata {
  readonly name: string;
  readonly case_: TestCase;
  readonly description: string | undefined;
  readonly tags: ReadonlyArray<Tag>;
}

// Tags that signal the test should be skipped on this head. Matches
// Python's `is_disabled` semantics. `disabledForMinimalInference` is
// tested in passing because the kernel here is the full inference, not
// the minimal variant.
const SKIP_TAGS = new Set(["disabled"]);

const shouldSkip = (c: TestCaseWithMetadata): boolean => {
  for (const t of c.tags) {
    if (t && typeof t === "object" && "value" in t && SKIP_TAGS.has((t as Tag).value)) {
      return true;
    }
  }
  return false;
};

interface TestGroup {
  readonly name: string;
  readonly description: string | undefined;
  readonly subgroups: ReadonlyArray<TestGroup>;
  readonly cases: ReadonlyArray<TestCaseWithMetadata>;
}

function runCase(c: TestCaseWithMetadata): void {
  if (shouldSkip(c)) {
    it.skip(c.name, () => { /* skipped: tagged as disabled */ });
    return;
  }
  it(c.name, () => {
    const tc = c.case_;
    const unit = undefined as unknown as void;
    if (tc.tag === "universal") {
      const u = tc.value;
      expect(u.actual(unit)).toBe(u.expected(unit));
    } else if (tc.tag === "effectful") {
      // For #494: force the actual thunk (running the effect) after preparing
      // the canonical temp dir; compare to the expected string. Mirrors the
      // Python runner's run_effectful.
      prepareEffectfulTempDir();
      const e = tc.value;
      expect(e.actual(unit)).toBe(e.expected(unit));
    } else {
      throw new Error(`unknown test-case variant: ${(tc as { tag: string }).tag}`);
    }
  });
}

function hasAnyCases(g: TestGroup): boolean {
  if (g.cases.length > 0) return true;
  for (const sub of g.subgroups) if (hasAnyCases(sub)) return true;
  return false;
}

function runGroup(g: TestGroup): void {
  // Vitest fails a describe block with no tests. Skip placeholder groups
  // that have no transitive cases (e.g. checking > Failures, which is
  // declared in the common test suite but has no cases yet).
  if (!hasAnyCases(g)) return;
  describe(g.name, () => {
    for (const sub of g.subgroups) runGroup(sub);
    for (const c of g.cases) runCase(c);
  });
}

runGroup(allTests as TestGroup);
