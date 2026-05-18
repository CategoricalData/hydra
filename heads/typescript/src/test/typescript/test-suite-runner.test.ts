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

import { allTests } from "../../../../../dist/typescript/hydra-kernel/src/test/typescript/hydra/test/testSuite.ts";

interface TestCaseUniversal {
  readonly actual: (_: void) => string;
  readonly expected: (_: void) => string;
}

interface TestCase {
  readonly tag: "universal";
  readonly value: TestCaseUniversal;
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
    if (tc.tag !== "universal") {
      throw new Error(`unknown test-case variant: ${(tc as { tag: string }).tag}`);
    }
    const u = tc.value;
    expect(u.actual(undefined as unknown as void)).toBe(u.expected(undefined as unknown as void));
  });
}

function runGroup(g: TestGroup): void {
  describe(g.name, () => {
    for (const sub of g.subgroups) runGroup(sub);
    for (const c of g.cases) runCase(c);
  });
}

runGroup(allTests as TestGroup);
