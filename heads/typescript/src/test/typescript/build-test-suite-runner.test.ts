// Vitest runner for hydra-build's own test suite.
//
// Package-scoped counterpart to test-suite-runner.test.ts (hydra-kernel); part of #547's
// per-package test aggregation. Walks the generated `allTests` tree (a `TestGroup` rooted
// in `dist/typescript/hydra-build/src/test/typescript/hydra/test/build/testSuite.js`) the
// same way the kernel runner does — vitest auto-discovers this file alongside it, so no
// composition/registration step is needed.
//
// All of hydra-build's test cases are universal (pure string comparison); no
// HYDRA_DEFAULT_IMPLS handling is needed here (unlike the kernel runner, hydra-build's
// tests don't evaluate against a primitive-backed Graph).

import { describe, expect, it } from "vitest";

import { allTests } from "../../../../../dist/typescript/hydra-build/src/test/typescript/hydra/test/build/testSuite.js";

interface TestCaseUniversal {
  readonly actual: (_: void) => string;
  readonly expected: (_: void) => string;
}

type TestCase =
  | { readonly tag: "universal"; readonly value: TestCaseUniversal };

interface Tag {
  readonly value: string;
}

interface TestCaseWithMetadata {
  readonly name: string;
  readonly case_: TestCase;
  readonly description: string | undefined;
  readonly tags: ReadonlyArray<Tag>;
}

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
  if (!hasAnyCases(g)) return;
  describe(g.name, () => {
    for (const sub of g.subgroups) runGroup(sub);
    for (const c of g.cases) runCase(c);
  });
}

runGroup(allTests as TestGroup);
