/**
 * Test infrastructure types for Hydra-TypeScript.
 *
 * Mirrors the generated hydra.testing module, providing the types needed
 * by the test runner to walk the generated TestGroup tree.
 */

export type TestGroup = {
  readonly name: string;
  readonly description: string | undefined;
  readonly subgroups: ReadonlyArray<TestGroup>;
  readonly cases: ReadonlyArray<TestCaseWithMetadata> | undefined;
};

export type TestCaseWithMetadata = {
  readonly name: string;
  readonly description: string | undefined;
  readonly case: TestCase;
  readonly tags: ReadonlyArray<Tag>;
};

export type TestCase =
  | { readonly tag: "universal"; readonly value: UniversalTestCase };

export type UniversalTestCase = {
  readonly actual: string;
  readonly expected: string;
};

export type Tag =
  | { readonly tag: "disabled" }
  | { readonly tag: "custom"; readonly value: string };

/**
 * Walks a TestGroup tree, collecting all test cases with their full path prefix.
 * Returns an array of [path, testCase] pairs suitable for vitest test generation.
 */
export function collectTestCases(
  group: TestGroup,
  prefix: string = "",
): ReadonlyArray<readonly [string, TestCaseWithMetadata]> {
  const path = prefix ? `${prefix} > ${group.name}` : group.name;
  const result: Array<readonly [string, TestCaseWithMetadata]> = [];

  if (group.cases) {
    for (const tc of group.cases) {
      result.push([path, tc] as const);
    }
  }

  for (const sub of group.subgroups) {
    result.push(...collectTestCases(sub, path));
  }

  return result;
}

/**
 * Runs a universal test case (string equality assertion).
 * Throws an Error if the assertion fails (for use with vitest's expect).
 */
export function runUniversalTestCase(tc: UniversalTestCase): void {
  if (tc.actual !== tc.expected) {
    throw new Error(
      `Test assertion failed:\n  Expected: ${JSON.stringify(tc.expected)}\n  Actual:   ${JSON.stringify(tc.actual)}`,
    );
  }
}
