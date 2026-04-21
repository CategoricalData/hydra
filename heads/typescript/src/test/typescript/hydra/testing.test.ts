import { describe, it, expect } from "vitest";
import {
  collectTestCases,
  runUniversalTestCase,
} from "../../../main/typescript/hydra/testing.js";
import type { TestGroup, TestCaseWithMetadata } from "../../../main/typescript/hydra/testing.js";

describe("testing infrastructure", () => {
  const sampleGroup: TestGroup = {
    name: "root",
    description: "test suite root",
    subgroups: [
      {
        name: "math",
        description: "math tests",
        subgroups: [],
        cases: [
          {
            name: "add",
            description: "addition",
            case: { tag: "universal", value: { actual: "5", expected: "5" } },
            tags: [],
          },
          {
            name: "sub",
            description: "subtraction",
            case: { tag: "universal", value: { actual: "3", expected: "3" } },
            tags: [],
          },
        ],
      },
      {
        name: "strings",
        description: "string tests",
        subgroups: [],
        cases: [
          {
            name: "cat",
            description: "concatenation",
            case: { tag: "universal", value: { actual: "ab", expected: "ab" } },
            tags: [],
          },
        ],
      },
    ],
    cases: undefined,
  };

  it("collectTestCases walks the tree", () => {
    const cases = collectTestCases(sampleGroup);
    expect(cases.length).toBe(3);
    expect(cases[0]![0]).toBe("root > math");
    expect(cases[0]![1].name).toBe("add");
    expect(cases[2]![0]).toBe("root > strings");
    expect(cases[2]![1].name).toBe("cat");
  });

  it("runUniversalTestCase passes on match", () => {
    expect(() => runUniversalTestCase({ actual: "42", expected: "42" })).not.toThrow();
  });

  it("runUniversalTestCase fails on mismatch", () => {
    expect(() => runUniversalTestCase({ actual: "41", expected: "42" })).toThrow(
      "Test assertion failed",
    );
  });

  it("handles empty groups", () => {
    const empty: TestGroup = {
      name: "empty",
      description: undefined,
      subgroups: [],
      cases: [],
    };
    expect(collectTestCases(empty)).toEqual([]);
  });

  it("handles deeply nested groups", () => {
    const deep: TestGroup = {
      name: "a",
      description: undefined,
      subgroups: [
        {
          name: "b",
          description: undefined,
          subgroups: [
            {
              name: "c",
              description: undefined,
              subgroups: [],
              cases: [
                {
                  name: "test1",
                  description: undefined,
                  case: { tag: "universal", value: { actual: "x", expected: "x" } },
                  tags: [],
                },
              ],
            },
          ],
          cases: undefined,
        },
      ],
      cases: undefined,
    };
    const cases = collectTestCases(deep);
    expect(cases.length).toBe(1);
    expect(cases[0]![0]).toBe("a > b > c");
  });
});
