// Note: this is an automatically generated file. Do not edit.

/**
 * A model for unit testing
 */



import * as Core from "./core.js";

export type Tag = string & { readonly __brand: "Tag" };

export type TestCase =
  | { readonly tag: "universal"; readonly value: UniversalTestCase };

export interface TestCaseWithMetadata {
  readonly name: string;
  readonly case: TestCase;
  readonly description: string | null;
  readonly tags: ReadonlyArray<Tag>;
}

export interface TestGroup {
  readonly name: string;
  readonly description: string | null;
  readonly subgroups: ReadonlyArray<TestGroup>;
  readonly cases: ReadonlyArray<TestCaseWithMetadata>;
}

export interface UniversalTestCase {
  readonly actual: string;
  readonly expected: string;
}
