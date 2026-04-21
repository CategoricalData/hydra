// Note: this is an automatically generated file. Do not edit.

/**
 * Execution context for tracing and diagnostics
 */



import * as Core from "./core.js";

export interface Context {
  readonly trace: ReadonlyArray<string>;
  readonly messages: ReadonlyArray<string>;
  readonly other: ReadonlyMap<Core.Name, Core.Term>;
}

export interface InContext<e> {
  readonly object: e;
  readonly context: Context;
}
