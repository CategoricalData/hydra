// Hand-written test environment for TypeScript.
//
// Provides the test graph and test context referenced by the generated
// hydra.test.test_graph module. Mirrors the role of
// heads/python/src/test/python/hydra/test/test_env.py and the equivalent
// hand-written modules in the Java, Scala and Lisp heads.
//
// The Hydra DSL declares:
//   hydra.test.testEnv.testContext :: InferenceContext
//   hydra.test.testEnv.testGraph   :: Map Name Type -> Map Name Term -> Graph
//
// This file MUST expose those two FQNs at the same arity:
//   - testContext: an InferenceContext value (not a function)
//   - testGraph(testTypes)(testTerms): a curried function returning a Graph
//
// The TypeScript coder filters hydra.test.testEnv from emitted output (via
// `testSkipEmitNamespaces` in Hydra.Sources.Test.All); this file is the
// TypeScript runtime counterpart that the generated test_graph.ts resolves
// against at import time.

// `InferenceContext`, `Graph`, `Primitive`, `Name`, `Term`, `Type`, and
// `TypeScheme` are imported from the GENERATED kernel modules. At src/
// authoring time these sibling files don't exist (they only land in
// dist after the sync runs), so the source-tree tsc check (tsconfig.json,
// which excludes the src/test tree from compilation) skips this file.
// At test time the file is copied into dist/.../src/test/typescript/hydra/test/
// where the imports resolve normally as `../../../../main/typescript/hydra/<mod>.js`.
import type { InferenceContext } from "../../../../main/typescript/hydra/typing.js";
import type { Graph, Primitive } from "../../../../main/typescript/hydra/graph.js";
import type { Name, Term, Type, TypeScheme } from "../../../../main/typescript/hydra/core.js";
import * as maps from "../../../../main/typescript/hydra/lib/maps.js";
import * as sets from "../../../../main/typescript/hydra/lib/sets.js";

import { standardPrimitives } from "../../../../main/typescript/hydra/lib/libraries.js";
import { loadAll } from "./jsonBindings.js";

// An empty InferenceContext value. No side effects.
export const testContext: InferenceContext = {
  freshTypeVariableCount: 0,
  trace: [],
};

// Build the primitives map via lib_maps.fromList so the runtime's
// value-equality keying handles wrapped Name lookups consistently with
// how the generated kernel constructs all other Map<Name, _> values.
const buildPrimitivesMap = (): ReadonlyMap<Name, Primitive> =>
  maps.fromList(standardPrimitives().map((p) => [p.definition.name, p] as const));

// The primitives map is computed once and shared across all calls
// (cheap: ~50 entries, constructed from a static list).
const _cachedPrimitives = buildPrimitivesMap();

// Kernel bindings + types loaded from dist/json/hydra-kernel/. These
// are the evaluator-essential namespaces (hydra.annotations, etc.) plus
// the type-bearing modules (hydra.core, hydra.graph, etc.) that
// kernel functions consult via the schema graph for nominal type
// resolution (e.g. `cases _Term`).
//
// Walk a Type, collecting all forall-bound variables along the
// outermost spine. Returns them in declaration order. Mirrors Python's
// f_type_to_type_scheme — kernel inference needs the type variables
// promoted from the type body up to the scheme level.
const collectForallVars = (t: Type): { vars: Name[]; body: Type } => {
  const vars: Name[] = [];
  let cur: { tag: string; value?: { parameter?: Name; body?: Type } } = t as never;
  while (cur && typeof cur === "object" && cur.tag === "annotated") {
    cur = (cur.value as { body?: Type })?.body as never;
  }
  while (cur && typeof cur === "object" && cur.tag === "forall" && cur.value) {
    if (cur.value.parameter) vars.push(cur.value.parameter);
    cur = cur.value.body as never;
    while (cur && typeof cur === "object" && cur.tag === "annotated") {
      cur = (cur.value as { body?: Type })?.body as never;
    }
  }
  return { vars, body: cur as unknown as Type };
};

// Loaded eagerly at module init so the cost is paid once per process.
const _kernelLoaded = loadAll();
const _cachedKernelTerms: ReadonlyMap<Name, Term> = maps.fromList(_kernelLoaded.terms);
// Promote forall-bound vars in kernel-loaded type schemes too. Kernel
// JSON often encodes polymorphic types as `{variables: [], body:
// annotated(forall(...))}`, but the inference engine reads
// `ts.variables` to know how many type vars to instantiate.
const _cachedKernelTypes: ReadonlyMap<Name, TypeScheme> = maps.fromList(
  _kernelLoaded.types.map(([k, ts]) => {
    const tsAny = ts as { variables?: Name[]; body: Type; constraints?: unknown };
    if (tsAny.variables && tsAny.variables.length > 0) return [k, ts] as const;
    const { vars, body } = collectForallVars(tsAny.body);
    if (vars.length === 0) return [k, ts] as const;
    return [k, { variables: vars, body, constraints: tsAny.constraints ?? { tag: "nothing" } } as unknown as TypeScheme] as const;
  }));

const buildGraph = (testTypes: ReadonlyMap<Name, Type>, testTerms: ReadonlyMap<Name, Term>): Graph => {
  // Wrap each test Type into a TypeScheme, promoting any forall-bound
  // variables from the body up to the scheme level.
  const wrappedTestTypes: ReadonlyMap<Name, TypeScheme> = maps.fromList(
    maps.toList(testTypes).map(([k, v]) => {
      const { vars, body } = collectForallVars(v);
      return [k, { variables: vars, body, constraints: { tag: "nothing" } } as unknown as TypeScheme] as const;
    }));
  // schemaTypes carries kernel type definitions (records/unions/wraps)
  // so the reducer can resolve nominal type fields via `cases _Term`.
  // boundTypes is the typing context for term-level lookups; test types
  // belong there. The two need to be kept separate or the typing
  // assertions for tests like `hydra.lib.maps.lookup` see kernel-type
  // schemas where they expect test-type schemas.
  //
  // `union` is left-biased (Haskell Data.Map.union), so per-test
  // overrides go FIRST to win on key collisions with the kernel.
  const schemaTypes: ReadonlyMap<Name, TypeScheme> =
    maps.union(wrappedTestTypes, _cachedKernelTypes);
  // boundTerms: per-test overrides + kernel JSON bindings.
  const mergedTerms: ReadonlyMap<Name, Term> = maps.union(testTerms, _cachedKernelTerms);
  return {
    boundTerms: mergedTerms,
    boundTypes: wrappedTestTypes,
    classConstraints: maps.empty,
    lambdaVariables: sets.empty,
    metadata: maps.empty,
    primitives: _cachedPrimitives,
    schemaTypes: schemaTypes,
    typeVariables: sets.empty,
  };
};

// Test graph: a flat (uncurried) function matching the coder's flat-args
// emission ABI. The DSL signature `Map Name Type -> Map Name Term -> Graph`
// is emitted by the TypeScript coder as `(testTypes, testTerms) => Graph`
// because nested lambdas are peeled into a single arrow.
export const testGraph = (testTypes: ReadonlyMap<Name, Type>, testTerms: ReadonlyMap<Name, Term>): Graph =>
  buildGraph(testTypes, testTerms);
