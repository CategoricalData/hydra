// Note: this is an automatically generated file. Do not edit.

/**
 * Transform test cases for code generation, filtering to tests that can be compiled to target languages
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Sorting from "../sorting.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function addGenerationPrefix(ns_: Packaging.Namespace): Packaging.Namespace {
  return LibStrings.cat2("generation.")(((_x) => _x)(ns_));
}

export function buildConvertCaseCall(fromConv: Util.CaseConvention): ((x: Util.CaseConvention) => ((x: string) => Core.Term)) {
  return ((toConv: Util.CaseConvention) => ((input_: string) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.formatting.convertCase" }),
    argument: encodeCaseConvention(fromConv)
  }) }),
    argument: encodeCaseConvention(toConv)
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: input_ }) })
  }) })));
}

export function buildTopologicalSortCall(adjList: ReadonlyArray<readonly [number, ReadonlyArray<number>]>): Core.Term {
  return ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.sorting.topologicalSort" }),
    argument: encodeAdjacencyList(adjList)
  }) });
}

export function buildTopologicalSortSCCCall(adjList: ReadonlyArray<readonly [number, ReadonlyArray<number>]>): Core.Term {
  return ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.sorting.topologicalSortComponents" }),
    argument: encodeAdjacencyList(adjList)
  }) });
}

export function collectTestCases(tg: Testing.TestGroup): ReadonlyArray<Testing.TestCaseWithMetadata> {
  return LibLists.concat2(((_x) => _x.cases)(tg))(LibLists.concat(LibLists.map(((sg: Testing.TestGroup) => collectTestCases(sg)))(((_x) => _x.subgroups)(tg))));
}

export function encodeAdjacencyList(pairs: ReadonlyArray<readonly [number, ReadonlyArray<number>]>): Core.Term {
  return ({ tag: "list", value: LibLists.map(((p: readonly [number, ReadonlyArray<number>]) => ({ tag: "pair", value: [encodeInt(LibPairs.first(p)), ({ tag: "list", value: LibLists.map(((d: number) => encodeInt(d)))(LibPairs.second(p)) })] })))(pairs) });
}

export function encodeCaseConvention(conv: Util.CaseConvention): Core.Term {
  return ({ tag: "inject", value: ({
    typeName: "hydra.util.CaseConvention",
    field: ({
    name: (() => {
  const _m = conv;
  switch (_m.tag) {
    case "lowerSnake": return ((_: void) => "lowerSnake")((_m as any).value);
    case "upperSnake": return ((_: void) => "upperSnake")((_m as any).value);
    case "camel": return ((_: void) => "camel")((_m as any).value);
    case "pascal": return ((_: void) => "pascal")((_m as any).value);
  }
})(),
    term: ({ tag: "unit" })
  })
  }) });
}

export function encodeEitherListList(e: ReadonlyArray<ReadonlyArray<number>> | ReadonlyArray<number>): Core.Term {
  return ({ tag: "either", value: LibEithers.bimap(((cycles: ReadonlyArray<ReadonlyArray<number>>) => encodeListList(cycles)))(((sorted: ReadonlyArray<number>) => encodeIntList(sorted)))(e) });
}

export function encodeInt(n: number): Core.Term {
  return ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: n }) }) });
}

export function encodeIntList(ints: ReadonlyArray<number>): Core.Term {
  return ({ tag: "list", value: LibLists.map(((n: number) => encodeInt(n)))(ints) });
}

export function encodeListList(lists: ReadonlyArray<ReadonlyArray<number>>): Core.Term {
  return ({ tag: "list", value: LibLists.map(((l: ReadonlyArray<number>) => encodeIntList(l)))(lists) });
}

export function transformModule(m: Packaging.Module): Packaging.Module {
  return ({
    namespace: addGenerationPrefix(((_x) => _x.namespace)(m)),
    definitions: ((_x) => _x.definitions)(m),
    termDependencies: ((_x) => _x.termDependencies)(m),
    typeDependencies: ((_x) => _x.typeDependencies)(m),
    description: ((_x) => _x.description)(m)
  });
}

export function transformTestCase<t0>(tcm: t0): t0 | null {
  return tcm;
}

export function transformToCompiledTests(tg: Testing.TestGroup): Testing.TestGroup | null {
  return (() => {
  const name_ = ((_x) => _x.name)(tg);
  const desc = ((_x) => _x.description)(tg);
  const subgroups = ((_x) => _x.subgroups)(tg);
  const cases_ = ((_x) => _x.cases)(tg);
  const transformedCases = LibMaybes.cat(LibLists.map(((tc: Testing.TestCaseWithMetadata) => transformTestCase(tc)))(cases_));
  const transformedSubgroups = LibMaybes.cat(LibLists.map(((sg: Testing.TestGroup) => transformToCompiledTests(sg)))(subgroups));
  return LibLogic.ifElse(LibLogic.and(LibLists.null_(transformedCases))(LibLists.null_(transformedSubgroups)))(null)(({
    name: name_,
    description: desc,
    subgroups: transformedSubgroups,
    cases: transformedCases
  }));
})();
}
