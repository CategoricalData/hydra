// Note: this is an automatically generated file. Do not edit.

/**
 * Graph to type environment conversions
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as EncodeCore from "./encode/core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Scoping from "./scoping.js";
import * as Sorting from "./sorting.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function definitionAsTypeApplicationTerm(el: Core.Binding): Errors.Error | Core.TypeApplicationTerm {
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "typed binding",
    actual: "untyped binding"
  }) }) }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: ({
    body: ((_x) => _x.term)(el),
    type: ((_x) => _x.type)(ts)
  }) })))(((_x) => _x.type)(el));
}

export function graphAsLet(bindings: ReadonlyArray<Core.Binding>): ((x: Core.Term) => Core.Let) {
  return ((body: Core.Term) => ({
    bindings: bindings,
    body: body
  }));
}

export function graphAsTerm(bindings: ReadonlyArray<Core.Binding>): ((x: Core.Term) => Core.Term) {
  return ((body: Core.Term) => ({ tag: "let", value: graphAsLet(bindings)(body) }));
}

export function graphAsTypes(graph: Graph.Graph): ((x: ReadonlyArray<Core.Binding>) => Errors.DecodingError | ReadonlyMap<Core.Name, Core.Type>) {
  return ((els: ReadonlyArray<Core.Binding>) => (() => {
  const toPair = ((el: Core.Binding) => LibEithers.map(((typ: Core.Type) => [((_x) => _x.name)(el), typ]))(DecodeCore.type(graph)(((_x) => _x.term)(el))));
  return LibEithers.map(LibMaps.fromList)(LibEithers.mapList(toPair)(els));
})());
}

export function partitionDefinitions(defs: ReadonlyArray<Packaging.Definition>): readonly [ReadonlyArray<Packaging.TypeDefinition>, ReadonlyArray<Packaging.TermDefinition>] {
  return (() => {
  const getType = ((def: Packaging.Definition) => (() => {
  const _m = def;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => td)((_m as any).value);
    case "term": return ((_: Packaging.TermDefinition) => null)((_m as any).value);
  }
})());
  return (() => {
  const getTerm = ((def: Packaging.Definition) => (() => {
  const _m = def;
  switch (_m.tag) {
    case "type": return ((_: Packaging.TypeDefinition) => null)((_m as any).value);
    case "term": return ((td: Packaging.TermDefinition) => td)((_m as any).value);
  }
})());
  return [LibMaybes.cat(LibLists.map(getType)(defs)), LibMaybes.cat(LibLists.map(getTerm)(defs))];
})();
})();
}

export function reorderDefs(defs: ReadonlyArray<Packaging.Definition>): ReadonlyArray<Packaging.Definition> {
  return (() => {
  const partitioned = partitionDefinitions(defs);
  return (() => {
  const typeDefsRaw = LibPairs.first(partitioned);
  return (() => {
  const nameFirst = LibLists.filter(((td: Packaging.TypeDefinition) => LibEquality.equal(((_x) => _x.name)(td))("hydra.core.Name")))(typeDefsRaw);
  return (() => {
  const nameRest = LibLists.filter(((td: Packaging.TypeDefinition) => LibLogic.not(LibEquality.equal(((_x) => _x.name)(td))("hydra.core.Name"))))(typeDefsRaw);
  return (() => {
  const typeDefs = LibLists.concat([LibLists.map(((td: Packaging.TypeDefinition) => ({ tag: "type", value: td })))(nameFirst), LibLists.map(((td: Packaging.TypeDefinition) => ({ tag: "type", value: td })))(nameRest)]);
  return (() => {
  const termDefsWrapped = LibLists.map(((td: Packaging.TermDefinition) => ({ tag: "term", value: td })))(LibPairs.second(partitioned));
  return (() => {
  const sortedTermDefs = LibLists.concat(Sorting.topologicalSortNodes(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ((_x) => _x.name)(td))((_m as any).value);
  }
})()))(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => LibSets.toList(Variables.freeVariablesInTerm(((_x) => _x.term)(td))))((_m as any).value);
    default: return [](_m);
  }
})()))(termDefsWrapped));
  return LibLists.concat([typeDefs, sortedTermDefs]);
})();
})();
})();
})();
})();
})();
})();
}

export function schemaGraphToTypingEnvironment(g: Graph.Graph): Errors.Error | ReadonlyMap<Core.Name, Core.TypeScheme> {
  return (() => {
  const toTypeScheme = ((vars: ReadonlyArray<Core.Name>) => ((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => toTypeScheme(LibLists.cons(((_x) => _x.parameter)(ft))(vars))(((_x) => _x.body)(ft)))((_m as any).value);
    default: return ({
    variables: LibLists.reverse(vars),
    type: typ,
    constraints: null
  })(_m);
  }
})()));
  return (() => {
  const decodeType = ((term: Core.Term) => LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((_a: Core.Type) => _a))(DecodeCore.type(g)(term)));
  return (() => {
  const decodeTypeScheme = ((term: Core.Term) => LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((_a: Core.TypeScheme) => _a))(DecodeCore.typeScheme(g)(term)));
  return (() => {
  const toPair = ((el: Core.Binding) => (() => {
  const forTerm = ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "record": return ((r: Core.Record) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.typeName)(r))("hydra.core.TypeScheme"))(LibEithers.map(LibMaybes.pure)(decodeTypeScheme(((_x) => _x.term)(el))))(({ tag: "right", value: null })))((_m as any).value);
    case "inject": return ((i: Core.Injection) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.typeName)(i))("hydra.core.Type"))(LibEithers.map(((decoded: Core.Type) => toTypeScheme([])(decoded)))(decodeType(((_x) => _x.term)(el))))(({ tag: "right", value: null })))((_m as any).value);
    default: return ({ tag: "right", value: null })(_m);
  }
})());
  return LibEithers.bind(LibMaybes.maybe(LibEithers.map(((typ: Core.Type) => Scoping.fTypeToTypeScheme(typ)))(decodeType(((_x) => _x.term)(el))))(((ts: Core.TypeScheme) => LibLogic.ifElse(LibEquality.equal(ts)(({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.TypeScheme" }),
    constraints: null
  })))(LibEithers.map(LibMaybes.pure)(decodeTypeScheme(((_x) => _x.term)(el))))(LibLogic.ifElse(LibEquality.equal(ts)(({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.Type" }),
    constraints: null
  })))(LibEithers.map(((decoded: Core.Type) => toTypeScheme([])(decoded)))(decodeType(((_x) => _x.term)(el))))(forTerm(Strip.deannotateTerm(((_x) => _x.term)(el)))))))(((_x) => _x.type)(el)))(((mts: Core.TypeScheme | null) => ({ tag: "right", value: LibMaybes.map(((ts: Core.TypeScheme) => [((_x) => _x.name)(el), ts]))(mts) })));
})());
  return LibEithers.map(((mpairs: ReadonlyArray<readonly [Core.Name, Core.TypeScheme] | null>) => LibMaps.fromList(LibMaybes.cat(mpairs))))(LibEithers.mapList(toPair)(Lexical.graphToBindings(g)));
})();
})();
})();
})();
}

export function termAsBindings(term: Core.Term): ReadonlyArray<Core.Binding> {
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => ((_x) => _x.bindings)(lt))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function typesToDefinitions(typeMap: ReadonlyMap<Core.Name, Core.Type>): ReadonlyArray<Core.Binding> {
  return (() => {
  const toElement = ((pair: readonly [Core.Name, Core.Type]) => (() => {
  const name = LibPairs.first(pair);
  return ({
    name: name,
    term: EncodeCore.type(LibPairs.second(pair)),
    type: null
  });
})());
  return LibLists.map(toElement)(LibMaps.toList(typeMap));
})();
}

export function withLambdaContext<t0, t1, t2>(getContext: ((x: t0) => Graph.Graph)): ((x: ((x: Graph.Graph) => ((x: t0) => t1))) => ((x: t0) => ((x: Core.Lambda) => ((x: ((x: t1) => t2)) => t2)))) {
  return ((setContext: ((x: Graph.Graph) => ((x: t0) => t1))) => ((env: t0) => ((lam: Core.Lambda) => ((body: ((x: t1) => t2)) => (() => {
  const newContext = Scoping.extendGraphForLambda(getContext(env))(lam);
  return body(setContext(newContext)(env));
})()))));
}

export function withLetContext<t0, t1, t2>(getContext: ((x: t0) => Graph.Graph)): ((x: ((x: Graph.Graph) => ((x: t0) => t1))) => ((x: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))) => ((x: t0) => ((x: Core.Let) => ((x: ((x: t1) => t2)) => t2))))) {
  return ((setContext: ((x: Graph.Graph) => ((x: t0) => t1))) => ((forBinding: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))) => ((env: t0) => ((letrec: Core.Let) => ((body: ((x: t1) => t2)) => (() => {
  const newContext = Scoping.extendGraphForLet(forBinding)(getContext(env))(letrec);
  return body(setContext(newContext)(env));
})())))));
}

export function withTypeLambdaContext<t0, t1, t2>(getContext: ((x: t0) => Graph.Graph)): ((x: ((x: Graph.Graph) => ((x: t0) => t1))) => ((x: t0) => ((x: Core.TypeLambda) => ((x: ((x: t1) => t2)) => t2)))) {
  return ((setContext: ((x: Graph.Graph) => ((x: t0) => t1))) => ((env: t0) => ((tlam: Core.TypeLambda) => ((body: ((x: t1) => t2)) => (() => {
  const newContext = Scoping.extendGraphForTypeLambda(getContext(env))(tlam);
  return body(setContext(newContext)(env));
})()))));
}
