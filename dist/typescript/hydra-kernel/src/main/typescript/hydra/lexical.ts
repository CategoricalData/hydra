// Note: this is an automatically generated file. Do not edit.

/**
 * A module for lexical operations over graphs.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as ShowCore from "./show/core.js";
import * as ShowErrors from "./show/errors.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function buildGraph(elements: ReadonlyArray<Core.Binding>): ((x: ReadonlyMap<Core.Name, Core.Term | null>) => ((x: ReadonlyMap<Core.Name, Graph.Primitive>) => Graph.Graph)) {
  return ((environment: ReadonlyMap<Core.Name, Core.Term | null>) => ((primitives: ReadonlyMap<Core.Name, Graph.Primitive>) => (() => {
  const elementTerms = LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), ((_x) => _x.term)(b)]))(elements));
  return (() => {
  const letTerms = LibMaps.map(((mt: Core.Term | null) => LibMaybes.fromJust(mt)))(LibMaps.filter(((mt: Core.Term | null) => LibMaybes.isJust(mt)))(environment));
  return (() => {
  const mergedTerms = LibMaps.union(elementTerms)(letTerms);
  return (() => {
  const filteredTerms = LibMaps.filterWithKey(((k: Core.Name) => ((_v: Core.Term) => LibLogic.not(LibMaps.member(k)(primitives)))))(mergedTerms);
  return (() => {
  const elementTypes = LibMaps.fromList(LibMaybes.cat(LibLists.map(((b: Core.Binding) => LibMaybes.map(((ts: Core.TypeScheme) => [((_x) => _x.name)(b), ts]))(((_x) => _x.type)(b))))(elements)));
  return (() => {
  const filteredTypes = LibMaps.filterWithKey(((k: Core.Name) => ((_v: Core.TypeScheme) => LibLogic.not(LibMaps.member(k)(primitives)))))(elementTypes);
  return ({
    boundTerms: filteredTerms,
    boundTypes: filteredTypes,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.fromList(LibMaps.keys(LibMaps.filter(((mt: Core.Term | null) => LibMaybes.isNothing(mt)))(environment))),
    metadata: LibMaps.empty,
    primitives: primitives,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  });
})();
})();
})();
})();
})();
})()));
}

export function chooseUniqueName(reserved: ReadonlySet<Core.Name>): ((x: Core.Name) => Core.Name) {
  return ((name: Core.Name) => (() => {
  const tryName = ((index: number) => (() => {
  const candidate = LibLogic.ifElse(LibEquality.equal(index)(1))(name)(LibStrings.cat2(((_x) => _x)(name))(LibLiterals.showInt32(index)));
  return LibLogic.ifElse(LibSets.member(candidate)(reserved))(tryName(LibMath.add(index)(1)))(candidate);
})());
  return tryName(1);
})());
}

export function dereferenceSchemaType(name: Core.Name): ((x: ReadonlyMap<Core.Name, Core.TypeScheme>) => Core.TypeScheme | null) {
  return ((types: ReadonlyMap<Core.Name, Core.TypeScheme>) => (() => {
  const forType = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => forType(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => LibMaybes.map(((ts: Core.TypeScheme) => ({
    variables: LibLists.cons(((_x) => _x.parameter)(ft))(((_x) => _x.variables)(ts)),
    type: ((_x) => _x.type)(ts),
    constraints: ((_x) => _x.constraints)(ts)
  })))(forType(((_x) => _x.body)(ft))))((_m as any).value);
    case "variable": return ((v: Core.Name) => dereferenceSchemaType(v)(types))((_m as any).value);
    default: return ({
    variables: [],
    type: t,
    constraints: null
  })(_m);
  }
})());
  return LibMaybes.bind(LibMaps.lookup(name)(types))(((ts: Core.TypeScheme) => LibMaybes.map(((ts2: Core.TypeScheme) => ({
    variables: LibLists.concat2(((_x) => _x.variables)(ts))(((_x) => _x.variables)(ts2)),
    type: ((_x) => _x.type)(ts2),
    constraints: ((_x) => _x.constraints)(ts2)
  })))(forType(((_x) => _x.type)(ts)))));
})());
}

export function dereferenceVariable(graph: Graph.Graph): ((x: Core.Name) => Errors.Error | Core.Binding) {
  return ((name: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchBinding", value: ({
    name: name
  }) }) }) }))(((right_: Core.Binding) => ({ tag: "right", value: right_ })))(lookupBinding(graph)(name)));
}

export function elementsToGraph(parent: Graph.Graph): ((x: ReadonlyMap<Core.Name, Core.TypeScheme>) => ((x: ReadonlyArray<Core.Binding>) => Graph.Graph)) {
  return ((schemaTypes: ReadonlyMap<Core.Name, Core.TypeScheme>) => ((elements: ReadonlyArray<Core.Binding>) => (() => {
  const prims = ((_x) => _x.primitives)(parent);
  return (() => {
  const g = buildGraph(elements)(LibMaps.empty)(prims);
  return ({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: ((_x) => _x.boundTypes)(g),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: schemaTypes,
    typeVariables: ((_x) => _x.typeVariables)(g)
  });
})();
})()));
}

export const emptyContext: Context.Context = ({
    trace: [],
    messages: [],
    other: LibMaps.empty
  });

export const emptyGraph: Graph.Graph = ({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  });

export function fieldsOf(t: Core.Type): ReadonlyArray<Core.FieldType> {
  return (() => {
  const stripped = Strip.deannotateType(t);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "forall": return ((forallType: Core.ForallType) => fieldsOf(((_x) => _x.body)(forallType)))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => rt)((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => rt)((_m as any).value);
    default: return [](_m);
  }
})();
})();
}

export function getField<t0, t1>(m: ReadonlyMap<Core.Name, t0>): ((x: Core.Name) => ((x: ((x: t0) => Errors.Error | t1)) => Errors.Error | t1)) {
  return ((fname: Core.Name) => ((decode: ((x: t0) => Errors.Error | t1)) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: fname
  }) }) }) }))(decode)(LibMaps.lookup(fname)(m))));
}

export function graphToBindings(g: Graph.Graph): ReadonlyArray<Core.Binding> {
  return LibLists.map(((p: readonly [Core.Name, Core.Term]) => (() => {
  const name = LibPairs.first(p);
  return (() => {
  const term = LibPairs.second(p);
  return ({
    name: name,
    term: term,
    type: LibMaps.lookup(name)(((_x) => _x.boundTypes)(g))
  });
})();
})()))(LibMaps.toList(((_x) => _x.boundTerms)(g)));
}

export function graphWithPrimitives(builtIn: ReadonlyArray<Graph.Primitive>): ((x: ReadonlyArray<Graph.Primitive>) => Graph.Graph) {
  return ((userProvided: ReadonlyArray<Graph.Primitive>) => (() => {
  const toMap = ((ps: ReadonlyArray<Graph.Primitive>) => LibMaps.fromList(LibLists.map(((p: Graph.Primitive) => [((_x) => _x.name)(p), p]))(ps)));
  return (() => {
  const prims = LibMaps.union(toMap(userProvided))(toMap(builtIn));
  return buildGraph([])(LibMaps.empty)(prims);
})();
})());
}

export function lookupBinding(graph: Graph.Graph): ((x: Core.Name) => Core.Binding | null) {
  return ((name: Core.Name) => LibMaybes.map(((term: Core.Term) => ({
    name: name,
    term: term,
    type: LibMaps.lookup(name)(((_x) => _x.boundTypes)(graph))
  })))(LibMaps.lookup(name)(((_x) => _x.boundTerms)(graph))));
}

export function lookupPrimitive(graph: Graph.Graph): ((x: Core.Name) => Graph.Primitive | null) {
  return ((name: Core.Name) => LibMaps.lookup(name)(((_x) => _x.primitives)(graph)));
}

export function lookupTerm(graph: Graph.Graph): ((x: Core.Name) => Core.Term | null) {
  return ((name: Core.Name) => LibMaps.lookup(name)(((_x) => _x.boundTerms)(graph)));
}

export function matchEnum<t0>(graph: Graph.Graph): ((x: Core.Name) => ((x: ReadonlyArray<readonly [Core.Name, t0]>) => ((x: Core.Term) => Errors.Error | t0))) {
  return ((tname: Core.Name) => ((pairs: ReadonlyArray<readonly [Core.Name, t0]>) => ((v1: Core.Term) => matchUnion(graph)(tname)(LibLists.map(((pair: readonly [Core.Name, t0]) => matchUnitField(LibPairs.first(pair))(LibPairs.second(pair))))(pairs))(v1))));
}

export function matchRecord<t0, t1>(graph: t0): ((x: ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | t1)) => ((x: Core.Term) => Errors.Error | t1)) {
  return ((decode: ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | t1)) => ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateAndDetypeTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => decode(LibMaps.fromList(LibLists.map(((field: Core.Field) => [((_x) => _x.name)(field), ((_x) => _x.term)(field)]))(((_x) => _x.fields)(record)))))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: "record",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})();
})()));
}

export function matchUnion<t0>(graph: Graph.Graph): ((x: Core.Name) => ((x: ReadonlyArray<readonly [Core.Name, ((x: Core.Term) => Errors.Error | t0)]>) => ((x: Core.Term) => Errors.Error | t0))) {
  return ((tname: Core.Name) => ((pairs: ReadonlyArray<readonly [Core.Name, ((x: Core.Term) => Errors.Error | t0)]>) => ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateAndDetypeTerm(term);
  return (() => {
  const mapping = LibMaps.fromList(pairs);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => LibEithers.bind(requireBinding(graph)(name))(((el: Core.Binding) => matchUnion(graph)(tname)(pairs)(((_x) => _x.term)(el)))))((_m as any).value);
    case "inject": return ((injection: Core.Injection) => (() => {
  const exp = (() => {
  const fname = ((_x) => _x.name)(((_x) => _x.field)(injection));
  return (() => {
  const val = ((_x) => _x.term)(((_x) => _x.field)(injection));
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: fname
  }) }) }) }))(((f: ((x: Core.Term) => Errors.Error | t0)) => f(val)))(LibMaps.lookup(fname)(mapping));
})();
})();
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.typeName)(injection)))(((_x) => _x)(tname)))(exp)(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("injection for type ")(((_x) => _x)(tname)),
    actual: ShowCore.term(term)
  }) }) }) }));
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: LibStrings.cat2("injection for type ")(((_x) => _x)(tname)),
    actual: ShowCore.term(stripped)
  }) }) }) })(_m);
  }
})();
})();
})())));
}

export function matchUnitField<t0, t1, t2, t3>(fname: t0): ((x: t1) => readonly [t0, ((x: t2) => t3 | t1)]) {
  return ((x: t1) => [fname, ((ignored: t2) => ({ tag: "right", value: x }))]);
}

export function requireBinding(graph: Graph.Graph): ((x: Core.Name) => Errors.Error | Core.Binding) {
  return ((name: Core.Name) => (() => {
  const showAll = false;
  return (() => {
  const ellipsis = ((strings: ReadonlyArray<string>) => LibLogic.ifElse(LibLogic.and(LibEquality.gt(LibLists.length(strings))(3))(LibLogic.not(showAll)))(LibLists.concat2(LibLists.take(3)(strings))(["..."]))(strings));
  return (() => {
  const errMsg = LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("no such element: ")(((_x) => _x)(name)))(". Available elements: {"))(LibStrings.intercalate(", ")(ellipsis(LibLists.map(((_x) => _x))(LibMaps.keys(((_x) => _x.boundTerms)(graph)))))))("}");
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "other", value: errMsg }) }) }))(((x: Core.Binding) => ({ tag: "right", value: x })))(lookupBinding(graph)(name));
})();
})();
})());
}

export function requirePrimitive(graph: Graph.Graph): ((x: Core.Name) => Errors.Error | Graph.Primitive) {
  return ((name: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchPrimitive", value: ({
    name: name
  }) }) }) }))(((x: Graph.Primitive) => ({ tag: "right", value: x })))(lookupPrimitive(graph)(name)));
}

export function requirePrimitiveType(tx: Graph.Graph): ((x: Core.Name) => Errors.Error | Core.TypeScheme) {
  return ((name: Core.Name) => (() => {
  const mts = LibMaybes.map(((_p: Graph.Primitive) => ((_x) => _x.type)(_p)))(LibMaps.lookup(name)(((_x) => _x.primitives)(tx)));
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchPrimitive", value: ({
    name: name
  }) }) }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: ts })))(mts);
})());
}

export function requireTerm(graph: Graph.Graph): ((x: Core.Name) => Errors.Error | Core.Term) {
  return ((name: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchBinding", value: ({
    name: name
  }) }) }) }))(((x: Core.Term) => ({ tag: "right", value: x })))(resolveTerm(graph)(name)));
}

export function resolveTerm(graph: Graph.Graph): ((x: Core.Name) => Core.Term | null) {
  return ((name: Core.Name) => (() => {
  const recurse = ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "variable": return ((name_: Core.Name) => resolveTerm(graph)(name_))((_m as any).value);
    default: return term(_m);
  }
})();
})());
  return LibMaybes.maybe(null)(recurse)(lookupTerm(graph)(name));
})());
}

export function stripAndDereferenceTerm(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Term) {
  return ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateAndDetypeTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibEithers.bind(requireTerm(graph)(v))(((t: Core.Term) => stripAndDereferenceTerm(graph)(t))))((_m as any).value);
    default: return ({ tag: "right", value: stripped })(_m);
  }
})();
})());
}

export function stripAndDereferenceTermEither(graph: Graph.Graph): ((x: Core.Term) => Errors.Error | Core.Term) {
  return ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateAndDetypeTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibEithers.either(((left_: Errors.Error) => ({ tag: "left", value: left_ })))(((binding: Core.Binding) => stripAndDereferenceTermEither(graph)(((_x) => _x.term)(binding))))(dereferenceVariable(graph)(v)))((_m as any).value);
    default: return ({ tag: "right", value: stripped })(_m);
  }
})();
})());
}
