// Note: this is an automatically generated file. Do not edit.

/**
 * Dependency extraction, binding sort, and let normalization
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
import * as Lexical from "./lexical.js";
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
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as Sorting from "./sorting.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function definitionsWithDependencies<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Binding>) => Errors.Error | ReadonlyArray<Core.Binding>)) {
  return ((graph: Graph.Graph) => ((original: ReadonlyArray<Core.Binding>) => (() => {
  const depNames = ((el: Core.Binding) => LibSets.toList(termDependencyNames(true)(false)(false)(((_x) => _x.term)(el))));
  return (() => {
  const allDepNames = LibLists.nub(LibLists.concat2(LibLists.map(((_x) => _x.name))(original))(LibLists.concat(LibLists.map(depNames)(original))));
  return LibEithers.mapList(((name: Core.Name) => Lexical.requireBinding(graph)(name)))(allDepNames);
})();
})()));
}

export function flattenLetTerms(term: Core.Term): Core.Term {
  return (() => {
  const rewriteBinding = ((binding: Core.Binding) => (() => {
  const key0 = ((_x) => _x.name)(binding);
  return (() => {
  const val0 = ((_x) => _x.term)(binding);
  return (() => {
  const t = ((_x) => _x.type)(binding);
  return (() => {
  const _m = val0;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => (() => {
  const val1 = ((_x) => _x.body)(at);
  return (() => {
  const ann = ((_x) => _x.annotation)(at);
  return (() => {
  const recursive = rewriteBinding(({
    name: key0,
    term: val1,
    type: t
  }));
  return (() => {
  const innerBinding = LibPairs.first(recursive);
  return (() => {
  const deps = LibPairs.second(recursive);
  return (() => {
  const val2 = ((_x) => _x.term)(innerBinding);
  return [({
    name: key0,
    term: ({ tag: "annotated", value: ({
    body: val2,
    annotation: ann
  }) }),
    type: t
  }), deps];
})();
})();
})();
})();
})();
})())((_m as any).value);
    case "let": return ((innerLet: Core.Let) => (() => {
  const bindings1 = ((_x) => _x.bindings)(innerLet);
  return (() => {
  const body1 = ((_x) => _x.body)(innerLet);
  return (() => {
  const prefix = LibStrings.cat2(((_x) => _x)(key0))("_");
  return (() => {
  const qualify = ((n: Core.Name) => LibStrings.cat2(prefix)(((_x) => _x)(n)));
  return (() => {
  const toSubstPair = ((b: Core.Binding) => [((_x) => _x.name)(b), qualify(((_x) => _x.name)(b))]);
  return (() => {
  const subst = LibMaps.fromList(LibLists.map(toSubstPair)(bindings1));
  return (() => {
  const replaceVars = ((v1: Core.Term) => Variables.substituteVariables(subst)(v1));
  return (() => {
  const newBody = replaceVars(body1);
  return (() => {
  const newBinding = ((b: Core.Binding) => ({
    name: qualify(((_x) => _x.name)(b)),
    term: replaceVars(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return [({
    name: key0,
    term: newBody,
    type: t
  }), LibLists.map(newBinding)(bindings1)];
})();
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    default: return [({
    name: key0,
    term: val0,
    type: t
  }), []](_m);
  }
})();
})();
})();
})());
  return (() => {
  const flattenBodyLet = ((bindings: ReadonlyArray<Core.Binding>) => ((body: Core.Term) => (() => {
  const _m = body;
  switch (_m.tag) {
    case "let": return ((innerLt: Core.Let) => (() => {
  const innerBindings = ((_x) => _x.bindings)(innerLt);
  return (() => {
  const innerBody = ((_x) => _x.body)(innerLt);
  return flattenBodyLet(LibLists.concat2(bindings)(innerBindings))(innerBody);
})();
})())((_m as any).value);
    default: return [LibLists.concat2([])(bindings), body](_m);
  }
})()));
  return (() => {
  const flatten = ((recurse: ((x: t0) => Core.Term)) => ((term2: t0) => (() => {
  const rewritten = recurse(term2);
  return (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return (() => {
  const forResult = ((hr: readonly [t1, ReadonlyArray<t1>]) => LibLists.concat2(LibPairs.second(hr))(LibLists.pure(LibPairs.first(hr))));
  return (() => {
  const flattenedBindings = LibLists.concat(LibLists.map(((arg_: Core.Binding) => forResult(rewriteBinding(arg_))))(bindings));
  return (() => {
  const merged = flattenBodyLet(flattenedBindings)(body);
  return (() => {
  const newBindings = LibPairs.first(merged);
  return (() => {
  const newBody = LibPairs.second(merged);
  return ({ tag: "let", value: ({
    bindings: newBindings,
    body: newBody
  }) });
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    default: return rewritten(_m);
  }
})();
})()));
  return Rewriting.rewriteTerm(flatten)(term);
})();
})();
})();
}

export function inlineType(schema: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Type) => Errors.Error | Core.Type) {
  return ((typ: Core.Type) => (() => {
  const f = ((recurse: ((x: t0) => Errors.Error | Core.Type)) => ((typ2: t0) => (() => {
  const afterRecurse = ((tr: Core.Type) => (() => {
  const _m = tr;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("No such type in schema: ")(((_x) => _x)(v)) }) }))(((v1: Core.Type) => inlineType(schema)(v1)))(LibMaps.lookup(v)(schema)))((_m as any).value);
    default: return ({ tag: "right", value: tr })(_m);
  }
})());
  return LibEithers.bind(recurse(typ2))(((tr: Core.Type) => afterRecurse(tr)));
})()));
  return Rewriting.rewriteTypeM(f)(typ);
})());
}

export function isLambda(term: Core.Term): boolean {
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "lambda": return ((_: Core.Lambda) => true)((_m as any).value);
    case "let": return ((lt: Core.Let) => isLambda(((_x) => _x.body)(lt)))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function liftLambdaAboveLet(term0: Core.Term): Core.Term {
  return (() => {
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const rewriteBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: rewrite(recurse)(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return (() => {
  const rewriteBindings = ((bs: ReadonlyArray<Core.Binding>) => LibLists.map(rewriteBinding)(bs));
  return (() => {
  const digForLambdas = ((original: Core.Term) => ((cons: ((x: Core.Term) => Core.Term)) => ((term2: Core.Term) => (() => {
  const _m = term2;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => digForLambdas(original)(((t: Core.Term) => ({ tag: "annotated", value: ({
    body: cons(t),
    annotation: ((_x) => _x.annotation)(at)
  }) })))(((_x) => _x.body)(at)))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: digForLambdas(cons(((_x) => _x.body)(l)))(((t: Core.Term) => cons(t)))(((_x) => _x.body)(l))
  }) }))((_m as any).value);
    case "let": return ((l: Core.Let) => digForLambdas(original)(((t: Core.Term) => cons(({ tag: "let", value: ({
    bindings: rewriteBindings(((_x) => _x.bindings)(l)),
    body: t
  }) }))))(((_x) => _x.body)(l)))((_m as any).value);
    default: return recurse(original)(_m);
  }
})())));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "let": return ((l: Core.Let) => digForLambdas(term)(((t: Core.Term) => ({ tag: "let", value: ({
    bindings: rewriteBindings(((_x) => _x.bindings)(l)),
    body: t
  }) })))(((_x) => _x.body)(l)))((_m as any).value);
    default: return recurse(term)(_m);
  }
})();
})();
})();
})()));
  return Rewriting.rewriteTerm(rewrite)(term0);
})();
}

export function pruneLet(l: Core.Let): Core.Let {
  return (() => {
  const bindingMap = LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), ((_x) => _x.term)(b)]))(((_x) => _x.bindings)(l)));
  return (() => {
  const rootName = "[[[root]]]";
  return (() => {
  const adj = ((n: Core.Name) => LibSets.intersection(LibSets.fromList(LibMaps.keys(bindingMap)))(Variables.freeVariablesInTerm(LibLogic.ifElse(LibEquality.equal(n)(rootName))(((_x) => _x.body)(l))(LibMaybes.fromJust(LibMaps.lookup(n)(bindingMap))))));
  return (() => {
  const reachable = Sorting.findReachableNodes(adj)(rootName);
  return (() => {
  const prunedBindings = LibLists.filter(((b: Core.Binding) => LibSets.member(((_x) => _x.name)(b))(reachable)))(((_x) => _x.bindings)(l));
  return ({
    bindings: prunedBindings,
    body: ((_x) => _x.body)(l)
  });
})();
})();
})();
})();
})();
}

export function replaceTypedefs(types: ReadonlyMap<Core.Name, Core.TypeScheme>): ((x: Core.Type) => Core.Type) {
  return ((typ0: Core.Type) => (() => {
  const rewrite = ((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => ({ tag: "annotated", value: ({
    body: rewrite(recurse)(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => typ)((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => typ)((_m as any).value);
    case "variable": return ((v: Core.Name) => (() => {
  const forMono = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => typ)((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => typ)((_m as any).value);
    case "wrap": return ((_: Core.Type) => typ)((_m as any).value);
    default: return rewrite(recurse)(t)(_m);
  }
})());
  return (() => {
  const forTypeScheme = ((ts: Core.TypeScheme) => (() => {
  const t = ((_x) => _x.type)(ts);
  return LibLogic.ifElse(LibLists.null_(((_x) => _x.variables)(ts)))(forMono(t))(typ);
})());
  return LibMaybes.maybe(typ)(((ts: Core.TypeScheme) => forTypeScheme(ts)))(LibMaps.lookup(v)(types));
})();
})())((_m as any).value);
    case "wrap": return ((_: Core.Type) => typ)((_m as any).value);
    default: return recurse(typ)(_m);
  }
})()));
  return Rewriting.rewriteType(rewrite)(typ0);
})());
}

export function simplifyTerm(term: Core.Term): Core.Term {
  return (() => {
  const simplify = ((recurse: ((x: Core.Term) => t0)) => ((term2: Core.Term) => (() => {
  const forRhs = ((rhs: Core.Term) => ((var_: Core.Name) => ((body: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(rhs);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => simplifyTerm(Variables.substituteVariable(var_)(v)(body)))((_m as any).value);
    default: return term2(_m);
  }
})())));
  return (() => {
  const forLhs = ((lhs: Core.Term) => ((rhs: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(lhs);
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const var_ = ((_x) => _x.parameter)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return LibLogic.ifElse(LibSets.member(var_)(Variables.freeVariablesInTerm(body)))(forRhs(rhs)(var_)(body))(simplifyTerm(body));
})();
})())((_m as any).value);
    default: return term2(_m);
  }
})()));
  return (() => {
  const forTerm = ((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return forLhs(lhs)(rhs);
})();
})())((_m as any).value);
    default: return term2(_m);
  }
})());
  return (() => {
  const stripped = Strip.deannotateTerm(term2);
  return recurse(forTerm(stripped));
})();
})();
})();
})()));
  return Rewriting.rewriteTerm(simplify)(term);
})();
}

export function termDependencyNames(binds: boolean): ((x: boolean) => ((x: boolean) => ((x: Core.Term) => ReadonlySet<Core.Name>))) {
  return ((withPrims: boolean) => ((withNoms: boolean) => ((term0: Core.Term) => (() => {
  const addNames = ((names: ReadonlySet<Core.Name>) => ((term: Core.Term) => (() => {
  const nominal = ((name: Core.Name) => LibLogic.ifElse(withNoms)(LibSets.insert(name)(names))(names));
  return (() => {
  const prim = ((name: Core.Name) => LibLogic.ifElse(withPrims)(LibSets.insert(name)(names))(names));
  return (() => {
  const var_ = ((name: Core.Name) => LibLogic.ifElse(binds)(LibSets.insert(name)(names))(names));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "cases": return ((caseStmt: Core.CaseStatement) => nominal(((_x) => _x.typeName)(caseStmt)))((_m as any).value);
    case "project": return ((proj: Core.Projection) => nominal(((_x) => _x.typeName)(proj)))((_m as any).value);
    case "unwrap": return ((name: Core.Name) => nominal(name))((_m as any).value);
    case "record": return ((record: Core.Record) => nominal(((_x) => _x.typeName)(record)))((_m as any).value);
    case "inject": return ((injection: Core.Injection) => nominal(((_x) => _x.typeName)(injection)))((_m as any).value);
    case "variable": return ((name: Core.Name) => var_(name))((_m as any).value);
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => nominal(((_x) => _x.typeName)(wrappedTerm)))((_m as any).value);
    default: return names(_m);
  }
})();
})();
})();
})()));
  return Rewriting.foldOverTerm(({ tag: "pre" }))(addNames)(LibSets.empty)(term0);
})())));
}

export function toShortNames(original: ReadonlyArray<Core.Name>): ReadonlyMap<Core.Name, Core.Name> {
  return (() => {
  const addName = ((acc: ReadonlyMap<string, ReadonlySet<Core.Name>>) => ((name: Core.Name) => (() => {
  const local = Names.localNameOf(name);
  return (() => {
  const group = LibMaybes.fromMaybe(LibSets.empty)(LibMaps.lookup(local)(acc));
  return LibMaps.insert(local)(LibSets.insert(name)(group))(acc);
})();
})()));
  return (() => {
  const groupNamesByLocal = ((names: ReadonlyArray<Core.Name>) => LibLists.foldl(addName)(LibMaps.empty)(names));
  return (() => {
  const groups = groupNamesByLocal(original);
  return (() => {
  const renameGroup = ((localNames: readonly [string, ReadonlySet<t0>]) => (() => {
  const local = LibPairs.first(localNames);
  return (() => {
  const names = LibPairs.second(localNames);
  return (() => {
  const rangeFrom = ((start: number) => LibLists.cons(start)(rangeFrom(LibMath.add(start)(1))));
  return (() => {
  const rename = ((name: t1) => ((i: number) => [name, LibLogic.ifElse(LibEquality.gt(i)(1))(LibStrings.cat2(local)(LibLiterals.showInt32(i)))(local)]));
  return LibLists.zipWith(rename)(LibSets.toList(names))(rangeFrom(1));
})();
})();
})();
})());
  return LibMaps.fromList(LibLists.concat(LibLists.map(renameGroup)(LibMaps.toList(groups))));
})();
})();
})();
})();
}

export function topologicalSortBindingMap(bindingMap: ReadonlyMap<Core.Name, Core.Term>): ReadonlyArray<ReadonlyArray<readonly [Core.Name, Core.Term]>> {
  return (() => {
  const bindings = LibMaps.toList(bindingMap);
  return (() => {
  const keys = LibSets.fromList(LibLists.map(LibPairs.first)(bindings));
  return (() => {
  const hasTypeAnnotation = ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => hasTypeAnnotation(((_x) => _x.body)(at)))((_m as any).value);
    default: return false(_m);
  }
})());
  return (() => {
  const depsOf = ((nameAndTerm: readonly [t0, Core.Term]) => (() => {
  const name = LibPairs.first(nameAndTerm);
  return (() => {
  const term = LibPairs.second(nameAndTerm);
  return [name, LibLogic.ifElse(hasTypeAnnotation(term))([])(LibSets.toList(LibSets.intersection(keys)(Variables.freeVariablesInTerm(term))))];
})();
})());
  return (() => {
  const toPair = ((name: Core.Name) => [name, LibMaybes.fromMaybe(({ tag: "literal", value: ({ tag: "string", value: "Impossible!" }) }))(LibMaps.lookup(name)(bindingMap))]);
  return LibLists.map(((v1) => LibLists.map(toPair)(v1)))(Sorting.topologicalSortComponents(LibLists.map(depsOf)(bindings)));
})();
})();
})();
})();
})();
}

export function topologicalSortBindings(els: ReadonlyArray<Core.Binding>): ReadonlyArray<ReadonlyArray<Core.Name>> | ReadonlyArray<Core.Name> {
  return (() => {
  const adjlist = ((e: Core.Binding) => [((_x) => _x.name)(e), LibSets.toList(termDependencyNames(false)(true)(true)(((_x) => _x.term)(e)))]);
  return Sorting.topologicalSort(LibLists.map(adjlist)(els));
})();
}

export function topologicalSortTypeDefinitions(defs: ReadonlyArray<Packaging.TypeDefinition>): ReadonlyArray<ReadonlyArray<Packaging.TypeDefinition>> {
  return (() => {
  const toPair = ((def: Packaging.TypeDefinition) => [((_x) => _x.name)(def), LibSets.toList(typeDependencyNames(false)(((_x) => _x.type)(((_x) => _x.type)(def))))]);
  return (() => {
  const nameToDef = LibMaps.fromList(LibLists.map(((d: Packaging.TypeDefinition) => [((_x) => _x.name)(d), d]))(defs));
  return (() => {
  const sorted = Sorting.topologicalSortComponents(LibLists.map(toPair)(defs));
  return LibLists.map(((names: ReadonlyArray<Core.Name>) => LibMaybes.cat(LibLists.map(((n: Core.Name) => LibMaps.lookup(n)(nameToDef)))(names))))(sorted);
})();
})();
})();
}

export function typeDependencyNames(withSchema: boolean): ((x: Core.Type) => ReadonlySet<Core.Name>) {
  return ((typ: Core.Type) => LibLogic.ifElse(withSchema)(LibSets.union(Variables.freeVariablesInType(typ))(typeNamesInType(typ)))(Variables.freeVariablesInType(typ)));
}

export function typeNamesInType<t0>(typ0: Core.Type): ReadonlySet<t0> {
  return (() => {
  const addNames = ((names: t1) => ((typ: t2) => names));
  return Rewriting.foldOverType(({ tag: "pre" }))(addNames)(LibSets.empty)(typ0);
})();
}
