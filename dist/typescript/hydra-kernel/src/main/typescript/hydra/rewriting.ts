// Note: this is an automatically generated file. Do not edit.

/**
 * Core rewrite and fold combinators for terms and types
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
import * as LibLists from "./lib/lists.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
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
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function applyInsideTypeLambdasAndAnnotations(f: ((x: Core.Term) => Core.Term)): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const _m = term0;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: applyInsideTypeLambdasAndAnnotations(f)(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: applyInsideTypeLambdasAndAnnotations(f)(((_x) => _x.body)(tl))
  }) }))((_m as any).value);
    default: return f(term0)(_m);
  }
})());
}

export function foldOverTerm<t0>(order: Coders.TraversalOrder): ((x: ((x: t0) => ((x: Core.Term) => t0))) => ((x: t0) => ((x: Core.Term) => t0))) {
  return ((fld: ((x: t0) => ((x: Core.Term) => t0))) => ((b0: t0) => ((term: Core.Term) => (() => {
  const _m = order;
  switch (_m.tag) {
    case "pre": return ((_: void) => LibLists.foldl(((v1: t0) => ((v2: Core.Term) => foldOverTerm(order)(fld)(v1)(v2))))(fld(b0)(term))(subterms(term)))((_m as any).value);
    case "post": return ((_: void) => fld(LibLists.foldl(((v1: t0) => ((v2: Core.Term) => foldOverTerm(order)(fld)(v1)(v2))))(b0)(subterms(term)))(term))((_m as any).value);
  }
})())));
}

export function foldOverType<t0>(order: Coders.TraversalOrder): ((x: ((x: t0) => ((x: Core.Type) => t0))) => ((x: t0) => ((x: Core.Type) => t0))) {
  return ((fld: ((x: t0) => ((x: Core.Type) => t0))) => ((b0: t0) => ((typ: Core.Type) => (() => {
  const _m = order;
  switch (_m.tag) {
    case "pre": return ((_: void) => LibLists.foldl(((v1: t0) => ((v2: Core.Type) => foldOverType(order)(fld)(v1)(v2))))(fld(b0)(typ))(subtypes(typ)))((_m as any).value);
    case "post": return ((_: void) => fld(LibLists.foldl(((v1: t0) => ((v2: Core.Type) => foldOverType(order)(fld)(v1)(v2))))(b0)(subtypes(typ)))(typ))((_m as any).value);
  }
})())));
}

export function foldTermWithGraphAndPath<t0>(f: ((x: ((x: t0) => ((x: Core.Term) => t0))) => ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: Graph.Graph) => ((x: t0) => ((x: Core.Term) => t0)))))): ((x: Graph.Graph) => ((x: t0) => ((x: Core.Term) => t0))) {
  return ((cx0: Graph.Graph) => ((val0: t0) => ((term0: Core.Term) => (() => {
  const wrapper = ((recurse: ((x: t0) => ((x: Core.Term) => readonly [t0, t1]))) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((cx: Graph.Graph) => ((val: t0) => ((term: Core.Term) => (() => {
  const recurseForUser = ((valIn: t0) => ((subterm: Core.Term) => (() => {
  const r = recurse(valIn)(subterm);
  return LibPairs.first(r);
})()));
  return [f(recurseForUser)(path)(cx)(val)(term), term];
})())))));
  return (() => {
  const result = rewriteAndFoldTermWithGraphAndPath(wrapper)(cx0)(val0)(term0);
  return LibPairs.first(result);
})();
})())));
}

export function mapBeneathTypeAnnotations(f: ((x: Core.Type) => Core.Type)): ((x: Core.Type) => Core.Type) {
  return ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => ({ tag: "annotated", value: ({
    body: mapBeneathTypeAnnotations(f)(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    default: return f(t)(_m);
  }
})());
}

export function rewriteAndFoldTerm<t0>(f: ((x: ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term])))): ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term])) {
  return ((term0: t0) => ((v1: Core.Term) => (() => {
  const fsub = ((recurse: ((x: t1) => ((x: Core.Term) => readonly [t1, Core.Term]))) => ((val0: t1) => ((term02: Core.Term) => (() => {
  const forSingle = ((rec: ((x: t2) => ((x: t3) => readonly [t4, t5]))) => ((cons: ((x: t5) => t6)) => ((val: t2) => ((term: t3) => (() => {
  const r = rec(val)(term);
  return [LibPairs.first(r), cons(LibPairs.second(r))];
})()))));
  return (() => {
  const forMany = ((rec: ((x: t2) => ((x: t3) => readonly [t2, t4]))) => ((cons: ((x: ReadonlyArray<t4>) => t5)) => ((val: t2) => ((els: ReadonlyArray<t3>) => (() => {
  const rr = LibLists.foldl(((r: readonly [t2, ReadonlyArray<t4>]) => ((el: t3) => (() => {
  const r2 = rec(LibPairs.first(r))(el);
  return [LibPairs.first(r2), LibLists.cons(LibPairs.second(r2))(LibPairs.second(r))];
})())))([val, []])(els);
  return [LibPairs.first(rr), cons(LibLists.reverse(LibPairs.second(rr)))];
})()))));
  return (() => {
  const forField = ((val: t1) => ((field: Core.Field) => (() => {
  const r = recurse(val)(((_x) => _x.term)(field));
  return [LibPairs.first(r), ({
    name: ((_x) => _x.name)(field),
    term: LibPairs.second(r)
  })];
})()));
  return (() => {
  const forFields = ((v1: t1) => ((v2: ReadonlyArray<Core.Field>) => forMany(forField)(((x: ReadonlyArray<Core.Field>) => x))(v1)(v2)));
  return (() => {
  const forPair = ((val: t1) => ((kv: readonly [Core.Term, Core.Term]) => (() => {
  const rk = recurse(val)(LibPairs.first(kv));
  return (() => {
  const rv = recurse(LibPairs.first(rk))(LibPairs.second(kv));
  return [LibPairs.first(rv), [LibPairs.second(rk), LibPairs.second(rv)]];
})();
})()));
  return (() => {
  const forBinding = ((val: t1) => ((binding: Core.Binding) => (() => {
  const r = recurse(val)(((_x) => _x.term)(binding));
  return [LibPairs.first(r), ({
    name: ((_x) => _x.name)(binding),
    term: LibPairs.second(r),
    type: ((_x) => _x.type)(binding)
  })];
})()));
  return (() => {
  const dflt = [val0, term02];
  return (() => {
  const _m = term02;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => forSingle(recurse)(((t: Core.Term) => ({ tag: "annotated", value: ({
    body: t,
    annotation: ((_x) => _x.annotation)(at)
  }) })))(val0)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((a: Core.Application) => (() => {
  const rlhs = recurse(val0)(((_x) => _x.function)(a));
  return (() => {
  const rrhs = recurse(LibPairs.first(rlhs))(((_x) => _x.argument)(a));
  return [LibPairs.first(rrhs), ({ tag: "application", value: ({
    function: LibPairs.second(rlhs),
    argument: LibPairs.second(rrhs)
  }) })];
})();
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const rmd = LibMaybes.map(((v1: Core.Term) => recurse(val0)(v1)))(((_x) => _x.default)(cs));
  return (() => {
  const val1 = LibMaybes.maybe(val0)(LibPairs.first)(rmd);
  return (() => {
  const rcases = forFields(val1)(((_x) => _x.cases)(cs));
  return [LibPairs.first(rcases), ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(LibPairs.second)(rmd),
    cases: LibPairs.second(rcases)
  }) })];
})();
})();
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => (() => {
  const rl = recurse(val0)(l);
  return [LibPairs.first(rl), ({ tag: "either", value: ({ tag: "left", value: LibPairs.second(rl) }) })];
})()))(((r: Core.Term) => (() => {
  const rr = recurse(val0)(r);
  return [LibPairs.first(rr), ({ tag: "either", value: ({ tag: "right", value: LibPairs.second(rr) }) })];
})()))(e))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const rl = recurse(val0)(((_x) => _x.body)(l));
  return [LibPairs.first(rl), ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: LibPairs.second(rl)
  }) })];
})())((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const renv = recurse(val0)(((_x) => _x.body)(l));
  return forMany(forBinding)(((bins: ReadonlyArray<Core.Binding>) => ({ tag: "let", value: ({
    bindings: bins,
    body: LibPairs.second(renv)
  }) })))(LibPairs.first(renv))(((_x) => _x.bindings)(l));
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => forMany(recurse)(((x: ReadonlyArray<Core.Term>) => ({ tag: "list", value: x })))(val0)(els))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => forMany(forPair)(((pairs: ReadonlyArray<readonly [Core.Term, Core.Term]>) => ({ tag: "map", value: LibMaps.fromList(pairs) })))(val0)(LibMaps.toList(m)))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe(dflt)(((t: Core.Term) => forSingle(recurse)(((t1: Core.Term) => ({ tag: "maybe", value: t1 })))(val0)(t)))(mt))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const rf = recurse(val0)(LibPairs.first(p));
  return (() => {
  const rs = recurse(LibPairs.first(rf))(LibPairs.second(p));
  return [LibPairs.first(rs), ({ tag: "pair", value: [LibPairs.second(rf), LibPairs.second(rs)] })];
})();
})())((_m as any).value);
    case "project": return ((p: Core.Projection) => [val0, ({ tag: "project", value: p })])((_m as any).value);
    case "record": return ((r: Core.Record) => forMany(forField)(((fields: ReadonlyArray<Core.Field>) => ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(r),
    fields: fields
  }) })))(val0)(((_x) => _x.fields)(r)))((_m as any).value);
    case "set": return ((els: ReadonlySet<Core.Term>) => forMany(recurse)(((e: ReadonlyArray<Core.Term>) => ({ tag: "set", value: LibSets.fromList(e) })))(val0)(LibSets.toList(els)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => forSingle(recurse)(((t: Core.Term) => ({ tag: "typeApplication", value: ({
    body: t,
    type: ((_x) => _x.type)(ta)
  }) })))(val0)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => forSingle(recurse)(((t: Core.Term) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: t
  }) })))(val0)(((_x) => _x.body)(tl)))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => forSingle(recurse)(((t: Core.Term) => ({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(inj),
    field: ({
    name: ((_x) => _x.name)(((_x) => _x.field)(inj)),
    term: t
  })
  }) })))(val0)(((_x) => _x.term)(((_x) => _x.field)(inj))))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => [val0, ({ tag: "unwrap", value: n })])((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => forSingle(recurse)(((t: Core.Term) => ({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: t
  }) })))(val0)(((_x) => _x.body)(wt)))((_m as any).value);
    default: return dflt(_m);
  }
})();
})();
})();
})();
})();
})();
})();
})())));
  return (() => {
  const recurse = ((v1: t0) => ((v2: Core.Term) => f(((v12: t0) => ((v22: Core.Term) => fsub(recurse)(v12)(v22))))(v1)(v2)));
  return recurse(term0)(v1);
})();
})()));
}

export function rewriteAndFoldTermWithGraph<t0>(f: ((x: ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))) => ((x: Graph.Graph) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))))): ((x: Graph.Graph) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))) {
  return ((cx0: Graph.Graph) => ((val0: t0) => ((term0: Core.Term) => (() => {
  const wrapper = ((lowLevelRecurse: ((x: readonly [t0, Graph.Graph]) => ((x: Core.Term) => readonly [readonly [t0, t1], Core.Term]))) => ((valAndCx: readonly [t0, Graph.Graph]) => ((term: Core.Term) => (() => {
  const val = LibPairs.first(valAndCx);
  return (() => {
  const cx = LibPairs.second(valAndCx);
  return (() => {
  const cx1 = (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => Scoping.extendGraphForLambda(cx)(l))((_m as any).value);
    case "let": return ((l: Core.Let) => Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(cx)(l))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => Scoping.extendGraphForTypeLambda(cx)(tl))((_m as any).value);
    default: return cx(_m);
  }
})();
  return (() => {
  const recurseForUser = ((newVal: t0) => ((subterm: Core.Term) => (() => {
  const result = lowLevelRecurse([newVal, cx1])(subterm);
  return [LibPairs.first(LibPairs.first(result)), LibPairs.second(result)];
})()));
  return (() => {
  const fResult = f(recurseForUser)(cx1)(val)(term);
  return [[LibPairs.first(fResult), cx], LibPairs.second(fResult)];
})();
})();
})();
})();
})())));
  return (() => {
  const result = rewriteAndFoldTerm(wrapper)([val0, cx0])(term0);
  return [LibPairs.first(LibPairs.first(result)), LibPairs.second(result)];
})();
})())));
}

export function rewriteAndFoldTermWithGraphAndPath<t0>(f: ((x: ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))) => ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: Graph.Graph) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term])))))): ((x: Graph.Graph) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))) {
  return ((cx0: Graph.Graph) => ((val0: t0) => ((term0: Core.Term) => (() => {
  const wrapper = ((recurse: ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: readonly [Graph.Graph, t0]) => ((x: Core.Term) => readonly [readonly [t1, t0], Core.Term])))) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((cxAndVal: readonly [Graph.Graph, t0]) => ((term: Core.Term) => (() => {
  const cx = LibPairs.first(cxAndVal);
  return (() => {
  const val = LibPairs.second(cxAndVal);
  return (() => {
  const cx1 = (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => Scoping.extendGraphForLambda(cx)(l))((_m as any).value);
    case "let": return ((l: Core.Let) => Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(cx)(l))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => Scoping.extendGraphForTypeLambda(cx)(tl))((_m as any).value);
    default: return cx(_m);
  }
})();
  return (() => {
  const recurseForUser = ((valIn: t0) => ((termIn: Core.Term) => (() => {
  const result = recurse(path)([cx1, valIn])(termIn);
  return [LibPairs.second(LibPairs.first(result)), LibPairs.second(result)];
})()));
  return (() => {
  const fResult = f(recurseForUser)(path)(cx1)(val)(term);
  return [[cx, LibPairs.first(fResult)], LibPairs.second(fResult)];
})();
})();
})();
})();
})()))));
  return (() => {
  const result = rewriteAndFoldTermWithPath(wrapper)([cx0, val0])(term0);
  return [LibPairs.second(LibPairs.first(result)), LibPairs.second(result)];
})();
})())));
}

export function rewriteAndFoldTermWithPath<t0>(f: ((x: ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term])))) => ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term]))))): ((x: t0) => ((x: Core.Term) => readonly [t0, Core.Term])) {
  return ((term0: t0) => ((v1: Core.Term) => (() => {
  const fsub = ((recurse: ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: t1) => ((x: Core.Term) => readonly [t1, Core.Term])))) => ((path: ReadonlyArray<Paths.SubtermStep>) => ((val0: t1) => ((term02: Core.Term) => (() => {
  const forSingleWithAccessor = ((rec: ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: t2) => ((x: t3) => readonly [t4, t5])))) => ((cons: ((x: t5) => t6)) => ((accessor: Paths.SubtermStep) => ((val: t2) => ((term: t3) => (() => {
  const r = rec(LibLists.concat2(path)([accessor]))(val)(term);
  return [LibPairs.first(r), cons(LibPairs.second(r))];
})())))));
  return (() => {
  const forManyWithAccessors = ((rec: ((x: ReadonlyArray<Paths.SubtermStep>) => ((x: t2) => ((x: t3) => readonly [t2, t4])))) => ((cons: ((x: ReadonlyArray<t4>) => t5)) => ((val: t2) => ((accessorTermPairs: ReadonlyArray<readonly [Paths.SubtermStep, t3]>) => (() => {
  const rr = LibLists.foldl(((r: readonly [t2, ReadonlyArray<t4>]) => ((atp: readonly [Paths.SubtermStep, t3]) => (() => {
  const r2 = rec(LibLists.concat2(path)([LibPairs.first(atp)]))(LibPairs.first(r))(LibPairs.second(atp));
  return [LibPairs.first(r2), LibLists.cons(LibPairs.second(r2))(LibPairs.second(r))];
})())))([val, []])(accessorTermPairs);
  return [LibPairs.first(rr), cons(LibLists.reverse(LibPairs.second(rr)))];
})()))));
  return (() => {
  const forFieldWithAccessor = ((mkAccessor: ((x: Core.Name) => Paths.SubtermStep)) => ((val: t1) => ((field: Core.Field) => (() => {
  const r = recurse(LibLists.concat2(path)([mkAccessor(((_x) => _x.name)(field))]))(val)(((_x) => _x.term)(field));
  return [LibPairs.first(r), ({
    name: ((_x) => _x.name)(field),
    term: LibPairs.second(r)
  })];
})())));
  return (() => {
  const forFieldsWithAccessor = ((mkAccessor: ((x: Core.Name) => Paths.SubtermStep)) => ((v1: t1) => ((v2: ReadonlyArray<readonly [Paths.SubtermStep, Core.Field]>) => forManyWithAccessors(((path1: ReadonlyArray<Paths.SubtermStep>) => ((val1: t1) => ((field1: Core.Field) => forFieldWithAccessor(mkAccessor)(val1)(field1)))))(((x: ReadonlyArray<Core.Field>) => x))(v1)(v2))));
  return (() => {
  const forPairWithAccessors = ((keyAccessor: Paths.SubtermStep) => ((valAccessor: Paths.SubtermStep) => ((val: t1) => ((kv: readonly [Core.Term, Core.Term]) => (() => {
  const rk = recurse(LibLists.concat2(path)([keyAccessor]))(val)(LibPairs.first(kv));
  return (() => {
  const rv = recurse(LibLists.concat2(path)([valAccessor]))(LibPairs.first(rk))(LibPairs.second(kv));
  return [LibPairs.first(rv), [LibPairs.second(rk), LibPairs.second(rv)]];
})();
})()))));
  return (() => {
  const forBindingWithAccessor = ((val: t1) => ((binding: Core.Binding) => (() => {
  const r = recurse(LibLists.concat2(path)([({ tag: "letBinding", value: ((_x) => _x.name)(binding) })]))(val)(((_x) => _x.term)(binding));
  return [LibPairs.first(r), ({
    name: ((_x) => _x.name)(binding),
    term: LibPairs.second(r),
    type: ((_x) => _x.type)(binding)
  })];
})()));
  return (() => {
  const dflt = [val0, term02];
  return (() => {
  const _m = term02;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => forSingleWithAccessor(recurse)(((t: Core.Term) => ({ tag: "annotated", value: ({
    body: t,
    annotation: ((_x) => _x.annotation)(at)
  }) })))(({ tag: "annotatedBody" }))(val0)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((a: Core.Application) => (() => {
  const rlhs = recurse(LibLists.concat2(path)([({ tag: "applicationFunction" })]))(val0)(((_x) => _x.function)(a));
  return (() => {
  const rrhs = recurse(LibLists.concat2(path)([({ tag: "applicationArgument" })]))(LibPairs.first(rlhs))(((_x) => _x.argument)(a));
  return [LibPairs.first(rrhs), ({ tag: "application", value: ({
    function: LibPairs.second(rlhs),
    argument: LibPairs.second(rrhs)
  }) })];
})();
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const rmd = LibMaybes.map(((def: Core.Term) => recurse(LibLists.concat2(path)([({ tag: "unionCasesDefault" })]))(val0)(def)))(((_x) => _x.default)(cs));
  return (() => {
  const val1 = LibMaybes.maybe(val0)(LibPairs.first)(rmd);
  return (() => {
  const rcases = forManyWithAccessors(recurse)(((x: ReadonlyArray<Core.Term>) => x))(val1)(LibLists.map(((f2: Core.Field) => [({ tag: "unionCasesBranch", value: ((_x) => _x.name)(f2) }), ((_x) => _x.term)(f2)]))(((_x) => _x.cases)(cs)));
  return [LibPairs.first(rcases), ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(LibPairs.second)(rmd),
    cases: LibLists.map(((ft: readonly [Core.Name, Core.Term]) => ({
    name: LibPairs.first(ft),
    term: LibPairs.second(ft)
  })))(LibLists.zip(LibLists.map(((_x) => _x.name))(((_x) => _x.cases)(cs)))(LibPairs.second(rcases)))
  }) })];
})();
})();
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => (() => {
  const rl = recurse(LibLists.concat2(path)([({ tag: "sumTerm" })]))(val0)(l);
  return [LibPairs.first(rl), ({ tag: "either", value: ({ tag: "left", value: LibPairs.second(rl) }) })];
})()))(((r: Core.Term) => (() => {
  const rr = recurse(LibLists.concat2(path)([({ tag: "sumTerm" })]))(val0)(r);
  return [LibPairs.first(rr), ({ tag: "either", value: ({ tag: "right", value: LibPairs.second(rr) }) })];
})()))(e))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const rl = recurse(LibLists.concat2(path)([({ tag: "lambdaBody" })]))(val0)(((_x) => _x.body)(l));
  return [LibPairs.first(rl), ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: LibPairs.second(rl)
  }) })];
})())((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const renv = recurse(LibLists.concat2(path)([({ tag: "letBody" })]))(val0)(((_x) => _x.body)(l));
  return (() => {
  const rbindings = LibLists.foldl(((r: readonly [t1, ReadonlyArray<Core.Binding>]) => ((binding: Core.Binding) => (() => {
  const rb = forBindingWithAccessor(LibPairs.first(r))(binding);
  return [LibPairs.first(rb), LibLists.cons(LibPairs.second(rb))(LibPairs.second(r))];
})())))([LibPairs.first(renv), []])(((_x) => _x.bindings)(l));
  return [LibPairs.first(rbindings), ({ tag: "let", value: ({
    bindings: LibLists.reverse(LibPairs.second(rbindings)),
    body: LibPairs.second(renv)
  }) })];
})();
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => (() => {
  const idx = 0;
  return (() => {
  const rr = LibLists.foldl(((r: readonly [number, readonly [t1, ReadonlyArray<Core.Term>]]) => ((el: Core.Term) => (() => {
  const r2 = recurse(LibLists.concat2(path)([({ tag: "listElement", value: LibPairs.first(r) })]))(LibPairs.first(LibPairs.second(r)))(el);
  return [LibMath.add(LibPairs.first(r))(1), [LibPairs.first(r2), LibLists.cons(LibPairs.second(r2))(LibPairs.second(LibPairs.second(r)))]];
})())))([idx, [val0, []]])(els);
  return [LibPairs.first(LibPairs.second(rr)), ({ tag: "list", value: LibLists.reverse(LibPairs.second(LibPairs.second(rr))) })];
})();
})())((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const idx = 0;
  return (() => {
  const rr = LibLists.foldl(((r: readonly [number, readonly [t1, ReadonlyArray<readonly [Core.Term, Core.Term]>]]) => ((kv: readonly [Core.Term, Core.Term]) => (() => {
  const rk = recurse(LibLists.concat2(path)([({ tag: "mapKey", value: LibPairs.first(r) })]))(LibPairs.first(LibPairs.second(r)))(LibPairs.first(kv));
  return (() => {
  const rv = recurse(LibLists.concat2(path)([({ tag: "mapValue", value: LibPairs.first(r) })]))(LibPairs.first(rk))(LibPairs.second(kv));
  return [LibMath.add(LibPairs.first(r))(1), [LibPairs.first(rv), LibLists.cons([LibPairs.second(rk), LibPairs.second(rv)])(LibPairs.second(LibPairs.second(r)))]];
})();
})())))([idx, [val0, []]])(LibMaps.toList(m));
  return [LibPairs.first(LibPairs.second(rr)), ({ tag: "map", value: LibMaps.fromList(LibLists.reverse(LibPairs.second(LibPairs.second(rr)))) })];
})();
})())((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe(dflt)(((t: Core.Term) => forSingleWithAccessor(recurse)(((t1: Core.Term) => ({ tag: "maybe", value: t1 })))(({ tag: "maybeTerm" }))(val0)(t)))(mt))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const rf = recurse(LibLists.concat2(path)([({ tag: "productTerm", value: 0 })]))(val0)(LibPairs.first(p));
  return (() => {
  const rs = recurse(LibLists.concat2(path)([({ tag: "productTerm", value: 1 })]))(LibPairs.first(rf))(LibPairs.second(p));
  return [LibPairs.first(rs), ({ tag: "pair", value: [LibPairs.second(rf), LibPairs.second(rs)] })];
})();
})())((_m as any).value);
    case "project": return ((p: Core.Projection) => [val0, ({ tag: "project", value: p })])((_m as any).value);
    case "record": return ((r: Core.Record) => (() => {
  const rfields = forManyWithAccessors(recurse)(((x: ReadonlyArray<Core.Term>) => x))(val0)(LibLists.map(((f2: Core.Field) => [({ tag: "recordField", value: ((_x) => _x.name)(f2) }), ((_x) => _x.term)(f2)]))(((_x) => _x.fields)(r)));
  return [LibPairs.first(rfields), ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(r),
    fields: LibLists.map(((ft: readonly [Core.Name, Core.Term]) => ({
    name: LibPairs.first(ft),
    term: LibPairs.second(ft)
  })))(LibLists.zip(LibLists.map(((_x) => _x.name))(((_x) => _x.fields)(r)))(LibPairs.second(rfields)))
  }) })];
})())((_m as any).value);
    case "set": return ((els: ReadonlySet<Core.Term>) => (() => {
  const idx = 0;
  return (() => {
  const rr = LibLists.foldl(((r: readonly [number, readonly [t1, ReadonlyArray<Core.Term>]]) => ((el: Core.Term) => (() => {
  const r2 = recurse(LibLists.concat2(path)([({ tag: "setElement", value: LibPairs.first(r) })]))(LibPairs.first(LibPairs.second(r)))(el);
  return [LibMath.add(LibPairs.first(r))(1), [LibPairs.first(r2), LibLists.cons(LibPairs.second(r2))(LibPairs.second(LibPairs.second(r)))]];
})())))([idx, [val0, []]])(LibSets.toList(els));
  return [LibPairs.first(LibPairs.second(rr)), ({ tag: "set", value: LibSets.fromList(LibLists.reverse(LibPairs.second(LibPairs.second(rr)))) })];
})();
})())((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => forSingleWithAccessor(recurse)(((t: Core.Term) => ({ tag: "typeApplication", value: ({
    body: t,
    type: ((_x) => _x.type)(ta)
  }) })))(({ tag: "typeApplicationTerm" }))(val0)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => forSingleWithAccessor(recurse)(((t: Core.Term) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: t
  }) })))(({ tag: "typeLambdaBody" }))(val0)(((_x) => _x.body)(tl)))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => forSingleWithAccessor(recurse)(((t: Core.Term) => ({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(inj),
    field: ({
    name: ((_x) => _x.name)(((_x) => _x.field)(inj)),
    term: t
  })
  }) })))(({ tag: "injectionTerm" }))(val0)(((_x) => _x.term)(((_x) => _x.field)(inj))))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => [val0, ({ tag: "unwrap", value: n })])((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => forSingleWithAccessor(recurse)(((t: Core.Term) => ({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: t
  }) })))(({ tag: "wrappedTerm" }))(val0)(((_x) => _x.body)(wt)))((_m as any).value);
    default: return dflt(_m);
  }
})();
})();
})();
})();
})();
})();
})();
})()))));
  return (() => {
  const recurse = ((v1: ReadonlyArray<Paths.SubtermStep>) => ((v2: t0) => ((v3: Core.Term) => f(((v12: ReadonlyArray<Paths.SubtermStep>) => ((v22: t0) => ((v32: Core.Term) => fsub(recurse)(v12)(v22)(v32)))))(v1)(v2)(v3))));
  return recurse([])(term0)(v1);
})();
})()));
}

export function rewriteTerm(f: ((x: ((x: Core.Term) => Core.Term)) => ((x: Core.Term) => Core.Term))): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const fsub = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const forField = ((f2: Core.Field) => ({
    name: ((_x) => _x.name)(f2),
    term: recurse(((_x) => _x.term)(f2))
  }));
  return (() => {
  const forLet = ((lt: Core.Let) => (() => {
  const mapBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: recurse(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return ({
    bindings: LibLists.map(mapBinding)(((_x) => _x.bindings)(lt)),
    body: recurse(((_x) => _x.body)(lt))
  });
})());
  return (() => {
  const forMap = ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const forPair = ((p: readonly [Core.Term, Core.Term]) => [recurse(LibPairs.first(p)), recurse(LibPairs.second(p))]);
  return LibMaps.fromList(LibLists.map(forPair)(LibMaps.toList(m)));
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: recurse(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "application": return ((a: Core.Application) => ({ tag: "application", value: ({
    function: recurse(((_x) => _x.function)(a)),
    argument: recurse(((_x) => _x.argument)(a))
  }) }))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(recurse)(((_x) => _x.default)(cs)),
    cases: LibLists.map(forField)(((_x) => _x.cases)(cs))
  }) }))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "either", value: LibEithers.either(((l: Core.Term) => ({ tag: "left", value: recurse(l) })))(((r: Core.Term) => ({ tag: "right", value: recurse(r) })))(e) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: recurse(((_x) => _x.body)(l))
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: forLet(lt) }))((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => ({ tag: "list", value: LibLists.map(recurse)(els) }))((_m as any).value);
    case "literal": return ((v: Core.Literal) => ({ tag: "literal", value: v }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "map", value: forMap(m) }))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => ({ tag: "maybe", value: LibMaybes.map(recurse)(m) }))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "pair", value: [recurse(LibPairs.first(p)), recurse(LibPairs.second(p))] }))((_m as any).value);
    case "project": return ((p: Core.Projection) => ({ tag: "project", value: p }))((_m as any).value);
    case "record": return ((r: Core.Record) => ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(r),
    fields: LibLists.map(forField)(((_x) => _x.fields)(r))
  }) }))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => ({ tag: "set", value: LibSets.fromList(LibLists.map(recurse)(LibSets.toList(s))) }))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: recurse(((_x) => _x.body)(tt)),
    type: ((_x) => _x.type)(tt)
  }) }))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(ta),
    body: recurse(((_x) => _x.body)(ta))
  }) }))((_m as any).value);
    case "inject": return ((i: Core.Injection) => ({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(i),
    field: forField(((_x) => _x.field)(i))
  }) }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => ({ tag: "unwrap", value: n }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "variable", value: v }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => ({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: recurse(((_x) => _x.body)(wt))
  }) }))((_m as any).value);
  }
})();
})();
})();
})()));
  return (() => {
  const recurse = ((v1: Core.Term) => f(((v12: Core.Term) => fsub(recurse)(v12)))(v1));
  return recurse(term0);
})();
})());
}

export function rewriteTermM<t0>(f: ((x: ((x: Core.Term) => t0 | Core.Term)) => ((x: Core.Term) => t0 | Core.Term))): ((x: Core.Term) => t0 | Core.Term) {
  return ((term0: Core.Term) => (() => {
  const fsub = ((recurse: ((x: Core.Term) => t1 | Core.Term)) => ((term: Core.Term) => (() => {
  const forField = ((field: Core.Field) => LibEithers.bind(recurse(((_x) => _x.term)(field)))(((t: Core.Term) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(field),
    term: t
  }) }))));
  return (() => {
  const forPair = ((kv: readonly [Core.Term, Core.Term]) => LibEithers.bind(recurse(LibPairs.first(kv)))(((k: Core.Term) => LibEithers.bind(recurse(LibPairs.second(kv)))(((v: Core.Term) => ({ tag: "right", value: [k, v] }))))));
  return (() => {
  const mapBinding = ((b: Core.Binding) => LibEithers.bind(recurse(((_x) => _x.term)(b)))(((v: Core.Term) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(b),
    term: v,
    type: ((_x) => _x.type)(b)
  }) }))));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => LibEithers.bind(recurse(((_x) => _x.body)(at)))(((ex: Core.Term) => ({ tag: "right", value: ({ tag: "annotated", value: ({
    body: ex,
    annotation: ((_x) => _x.annotation)(at)
  }) }) }))))((_m as any).value);
    case "application": return ((app: Core.Application) => LibEithers.bind(recurse(((_x) => _x.function)(app)))(((lhs: Core.Term) => LibEithers.bind(recurse(((_x) => _x.argument)(app)))(((rhs: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: lhs,
    argument: rhs
  }) }) }))))))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const n = ((_x) => _x.typeName)(cs);
  return (() => {
  const def = ((_x) => _x.default)(cs);
  return (() => {
  const csCases = ((_x) => _x.cases)(cs);
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((t: Core.Term) => LibEithers.map(LibMaybes.pure)(recurse(t))))(def))(((rdef: Core.Term | null) => LibEithers.map(((rcases: ReadonlyArray<Core.Field>) => ({ tag: "cases", value: ({
    typeName: n,
    default: rdef,
    cases: rcases
  }) })))(LibEithers.mapList(forField)(csCases))));
})();
})();
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.bind(LibEithers.either(((l: Core.Term) => LibEithers.map(((x: Core.Term) => ({ tag: "left", value: x })))(recurse(l))))(((r: Core.Term) => LibEithers.map(((x: Core.Term) => ({ tag: "right", value: x })))(recurse(r))))(e))(((re: Core.Term | Core.Term) => ({ tag: "right", value: ({ tag: "either", value: re }) }))))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return (() => {
  const d = ((_x) => _x.domain)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return LibEithers.bind(recurse(body))(((rbody: Core.Term) => ({ tag: "right", value: ({ tag: "lambda", value: ({
    parameter: v,
    domain: d,
    body: rbody
  }) }) })));
})();
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const env = ((_x) => _x.body)(lt);
  return LibEithers.bind(LibEithers.mapList(mapBinding)(bindings))(((rbindings: ReadonlyArray<Core.Binding>) => LibEithers.bind(recurse(env))(((renv: Core.Term) => ({ tag: "right", value: ({ tag: "let", value: ({
    bindings: rbindings,
    body: renv
  }) }) })))));
})();
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(recurse)(els))(((rels: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "list", value: rels }) }))))((_m as any).value);
    case "literal": return ((v: Core.Literal) => ({ tag: "right", value: ({ tag: "literal", value: v }) }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.bind(LibEithers.mapList(forPair)(LibMaps.toList(m)))(((pairs: ReadonlyArray<readonly [Core.Term, Core.Term]>) => ({ tag: "right", value: ({ tag: "map", value: LibMaps.fromList(pairs) }) }))))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibEithers.bind(LibEithers.mapMaybe(recurse)(m))(((rm: Core.Term | null) => ({ tag: "right", value: ({ tag: "maybe", value: rm }) }))))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(recurse(LibPairs.first(p)))(((rf: Core.Term) => LibEithers.bind(recurse(LibPairs.second(p)))(((rs: Core.Term) => ({ tag: "right", value: ({ tag: "pair", value: [rf, rs] }) }))))))((_m as any).value);
    case "project": return ((p: Core.Projection) => ({ tag: "right", value: ({ tag: "project", value: p }) }))((_m as any).value);
    case "record": return ((r: Core.Record) => (() => {
  const n = ((_x) => _x.typeName)(r);
  return (() => {
  const fields = ((_x) => _x.fields)(r);
  return LibEithers.map(((rfields: ReadonlyArray<Core.Field>) => ({ tag: "record", value: ({
    typeName: n,
    fields: rfields
  }) })))(LibEithers.mapList(forField)(fields));
})();
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibEithers.bind(LibEithers.mapList(recurse)(LibSets.toList(s)))(((rlist: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "set", value: LibSets.fromList(rlist) }) }))))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => LibEithers.bind(recurse(((_x) => _x.body)(tt)))(((t: Core.Term) => ({ tag: "right", value: ({ tag: "typeApplication", value: ({
    body: t,
    type: ((_x) => _x.type)(tt)
  }) }) }))))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const v = ((_x) => _x.parameter)(tl);
  return (() => {
  const body = ((_x) => _x.body)(tl);
  return LibEithers.bind(recurse(body))(((rbody: Core.Term) => ({ tag: "right", value: ({ tag: "typeLambda", value: ({
    parameter: v,
    body: rbody
  }) }) })));
})();
})())((_m as any).value);
    case "inject": return ((i: Core.Injection) => (() => {
  const n = ((_x) => _x.typeName)(i);
  return (() => {
  const field = ((_x) => _x.field)(i);
  return LibEithers.map(((rfield: Core.Field) => ({ tag: "inject", value: ({
    typeName: n,
    field: rfield
  }) })))(forField(field));
})();
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "unit" }) }))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => ({ tag: "right", value: ({ tag: "unwrap", value: n }) }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "right", value: ({ tag: "variable", value: v }) }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const name = ((_x) => _x.typeName)(wt);
  return (() => {
  const t = ((_x) => _x.body)(wt);
  return LibEithers.bind(recurse(t))(((rt: Core.Term) => ({ tag: "right", value: ({ tag: "wrap", value: ({
    typeName: name,
    body: rt
  }) }) })));
})();
})())((_m as any).value);
  }
})();
})();
})();
})()));
  return (() => {
  const recurse = ((v1: Core.Term) => f(((v12: Core.Term) => fsub(recurse)(v12)))(v1));
  return recurse(term0);
})();
})());
}

export function rewriteTermWithContext<t0>(f: ((x: ((x: t0) => ((x: Core.Term) => Core.Term))) => ((x: t0) => ((x: Core.Term) => Core.Term)))): ((x: t0) => ((x: Core.Term) => Core.Term)) {
  return ((cx0: t0) => ((term0: Core.Term) => (() => {
  const forSubterms = ((recurse0: ((x: t1) => ((x: Core.Term) => Core.Term))) => ((cx: t1) => ((term: Core.Term) => (() => {
  const recurse = ((v1: Core.Term) => recurse0(cx)(v1));
  return (() => {
  const forField = ((field: Core.Field) => ({
    name: ((_x) => _x.name)(field),
    term: recurse(((_x) => _x.term)(field))
  }));
  return (() => {
  const forLet = ((lt: Core.Let) => (() => {
  const mapBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: recurse(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return ({
    bindings: LibLists.map(mapBinding)(((_x) => _x.bindings)(lt)),
    body: recurse(((_x) => _x.body)(lt))
  });
})());
  return (() => {
  const forMap = ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const forPair = ((p: readonly [Core.Term, Core.Term]) => [recurse(LibPairs.first(p)), recurse(LibPairs.second(p))]);
  return LibMaps.fromList(LibLists.map(forPair)(LibMaps.toList(m)));
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: recurse(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "application": return ((a: Core.Application) => ({ tag: "application", value: ({
    function: recurse(((_x) => _x.function)(a)),
    argument: recurse(((_x) => _x.argument)(a))
  }) }))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(recurse)(((_x) => _x.default)(cs)),
    cases: LibLists.map(forField)(((_x) => _x.cases)(cs))
  }) }))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "either", value: LibEithers.either(((l: Core.Term) => ({ tag: "left", value: recurse(l) })))(((r: Core.Term) => ({ tag: "right", value: recurse(r) })))(e) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: recurse(((_x) => _x.body)(l))
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: forLet(lt) }))((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => ({ tag: "list", value: LibLists.map(recurse)(els) }))((_m as any).value);
    case "literal": return ((v: Core.Literal) => ({ tag: "literal", value: v }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "map", value: forMap(m) }))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => ({ tag: "maybe", value: LibMaybes.map(recurse)(m) }))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "pair", value: [recurse(LibPairs.first(p)), recurse(LibPairs.second(p))] }))((_m as any).value);
    case "project": return ((p: Core.Projection) => ({ tag: "project", value: p }))((_m as any).value);
    case "record": return ((r: Core.Record) => ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(r),
    fields: LibLists.map(forField)(((_x) => _x.fields)(r))
  }) }))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => ({ tag: "set", value: LibSets.fromList(LibLists.map(recurse)(LibSets.toList(s))) }))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: recurse(((_x) => _x.body)(tt)),
    type: ((_x) => _x.type)(tt)
  }) }))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(ta),
    body: recurse(((_x) => _x.body)(ta))
  }) }))((_m as any).value);
    case "inject": return ((i: Core.Injection) => ({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(i),
    field: forField(((_x) => _x.field)(i))
  }) }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => ({ tag: "unwrap", value: n }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "variable", value: v }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => ({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: recurse(((_x) => _x.body)(wt))
  }) }))((_m as any).value);
  }
})();
})();
})();
})();
})())));
  return (() => {
  const rewrite = ((cx: t0) => ((term: Core.Term) => f(((v1: t0) => ((v2: Core.Term) => forSubterms(rewrite)(v1)(v2))))(cx)(term)));
  return rewrite(cx0)(term0);
})();
})()));
}

export function rewriteTermWithContextM<t0, t1>(f: ((x: ((x: t0) => ((x: Core.Term) => t1 | Core.Term))) => ((x: t0) => ((x: Core.Term) => t1 | Core.Term)))): ((x: t0) => ((x: Core.Term) => t1 | Core.Term)) {
  return ((cx0: t0) => ((term0: Core.Term) => (() => {
  const forSubterms = ((recurse0: ((x: t2) => ((x: Core.Term) => t3 | Core.Term))) => ((cx: t2) => ((term: Core.Term) => (() => {
  const recurse = ((v1: Core.Term) => recurse0(cx)(v1));
  return (() => {
  const forField = ((field: Core.Field) => LibEithers.bind(recurse(((_x) => _x.term)(field)))(((t: Core.Term) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(field),
    term: t
  }) }))));
  return (() => {
  const forPair = ((kv: readonly [Core.Term, Core.Term]) => LibEithers.bind(recurse(LibPairs.first(kv)))(((k: Core.Term) => LibEithers.bind(recurse(LibPairs.second(kv)))(((v: Core.Term) => ({ tag: "right", value: [k, v] }))))));
  return (() => {
  const mapBinding = ((b: Core.Binding) => LibEithers.bind(recurse(((_x) => _x.term)(b)))(((v: Core.Term) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(b),
    term: v,
    type: ((_x) => _x.type)(b)
  }) }))));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => LibEithers.bind(recurse(((_x) => _x.body)(at)))(((ex: Core.Term) => ({ tag: "right", value: ({ tag: "annotated", value: ({
    body: ex,
    annotation: ((_x) => _x.annotation)(at)
  }) }) }))))((_m as any).value);
    case "application": return ((app: Core.Application) => LibEithers.bind(recurse(((_x) => _x.function)(app)))(((lhs: Core.Term) => LibEithers.bind(recurse(((_x) => _x.argument)(app)))(((rhs: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: lhs,
    argument: rhs
  }) }) }))))))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const n = ((_x) => _x.typeName)(cs);
  return (() => {
  const def = ((_x) => _x.default)(cs);
  return (() => {
  const csCases = ((_x) => _x.cases)(cs);
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((t: Core.Term) => LibEithers.map(LibMaybes.pure)(recurse(t))))(def))(((rdef: Core.Term | null) => LibEithers.map(((rcases: ReadonlyArray<Core.Field>) => ({ tag: "cases", value: ({
    typeName: n,
    default: rdef,
    cases: rcases
  }) })))(LibEithers.mapList(forField)(csCases))));
})();
})();
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.bind(LibEithers.either(((l: Core.Term) => LibEithers.map(((x: Core.Term) => ({ tag: "left", value: x })))(recurse(l))))(((r: Core.Term) => LibEithers.map(((x: Core.Term) => ({ tag: "right", value: x })))(recurse(r))))(e))(((re: Core.Term | Core.Term) => ({ tag: "right", value: ({ tag: "either", value: re }) }))))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return (() => {
  const d = ((_x) => _x.domain)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return LibEithers.bind(recurse(body))(((rbody: Core.Term) => ({ tag: "right", value: ({ tag: "lambda", value: ({
    parameter: v,
    domain: d,
    body: rbody
  }) }) })));
})();
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return LibEithers.bind(LibEithers.mapList(mapBinding)(bindings))(((rbindings: ReadonlyArray<Core.Binding>) => LibEithers.bind(recurse(body))(((rbody: Core.Term) => ({ tag: "right", value: ({ tag: "let", value: ({
    bindings: rbindings,
    body: rbody
  }) }) })))));
})();
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(recurse)(els))(((rels: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "list", value: rels }) }))))((_m as any).value);
    case "literal": return ((v: Core.Literal) => ({ tag: "right", value: ({ tag: "literal", value: v }) }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.bind(LibEithers.mapList(forPair)(LibMaps.toList(m)))(((pairs: ReadonlyArray<readonly [Core.Term, Core.Term]>) => ({ tag: "right", value: ({ tag: "map", value: LibMaps.fromList(pairs) }) }))))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibEithers.bind(LibEithers.mapMaybe(recurse)(m))(((rm: Core.Term | null) => ({ tag: "right", value: ({ tag: "maybe", value: rm }) }))))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(recurse(LibPairs.first(p)))(((rfirst: Core.Term) => LibEithers.bind(recurse(LibPairs.second(p)))(((rsecond: Core.Term) => ({ tag: "right", value: ({ tag: "pair", value: [rfirst, rsecond] }) }))))))((_m as any).value);
    case "project": return ((p: Core.Projection) => ({ tag: "right", value: ({ tag: "project", value: p }) }))((_m as any).value);
    case "record": return ((r: Core.Record) => (() => {
  const n = ((_x) => _x.typeName)(r);
  return (() => {
  const fields = ((_x) => _x.fields)(r);
  return LibEithers.map(((rfields: ReadonlyArray<Core.Field>) => ({ tag: "record", value: ({
    typeName: n,
    fields: rfields
  }) })))(LibEithers.mapList(forField)(fields));
})();
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibEithers.bind(LibEithers.mapList(recurse)(LibSets.toList(s)))(((rlist: ReadonlyArray<Core.Term>) => ({ tag: "right", value: ({ tag: "set", value: LibSets.fromList(rlist) }) }))))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => LibEithers.bind(recurse(((_x) => _x.body)(tt)))(((t: Core.Term) => ({ tag: "right", value: ({ tag: "typeApplication", value: ({
    body: t,
    type: ((_x) => _x.type)(tt)
  }) }) }))))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const v = ((_x) => _x.parameter)(tl);
  return (() => {
  const body = ((_x) => _x.body)(tl);
  return LibEithers.bind(recurse(body))(((rbody: Core.Term) => ({ tag: "right", value: ({ tag: "typeLambda", value: ({
    parameter: v,
    body: rbody
  }) }) })));
})();
})())((_m as any).value);
    case "inject": return ((i: Core.Injection) => (() => {
  const n = ((_x) => _x.typeName)(i);
  return (() => {
  const field = ((_x) => _x.field)(i);
  return LibEithers.map(((rfield: Core.Field) => ({ tag: "inject", value: ({
    typeName: n,
    field: rfield
  }) })))(forField(field));
})();
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "unit" }) }))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => ({ tag: "right", value: ({ tag: "unwrap", value: n }) }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "right", value: ({ tag: "variable", value: v }) }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const name = ((_x) => _x.typeName)(wt);
  return (() => {
  const t = ((_x) => _x.body)(wt);
  return LibEithers.bind(recurse(t))(((rt: Core.Term) => ({ tag: "right", value: ({ tag: "wrap", value: ({
    typeName: name,
    body: rt
  }) }) })));
})();
})())((_m as any).value);
  }
})();
})();
})();
})();
})())));
  return (() => {
  const rewrite = ((cx: t0) => ((term: Core.Term) => f(((v1: t0) => ((v2: Core.Term) => forSubterms(rewrite)(v1)(v2))))(cx)(term)));
  return rewrite(cx0)(term0);
})();
})()));
}

export function rewriteTermWithGraph<t0>(f: ((x: ((x: Core.Term) => t0)) => ((x: Graph.Graph) => ((x: Core.Term) => t0)))): ((x: Graph.Graph) => ((x: Core.Term) => t0)) {
  return ((cx0: Graph.Graph) => ((term0: Core.Term) => (() => {
  const f2 = ((recurse: ((x: Graph.Graph) => ((x: Core.Term) => t0))) => ((cx: Graph.Graph) => ((term: Core.Term) => (() => {
  const recurse1 = ((term2: Core.Term) => recurse(cx)(term2));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const cx1 = Scoping.extendGraphForLambda(cx)(l);
  return (() => {
  const recurse2 = ((term2: Core.Term) => recurse(cx1)(term2));
  return f(recurse2)(cx1)(term);
})();
})())((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const cx1 = Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(cx)(l);
  return (() => {
  const recurse2 = ((term2: Core.Term) => recurse(cx1)(term2));
  return f(recurse2)(cx1)(term);
})();
})())((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const cx1 = Scoping.extendGraphForTypeLambda(cx)(tl);
  return (() => {
  const recurse2 = ((term2: Core.Term) => recurse(cx1)(term2));
  return f(recurse2)(cx1)(term);
})();
})())((_m as any).value);
    default: return f(recurse1)(cx)(term)(_m);
  }
})();
})())));
  return (() => {
  const rewrite = ((cx: Graph.Graph) => ((term: Core.Term) => f2(rewrite)(cx)(term)));
  return rewrite(cx0)(term0);
})();
})()));
}

export function rewriteType(f: ((x: ((x: Core.Type) => Core.Type)) => ((x: Core.Type) => Core.Type))): ((x: Core.Type) => Core.Type) {
  return ((typ0: Core.Type) => (() => {
  const fsub = ((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => (() => {
  const forField = ((field: Core.FieldType) => ({
    name: ((_x) => _x.name)(field),
    type: recurse(((_x) => _x.type)(field))
  }));
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => ({ tag: "annotated", value: ({
    body: recurse(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "application": return ((app: Core.ApplicationType) => ({ tag: "application", value: ({
    function: recurse(((_x) => _x.function)(app)),
    argument: recurse(((_x) => _x.argument)(app))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: recurse(((_x) => _x.left)(et)),
    right: recurse(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: recurse(((_x) => _x.first)(pt)),
    second: recurse(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "function": return ((fun: Core.FunctionType) => ({ tag: "function", value: ({
    domain: recurse(((_x) => _x.domain)(fun)),
    codomain: recurse(((_x) => _x.codomain)(fun))
  }) }))((_m as any).value);
    case "forall": return ((lt: Core.ForallType) => ({ tag: "forall", value: ({
    parameter: ((_x) => _x.parameter)(lt),
    body: recurse(((_x) => _x.body)(lt))
  }) }))((_m as any).value);
    case "list": return ((t: Core.Type) => ({ tag: "list", value: recurse(t) }))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => ({ tag: "literal", value: lt }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: recurse(((_x) => _x.keys)(mt)),
    values: recurse(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "maybe": return ((t: Core.Type) => ({ tag: "maybe", value: recurse(t) }))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "record", value: LibLists.map(forField)(rt) }))((_m as any).value);
    case "set": return ((t: Core.Type) => ({ tag: "set", value: recurse(t) }))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "union", value: LibLists.map(forField)(rt) }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "variable", value: v }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "void" }))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => ({ tag: "wrap", value: recurse(wt) }))((_m as any).value);
  }
})();
})()));
  return (() => {
  const recurse = ((v1: Core.Type) => f(((v12: Core.Type) => fsub(recurse)(v12)))(v1));
  return recurse(typ0);
})();
})());
}

export function rewriteTypeM<t0>(f: ((x: ((x: Core.Type) => t0 | Core.Type)) => ((x: Core.Type) => t0 | Core.Type))): ((x: Core.Type) => t0 | Core.Type) {
  return ((typ0: Core.Type) => (() => {
  const fsub = ((recurse: ((x: Core.Type) => t1 | Core.Type)) => ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => LibEithers.bind(recurse(((_x) => _x.body)(at)))(((t: Core.Type) => ({ tag: "right", value: ({ tag: "annotated", value: ({
    body: t,
    annotation: ((_x) => _x.annotation)(at)
  }) }) }))))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => LibEithers.bind(recurse(((_x) => _x.function)(at)))(((lhs: Core.Type) => LibEithers.bind(recurse(((_x) => _x.argument)(at)))(((rhs: Core.Type) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: lhs,
    argument: rhs
  }) }) }))))))((_m as any).value);
    case "either": return ((et: Core.EitherType) => LibEithers.bind(recurse(((_x) => _x.left)(et)))(((left: Core.Type) => LibEithers.bind(recurse(((_x) => _x.right)(et)))(((right: Core.Type) => ({ tag: "right", value: ({ tag: "either", value: ({
    left: left,
    right: right
  }) }) }))))))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibEithers.bind(recurse(((_x) => _x.first)(pt)))(((pairFirst: Core.Type) => LibEithers.bind(recurse(((_x) => _x.second)(pt)))(((pairSecond: Core.Type) => ({ tag: "right", value: ({ tag: "pair", value: ({
    first: pairFirst,
    second: pairSecond
  }) }) }))))))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => LibEithers.bind(recurse(((_x) => _x.domain)(ft)))(((dom: Core.Type) => LibEithers.bind(recurse(((_x) => _x.codomain)(ft)))(((cod: Core.Type) => ({ tag: "right", value: ({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) }) }))))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => LibEithers.bind(recurse(((_x) => _x.body)(ft)))(((b: Core.Type) => ({ tag: "right", value: ({ tag: "forall", value: ({
    parameter: ((_x) => _x.parameter)(ft),
    body: b
  }) }) }))))((_m as any).value);
    case "list": return ((t: Core.Type) => LibEithers.bind(recurse(t))(((rt: Core.Type) => ({ tag: "right", value: ({ tag: "list", value: rt }) }))))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => ({ tag: "right", value: ({ tag: "literal", value: lt }) }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibEithers.bind(recurse(((_x) => _x.keys)(mt)))(((kt: Core.Type) => LibEithers.bind(recurse(((_x) => _x.values)(mt)))(((vt: Core.Type) => ({ tag: "right", value: ({ tag: "map", value: ({
    keys: kt,
    values: vt
  }) }) }))))))((_m as any).value);
    case "maybe": return ((t: Core.Type) => LibEithers.bind(recurse(t))(((rt: Core.Type) => ({ tag: "right", value: ({ tag: "maybe", value: rt }) }))))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const forField = ((f2: Core.FieldType) => LibEithers.bind(recurse(((_x) => _x.type)(f2)))(((t: Core.Type) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(f2),
    type: t
  }) }))));
  return LibEithers.bind(LibEithers.mapList(forField)(rt))(((rfields: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: ({ tag: "record", value: rfields }) })));
})())((_m as any).value);
    case "set": return ((t: Core.Type) => LibEithers.bind(recurse(t))(((rt: Core.Type) => ({ tag: "right", value: ({ tag: "set", value: rt }) }))))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const forField = ((f2: Core.FieldType) => LibEithers.bind(recurse(((_x) => _x.type)(f2)))(((t: Core.Type) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(f2),
    type: t
  }) }))));
  return LibEithers.bind(LibEithers.mapList(forField)(rt))(((rfields: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: ({ tag: "union", value: rfields }) })));
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "unit" }) }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "right", value: ({ tag: "variable", value: v }) }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "right", value: ({ tag: "void" }) }))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => LibEithers.bind(recurse(wt))(((t: Core.Type) => ({ tag: "right", value: ({ tag: "wrap", value: t }) }))))((_m as any).value);
  }
})()));
  return (() => {
  const recurse = ((v1: Core.Type) => f(((v12: Core.Type) => fsub(recurse)(v12)))(v1));
  return recurse(typ0);
})();
})());
}

export function subterms(v1: Core.Term): ReadonlyArray<Core.Term> {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => [((_x) => _x.body)(at)])((_m as any).value);
    case "application": return ((p: Core.Application) => [((_x) => _x.function)(p), ((_x) => _x.argument)(p)])((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => LibLists.concat2(LibMaybes.maybe([])(((t: Core.Term) => [t]))(((_x) => _x.default)(cs)))(LibLists.map(((_x) => _x.term))(((_x) => _x.cases)(cs))))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => [l]))(((r: Core.Term) => [r]))(e))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => [((_x) => _x.body)(l)])((_m as any).value);
    case "let": return ((lt: Core.Let) => LibLists.cons(((_x) => _x.body)(lt))(LibLists.map(((_x) => _x.term))(((_x) => _x.bindings)(lt))))((_m as any).value);
    case "list": return ((l: ReadonlyArray<Core.Term>) => l)((_m as any).value);
    case "literal": return ((_: Core.Literal) => [])((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibLists.concat(LibLists.map(((p: readonly [Core.Term, Core.Term]) => [LibPairs.first(p), LibPairs.second(p)]))(LibMaps.toList(m))))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe([])(((t: Core.Term) => [t]))(m))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => [LibPairs.first(p), LibPairs.second(p)])((_m as any).value);
    case "project": return ((_: Core.Projection) => [])((_m as any).value);
    case "record": return ((rt: Core.Record) => LibLists.map(((_x) => _x.term))(((_x) => _x.fields)(rt)))((_m as any).value);
    case "set": return ((l: ReadonlySet<Core.Term>) => LibSets.toList(l))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => [((_x) => _x.body)(ta)])((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => [((_x) => _x.body)(ta)])((_m as any).value);
    case "inject": return ((ut: Core.Injection) => [((_x) => _x.term)(((_x) => _x.field)(ut))])((_m as any).value);
    case "unit": return ((_: void) => [])((_m as any).value);
    case "unwrap": return ((_: Core.Name) => [])((_m as any).value);
    case "variable": return ((_: Core.Name) => [])((_m as any).value);
    case "wrap": return ((n: Core.WrappedTerm) => [((_x) => _x.body)(n)])((_m as any).value);
  }
})();
}

export function subtermsWithSteps(v1: Core.Term): ReadonlyArray<readonly [Paths.SubtermStep, Core.Term]> {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => [[({ tag: "annotatedBody" }), ((_x) => _x.body)(at)]])((_m as any).value);
    case "application": return ((p: Core.Application) => [[({ tag: "applicationFunction" }), ((_x) => _x.function)(p)], [({ tag: "applicationArgument" }), ((_x) => _x.argument)(p)]])((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => LibLists.concat2(LibMaybes.maybe([])(((t: Core.Term) => [[({ tag: "unionCasesDefault" }), t]]))(((_x) => _x.default)(cs)))(LibLists.map(((f: Core.Field) => [({ tag: "unionCasesBranch", value: ((_x) => _x.name)(f) }), ((_x) => _x.term)(f)]))(((_x) => _x.cases)(cs))))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => [])((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => [[({ tag: "lambdaBody" }), ((_x) => _x.body)(l)]])((_m as any).value);
    case "let": return ((lt: Core.Let) => LibLists.cons([({ tag: "letBody" }), ((_x) => _x.body)(lt)])(LibLists.map(((b: Core.Binding) => [({ tag: "letBinding", value: ((_x) => _x.name)(b) }), ((_x) => _x.term)(b)]))(((_x) => _x.bindings)(lt))))((_m as any).value);
    case "list": return ((l: ReadonlyArray<Core.Term>) => LibLists.map(((e: Core.Term) => [({ tag: "listElement", value: 0 }), e]))(l))((_m as any).value);
    case "literal": return ((_: Core.Literal) => [])((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibLists.concat(LibLists.map(((p: readonly [Core.Term, Core.Term]) => [[({ tag: "mapKey", value: 0 }), LibPairs.first(p)], [({ tag: "mapValue", value: 0 }), LibPairs.second(p)]]))(LibMaps.toList(m))))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe([])(((t: Core.Term) => [[({ tag: "maybeTerm" }), t]]))(m))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => [])((_m as any).value);
    case "project": return ((_: Core.Projection) => [])((_m as any).value);
    case "record": return ((rt: Core.Record) => LibLists.map(((f: Core.Field) => [({ tag: "recordField", value: ((_x) => _x.name)(f) }), ((_x) => _x.term)(f)]))(((_x) => _x.fields)(rt)))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibLists.map(((e: Core.Term) => [({ tag: "listElement", value: 0 }), e]))(LibSets.toList(s)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => [[({ tag: "typeApplicationTerm" }), ((_x) => _x.body)(ta)]])((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => [[({ tag: "typeLambdaBody" }), ((_x) => _x.body)(ta)]])((_m as any).value);
    case "inject": return ((ut: Core.Injection) => [[({ tag: "injectionTerm" }), ((_x) => _x.term)(((_x) => _x.field)(ut))]])((_m as any).value);
    case "unit": return ((_: void) => [])((_m as any).value);
    case "unwrap": return ((_: Core.Name) => [])((_m as any).value);
    case "variable": return ((_: Core.Name) => [])((_m as any).value);
    case "wrap": return ((n: Core.WrappedTerm) => [[({ tag: "wrappedTerm" }), ((_x) => _x.body)(n)]])((_m as any).value);
  }
})();
}

export function subtypes(v1: Core.Type): ReadonlyArray<Core.Type> {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => [((_x) => _x.body)(at)])((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => [((_x) => _x.function)(at), ((_x) => _x.argument)(at)])((_m as any).value);
    case "either": return ((et: Core.EitherType) => [((_x) => _x.left)(et), ((_x) => _x.right)(et)])((_m as any).value);
    case "pair": return ((pt: Core.PairType) => [((_x) => _x.first)(pt), ((_x) => _x.second)(pt)])((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => [((_x) => _x.domain)(ft), ((_x) => _x.codomain)(ft)])((_m as any).value);
    case "forall": return ((lt: Core.ForallType) => [((_x) => _x.body)(lt)])((_m as any).value);
    case "list": return ((lt: Core.Type) => [lt])((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => [])((_m as any).value);
    case "map": return ((mt: Core.MapType) => [((_x) => _x.keys)(mt), ((_x) => _x.values)(mt)])((_m as any).value);
    case "maybe": return ((ot: Core.Type) => [ot])((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.map(((_x) => _x.type))(rt))((_m as any).value);
    case "set": return ((st: Core.Type) => [st])((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.map(((_x) => _x.type))(rt))((_m as any).value);
    case "unit": return ((_: void) => [])((_m as any).value);
    case "variable": return ((_: Core.Name) => [])((_m as any).value);
    case "void": return ((_: void) => [])((_m as any).value);
    case "wrap": return ((nt: Core.Type) => [nt])((_m as any).value);
  }
})();
}
