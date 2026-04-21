// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for reducing terms and types, i.e. performing computations.
 */



import * as Arity from "./arity.js";
import * as Ast from "./ast.js";
import * as Checking from "./checking.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as EncodeCore from "./encode/core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as ExtractCore from "./extract/core.js";
import * as Graph from "./graph.js";
import * as Hoisting from "./hoisting.js";
import * as Inference from "./inference.js";
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
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Resolution from "./resolution.js";
import * as Rewriting from "./rewriting.js";
import * as Scoping from "./scoping.js";
import * as ShowCore from "./show/core.js";
import * as ShowErrors from "./show/errors.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function alphaConvert(vold: Core.Name): ((x: Core.Name) => ((x: Core.Term) => Core.Term)) {
  return ((vnew: Core.Name) => ((term: Core.Term) => Variables.replaceFreeTermVariable(vold)(({ tag: "variable", value: vnew }))(term)));
}

export function betaReduceType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Type) => Errors.Error | Core.Type)) {
  return ((graph: Graph.Graph) => ((typ: Core.Type) => (() => {
  const reduceApp = ((app: Core.ApplicationType) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const _m = lhs;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => LibEithers.bind(reduceApp(({
    function: ((_x) => _x.body)(at),
    argument: rhs
  })))(((a: Core.Type) => ({ tag: "right", value: ({ tag: "annotated", value: ({
    body: a,
    annotation: ((_x) => _x.annotation)(at)
  }) }) }))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => betaReduceType(cx)(graph)(Variables.replaceFreeTypeVariable(((_x) => _x.parameter)(ft))(rhs)(((_x) => _x.body)(ft))))((_m as any).value);
    case "variable": return ((name: Core.Name) => LibEithers.bind(Resolution.requireType(cx)(graph)(name))(((t_: Core.Type) => betaReduceType(cx)(graph)(({ tag: "application", value: ({
    function: t_,
    argument: rhs
  }) })))))((_m as any).value);
  }
})();
})();
})());
  return (() => {
  const mapExpr = ((recurse: ((x: t1) => Errors.Error | Core.Type)) => ((t: t1) => (() => {
  const findApp = ((r: Core.Type) => (() => {
  const _m = r;
  switch (_m.tag) {
    case "application": return ((a: Core.ApplicationType) => reduceApp(a))((_m as any).value);
    default: return ({ tag: "right", value: r })(_m);
  }
})());
  return LibEithers.bind(recurse(t))(((r: Core.Type) => findApp(r)));
})()));
  return Rewriting.rewriteTypeM(mapExpr)(typ);
})();
})()));
}

export function contractTerm(term: Core.Term): Core.Term {
  return (() => {
  const rewrite = ((recurse: ((x: t0) => Core.Term)) => ((t: t0) => (() => {
  const rec = recurse(t);
  return (() => {
  const _m = rec;
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const _m = Strip.deannotateTerm(lhs);
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return LibLogic.ifElse(Variables.isFreeVariableInTerm(v)(body))(body)(Variables.replaceFreeTermVariable(v)(rhs)(body));
})();
})())((_m as any).value);
    default: return rec(_m);
  }
})();
})();
})())((_m as any).value);
    default: return rec(_m);
  }
})();
})()));
  return Rewriting.rewriteTerm(rewrite)(term);
})();
}

export const countPrimitiveInvocations: boolean = true;

export function etaExpandTerm(tx0: Graph.Graph): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const primTypes = LibMaps.fromList(LibLists.map(((_gpt_p: Graph.Primitive) => [((_x) => _x.name)(_gpt_p), ((_x) => _x.type)(_gpt_p)]))(LibMaps.elems(((_x) => _x.primitives)(tx0))));
  return (() => {
  const termArityWithContext = ((tx: Graph.Graph) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => termArityWithContext(tx)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((app: Core.Application) => LibMath.sub(termArityWithContext(tx)(((_x) => _x.function)(app)))(1))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => 1)((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => 0)((_m as any).value);
    case "project": return ((_: Core.Projection) => 1)((_m as any).value);
    case "unwrap": return ((_: Core.Name) => 1)((_m as any).value);
    case "let": return ((l: Core.Let) => termArityWithContext(Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(tx)(l))(((_x) => _x.body)(l)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => termArityWithContext(Scoping.extendGraphForTypeLambda(tx)(tl))(((_x) => _x.body)(tl)))((_m as any).value);
    case "typeApplication": return ((tat: Core.TypeApplicationTerm) => termArityWithContext(tx)(((_x) => _x.body)(tat)))((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(LibMaybes.maybe(0)(Arity.typeSchemeArity)(LibMaps.lookup(name)(primTypes)))(Arity.typeArity)(LibMaybes.map(Scoping.typeSchemeToFType)(LibMaps.lookup(name)(((_x) => _x.boundTypes)(tx)))))((_m as any).value);
    default: return 0(_m);
  }
})()));
  return (() => {
  const domainTypes = ((n: number) => ((mt: Core.Type | null) => LibLogic.ifElse(LibEquality.lte(n)(0))([])(LibMaybes.maybe(LibLists.map(((_: number) => null))(LibMath.range(1)(n)))(((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "function": return ((ftyp: Core.FunctionType) => LibLists.cons(((_x) => _x.domain)(ftyp))(domainTypes(LibMath.sub(n)(1))(((_x) => _x.codomain)(ftyp))))((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedType) => domainTypes(n)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((atyp: Core.ApplicationType) => domainTypes(n)(((_x) => _x.function)(atyp)))((_m as any).value);
    case "forall": return ((_: Core.ForallType) => LibLists.map(((_2: number) => null))(LibMath.range(1)(n)))((_m as any).value);
    default: return LibLists.map(((_: number) => null))(LibMath.range(1)(n))(_m);
  }
})()))(mt))));
  return (() => {
  const peelFunctionDomains = ((mtyp: Core.Type | null) => ((n: number) => LibLogic.ifElse(LibEquality.lte(n)(0))(mtyp)(LibMaybes.maybe(null)(((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "function": return ((ftyp: Core.FunctionType) => peelFunctionDomains(((_x) => _x.codomain)(ftyp))(LibMath.sub(n)(1)))((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedType) => peelFunctionDomains(((_x) => _x.body)(at))(n))((_m as any).value);
    case "application": return ((atyp: Core.ApplicationType) => peelFunctionDomains(((_x) => _x.function)(atyp))(n))((_m as any).value);
    case "forall": return ((_: Core.ForallType) => null)((_m as any).value);
    default: return null(_m);
  }
})()))(mtyp))));
  return (() => {
  const expand = ((alwaysPad: boolean) => ((args: ReadonlyArray<Core.Term>) => ((arity: number) => ((headTyp: Core.Type | null) => ((head: Core.Term) => (() => {
  const applied = LibLists.foldl(((lhs: Core.Term) => ((arg: Core.Term) => ({ tag: "application", value: ({
    function: lhs,
    argument: arg
  }) }))))(head)(args);
  return (() => {
  const numArgs = LibLists.length(args);
  return (() => {
  const needed = LibMath.sub(arity)(numArgs);
  return LibLogic.ifElse(LibLogic.and(LibEquality.gt(needed)(0))(LibLogic.or(alwaysPad)(LibEquality.gt(numArgs)(0))))((() => {
  const indices = LibMath.range(1)(needed);
  return (() => {
  const remainingType = peelFunctionDomains(headTyp)(numArgs);
  return (() => {
  const domains = domainTypes(needed)(remainingType);
  return (() => {
  const codomainType = peelFunctionDomains(remainingType)(needed);
  return (() => {
  const fullyAppliedRaw = LibLists.foldl(((body: Core.Term) => ((i: number) => (() => {
  const vn = LibStrings.cat2("v")(LibLiterals.showInt32(i));
  return ({ tag: "application", value: ({
    function: body,
    argument: ({ tag: "variable", value: vn })
  }) });
})())))(applied)(indices);
  return (() => {
  const fullyApplied = LibMaybes.maybe(fullyAppliedRaw)(((ct: Core.Type) => ({ tag: "annotated", value: ({
    body: fullyAppliedRaw,
    annotation: LibMaps.singleton("type")(EncodeCore.type(ct))
  }) })))(codomainType);
  return (() => {
  const indexedDomains = LibLists.zip(indices)(domains);
  return LibLists.foldl(((body: Core.Term) => ((idPair: readonly [number, Core.Type | null]) => (() => {
  const i = LibPairs.first(idPair);
  return (() => {
  const dom = LibPairs.second(idPair);
  return (() => {
  const vn = LibStrings.cat2("v")(LibLiterals.showInt32(i));
  return ({ tag: "lambda", value: ({
    parameter: vn,
    domain: dom,
    body: body
  }) });
})();
})();
})())))(fullyApplied)(LibLists.reverse(indexedDomains));
})();
})();
})();
})();
})();
})();
})())(applied);
})();
})();
})())))));
  return (() => {
  const rewriteWithArgs = ((args: ReadonlyArray<Core.Term>) => ((tx: Graph.Graph) => ((term: Core.Term) => (() => {
  const recurse = ((tx1: Graph.Graph) => ((term1: Core.Term) => rewriteWithArgs([])(tx1)(term1)));
  return (() => {
  const termHeadType = ((tx2: Graph.Graph) => ((trm2: Core.Term) => (() => {
  const _m = trm2;
  switch (_m.tag) {
    case "annotated": return ((at2: Core.AnnotatedTerm) => termHeadType(tx2)(((_x) => _x.body)(at2)))((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => null)((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => null)((_m as any).value);
    case "project": return ((_: Core.Projection) => null)((_m as any).value);
    case "unwrap": return ((_: Core.Name) => null)((_m as any).value);
    case "let": return ((l2: Core.Let) => termHeadType(Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(tx2)(l2))(((_x) => _x.body)(l2)))((_m as any).value);
    case "typeLambda": return ((tl2: Core.TypeLambda) => termHeadType(Scoping.extendGraphForTypeLambda(tx2)(tl2))(((_x) => _x.body)(tl2)))((_m as any).value);
    case "typeApplication": return ((tat2: Core.TypeApplicationTerm) => LibMaybes.bind(termHeadType(tx2)(((_x) => _x.body)(tat2)))(((htyp2: Core.Type) => (() => {
  const _m = htyp2;
  switch (_m.tag) {
    case "forall": return ((ft2: Core.ForallType) => Variables.replaceFreeTypeVariable(((_x) => _x.parameter)(ft2))(((_x) => _x.type)(tat2))(((_x) => _x.body)(ft2)))((_m as any).value);
    default: return htyp2(_m);
  }
})())))((_m as any).value);
    case "variable": return ((vn2: Core.Name) => LibMaybes.map(Scoping.typeSchemeToFType)(LibMaps.lookup(vn2)(((_x) => _x.boundTypes)(tx2))))((_m as any).value);
    default: return null(_m);
  }
})()));
  return (() => {
  const afterRecursion = ((trm: Core.Term) => (() => {
  const arity = termArityWithContext(tx)(trm);
  return (() => {
  const hType = termHeadType(tx)(trm);
  return expand(false)(args)(arity)(hType)(trm);
})();
})());
  return (() => {
  const forField = ((f: Core.Field) => ({
    name: ((_x) => _x.name)(f),
    term: recurse(tx)(((_x) => _x.term)(f))
  }));
  return (() => {
  const forCaseBranch = ((f: Core.Field) => (() => {
  const branchBody = recurse(tx)(((_x) => _x.term)(f));
  return (() => {
  const arty = termArityWithContext(tx)(branchBody);
  return (() => {
  const branchHType = termHeadType(tx)(branchBody);
  return ({
    name: ((_x) => _x.name)(f),
    term: expand(true)([])(arty)(branchHType)(branchBody)
  });
})();
})();
})());
  return (() => {
  const forMap = ((mp: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const forPair = ((pr: readonly [Core.Term, Core.Term]) => [recurse(tx)(LibPairs.first(pr)), recurse(tx)(LibPairs.second(pr))]);
  return LibMaps.fromList(LibLists.map(forPair)(LibMaps.toList(mp)));
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => afterRecursion(({ tag: "annotated", value: ({
    body: recurse(tx)(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) })))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const rhs = rewriteWithArgs([])(tx)(((_x) => _x.argument)(app));
  return rewriteWithArgs(LibLists.cons(rhs)(args))(tx)(((_x) => _x.function)(app));
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => afterRecursion(({ tag: "either", value: LibEithers.either(((l: Core.Term) => ({ tag: "left", value: recurse(tx)(l) })))(((r: Core.Term) => ({ tag: "right", value: recurse(tx)(r) })))(e) })))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const newCs = ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(((t1: Core.Term) => recurse(tx)(t1)))(((_x) => _x.default)(cs)),
    cases: LibLists.map(forCaseBranch)(((_x) => _x.cases)(cs))
  });
  return (() => {
  const elimTerm = ({ tag: "cases", value: newCs });
  return (() => {
  const elimHeadType = ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: ((_x) => _x.typeName)(cs) }),
    codomain: ({ tag: "unit" })
  }) });
  return expand(true)(args)(1)(elimHeadType)(elimTerm);
})();
})();
})())((_m as any).value);
    case "project": return ((p: Core.Projection) => expand(false)(args)(1)(null)(({ tag: "project", value: p })))((_m as any).value);
    case "unwrap": return ((nm: Core.Name) => expand(false)(args)(1)(null)(({ tag: "unwrap", value: nm })))((_m as any).value);
    case "lambda": return ((lm: Core.Lambda) => (() => {
  const tx1 = Scoping.extendGraphForLambda(tx)(lm);
  return (() => {
  const body = rewriteWithArgs([])(tx1)(((_x) => _x.body)(lm));
  return (() => {
  const result = ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(lm),
    domain: ((_x) => _x.domain)(lm),
    body: body
  }) });
  return (() => {
  const arty = termArityWithContext(tx)(result);
  return expand(false)(args)(arty)(null)(result);
})();
})();
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const tx1 = Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(tx)(lt);
  return (() => {
  const mapBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: rewriteWithArgs([])(tx1)(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return (() => {
  const result = ({ tag: "let", value: ({
    bindings: LibLists.map(mapBinding)(((_x) => _x.bindings)(lt)),
    body: rewriteWithArgs([])(tx1)(((_x) => _x.body)(lt))
  }) });
  return afterRecursion(result);
})();
})();
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => afterRecursion(({ tag: "list", value: LibLists.map(((el: Core.Term) => recurse(tx)(el)))(els) })))((_m as any).value);
    case "literal": return ((v: Core.Literal) => ({ tag: "literal", value: v }))((_m as any).value);
    case "map": return ((mp: ReadonlyMap<Core.Term, Core.Term>) => afterRecursion(({ tag: "map", value: forMap(mp) })))((_m as any).value);
    case "maybe": return ((mb: Core.Term | null) => afterRecursion(({ tag: "maybe", value: LibMaybes.map(((v: Core.Term) => recurse(tx)(v)))(mb) })))((_m as any).value);
    case "pair": return ((pr: readonly [Core.Term, Core.Term]) => afterRecursion(({ tag: "pair", value: [recurse(tx)(LibPairs.first(pr)), recurse(tx)(LibPairs.second(pr))] })))((_m as any).value);
    case "record": return ((rc: Core.Record) => afterRecursion(({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(rc),
    fields: LibLists.map(forField)(((_x) => _x.fields)(rc))
  }) })))((_m as any).value);
    case "set": return ((st: ReadonlySet<Core.Term>) => afterRecursion(({ tag: "set", value: LibSets.fromList(LibLists.map(((el: Core.Term) => recurse(tx)(el)))(LibSets.toList(st))) })))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => afterRecursion(({ tag: "typeApplication", value: ({
    body: recurse(tx)(((_x) => _x.body)(tt)),
    type: ((_x) => _x.type)(tt)
  }) })))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const tx1 = Scoping.extendGraphForTypeLambda(tx)(tl);
  return (() => {
  const result = ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: rewriteWithArgs([])(tx1)(((_x) => _x.body)(tl))
  }) });
  return afterRecursion(result);
})();
})())((_m as any).value);
    case "inject": return ((inj: Core.Injection) => afterRecursion(({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(inj),
    field: forField(((_x) => _x.field)(inj))
  }) })))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((vn: Core.Name) => (() => {
  const arty = termArityWithContext(tx)(term);
  return (() => {
  const varType = LibMaybes.map(Scoping.typeSchemeToFType)(LibMaps.lookup(vn)(((_x) => _x.boundTypes)(tx)));
  return expand(false)(args)(arty)(varType)(term);
})();
})())((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => afterRecursion(({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: recurse(tx)(((_x) => _x.body)(wt))
  }) })))((_m as any).value);
  }
})();
})();
})();
})();
})();
})();
})())));
  return contractTerm(rewriteWithArgs([])(tx0)(term0));
})();
})();
})();
})();
})();
})());
}

export function etaExpandTypedTerm(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((tx0: Graph.Graph) => ((term0: Core.Term) => (() => {
  const rewrite = ((topLevel: boolean) => ((forced: boolean) => ((typeArgs: ReadonlyArray<Core.Type>) => ((recurse: ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term))) => ((tx: Graph.Graph) => ((term: Core.Term) => (() => {
  const rewriteSpine = ((term2: Core.Term) => (() => {
  const _m = term2;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => LibEithers.bind(rewriteSpine(((_x) => _x.body)(at)))(((body: Core.Term) => (() => {
  const ann = ((_x) => _x.annotation)(at);
  return ({ tag: "right", value: ({ tag: "annotated", value: ({
    body: body,
    annotation: ann
  }) }) });
})())))((_m as any).value);
    case "application": return ((a: Core.Application) => (() => {
  const l = LibLogic.ifElse(false)([({ tag: "literal", value: ({ tag: "string" }) })])([]);
  return LibEithers.bind(rewriteSpine(((_x) => _x.function)(a)))(((lhs: Core.Term) => LibEithers.bind(rewrite(true)(false)(l)(recurse)(tx)(((_x) => _x.argument)(a)))(((rhs: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: lhs,
    argument: rhs
  }) }) })))));
})())((_m as any).value);
    case "typeApplication": return ((tat: Core.TypeApplicationTerm) => LibEithers.bind(rewriteSpine(((_x) => _x.body)(tat)))(((body: Core.Term) => (() => {
  const typ = ((_x) => _x.type)(tat);
  return ({ tag: "right", value: ({ tag: "typeApplication", value: ({
    body: body,
    type: typ
  }) }) });
})())))((_m as any).value);
    default: return rewrite(false)(false)([])(recurse)(tx)(term2)(_m);
  }
})());
  return (() => {
  const arityOf = ((tx2: Graph.Graph) => ((term2: Core.Term) => (() => {
  const dflt = LibEithers.map(((_tc: readonly [Core.Type, Context.Context]) => Arity.typeArity(LibPairs.first(_tc))))(Checking.typeOf(cx)(tx2)([])(term2));
  return (() => {
  const _m = term2;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => arityOf(tx2)(((_x) => _x.body)(at)))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => ({ tag: "right", value: 1 }))((_m as any).value);
    case "project": return ((_: Core.Projection) => ({ tag: "right", value: 1 }))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => ({ tag: "right", value: 1 }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const txl = Scoping.extendGraphForLambda(tx2)(l);
  return arityOf(txl)(((_x) => _x.body)(l));
})())((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const txl = Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(tx2)(l);
  return arityOf(txl)(((_x) => _x.body)(l));
})())((_m as any).value);
    case "typeApplication": return ((tat: Core.TypeApplicationTerm) => arityOf(tx2)(((_x) => _x.body)(tat)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const txt = Scoping.extendGraphForTypeLambda(tx2)(tl);
  return arityOf(txt)(((_x) => _x.body)(tl));
})())((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(LibEithers.map(((_tc: readonly [Core.Type, Context.Context]) => Arity.typeArity(LibPairs.first(_tc))))(Checking.typeOf(cx)(tx2)([])(({ tag: "variable", value: name }))))(((t: Core.Type) => ({ tag: "right", value: Arity.typeArity(t) })))(LibMaybes.map(Scoping.typeSchemeToFType)(LibMaps.lookup(name)(((_x) => _x.boundTypes)(tx2)))))((_m as any).value);
    default: return dflt(_m);
  }
})();
})()));
  return (() => {
  const extraVariables = ((n: number) => LibLists.map(((i: number) => LibStrings.cat2("v")(LibLiterals.showInt32(i))))(LibMath.range(1)(n)));
  return (() => {
  const pad = ((vars: ReadonlyArray<Core.Name>) => ((body: Core.Term) => LibLogic.ifElse(LibLists.null_(vars))(body)(({ tag: "lambda", value: ({
    parameter: LibLists.head(vars),
    domain: null,
    body: pad(LibLists.tail(vars))(({ tag: "application", value: ({
    function: body,
    argument: ({ tag: "variable", value: LibLists.head(vars) })
  }) }))
  }) }))));
  return (() => {
  const padn = ((n: number) => ((body: Core.Term) => pad(extraVariables(n))(body)));
  return (() => {
  const unwind = ((term2: Core.Term) => LibLists.foldl(((e: Core.Term) => ((t: Core.Type) => ({ tag: "typeApplication", value: ({
    body: e,
    type: t
  }) }))))(term2)(typeArgs));
  return (() => {
  const forceExpansion = ((t: Core.Term) => LibEithers.bind(Checking.typeOf(cx)(tx)([])(t))(((typCx: readonly [Core.Type, Context.Context]) => (() => {
  const arity = Arity.typeArity(LibPairs.first(typCx));
  return ({ tag: "right", value: padn(arity)(unwind(t)) });
})())));
  return (() => {
  const recurseOrForce = ((term2: Core.Term) => LibLogic.ifElse(forced)(forceExpansion(term2))(recurse(tx)(unwind(term2))));
  return (() => {
  const forCase = ((f: Core.Field) => LibEithers.bind(rewrite(false)(true)([])(recurse)(tx)(((_x) => _x.term)(f)))(((r: Core.Term) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(f),
    term: r
  }) }))));
  return (() => {
  const forCaseStatement = ((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const csCases = ((_x) => _x.cases)(cs);
  return LibEithers.bind(LibEithers.mapMaybe(((v1: Core.Term) => rewrite(false)(false)([])(recurse)(tx)(v1)))(dflt))(((rdflt: Core.Term | null) => LibEithers.bind(LibEithers.mapList(forCase)(csCases))(((rcases: ReadonlyArray<Core.Field>) => ({ tag: "right", value: ({ tag: "cases", value: ({
    typeName: tname,
    default: rdflt,
    cases: rcases
  }) }) })))));
})();
})();
})());
  return (() => {
  const forCases = ((cs: Core.CaseStatement) => LibEithers.bind(LibEithers.map(unwind)(forCaseStatement(cs)))(((base: Core.Term) => ({ tag: "right", value: LibLogic.ifElse(LibLogic.or(topLevel)(forced))(padn(1)(base))(base) }))));
  return (() => {
  const forNullaryElim = ((elimTerm: Core.Term) => (() => {
  const base = unwind(elimTerm);
  return LibLogic.ifElse(LibLogic.or(topLevel)(forced))(padn(1)(base))(base);
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "application": return ((a: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(a);
  return (() => {
  const rhs = ((_x) => _x.argument)(a);
  return LibEithers.bind(rewrite(true)(false)([])(recurse)(tx)(rhs))(((rhs2: Core.Term) => LibEithers.bind(arityOf(tx)(lhs))(((lhsarity: number) => LibEithers.bind(rewriteSpine(lhs))(((lhs2: Core.Term) => (() => {
  const a2 = ({ tag: "application", value: ({
    function: lhs2,
    argument: rhs2
  }) });
  return ({ tag: "right", value: LibLogic.ifElse(LibEquality.gt(lhsarity)(1))(padn(LibMath.sub(lhsarity)(1))(a2))(a2) });
})()))))));
})();
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => forCases(cs))((_m as any).value);
    case "project": return ((p: Core.Projection) => ({ tag: "right", value: forNullaryElim(({ tag: "project", value: p })) }))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => ({ tag: "right", value: forNullaryElim(({ tag: "unwrap", value: n })) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => (() => {
  const txl = Scoping.extendGraphForLambda(tx)(l);
  return LibEithers.map(unwind)(recurse(txl)(term));
})())((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const txlt = Scoping.extendGraphForLet(((_: Graph.Graph) => ((_2: Core.Binding) => null)))(tx)(l);
  return recurse(txlt)(term);
})())((_m as any).value);
    case "typeApplication": return ((tat: Core.TypeApplicationTerm) => rewrite(topLevel)(forced)(LibLists.cons(((_x) => _x.type)(tat))(typeArgs))(recurse)(tx)(((_x) => _x.body)(tat)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const txt = Scoping.extendGraphForTypeLambda(tx)(tl);
  return recurse(txt)(term);
})())((_m as any).value);
    default: return recurseOrForce(term)(_m);
  }
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()))))));
  return Rewriting.rewriteTermWithContextM(((v1: ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term))) => ((v2: Graph.Graph) => ((v3: Core.Term) => rewrite(true)(false)([])(v1)(v2)(v3)))))(tx0)(term0);
})()));
}

export function etaExpansionArity(graph: Graph.Graph): ((x: Core.Term) => number) {
  return ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => etaExpansionArity(graph)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((app: Core.Application) => LibMath.sub(etaExpansionArity(graph)(((_x) => _x.function)(app)))(1))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => 1)((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => 0)((_m as any).value);
    case "project": return ((_: Core.Projection) => 1)((_m as any).value);
    case "unwrap": return ((_: Core.Name) => 1)((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => etaExpansionArity(graph)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => etaExpansionArity(graph)(((_x) => _x.body)(tt)))((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(0)(((ts: Core.TypeScheme) => Arity.typeArity(((_x) => _x.type)(ts))))(LibMaybes.bind(Lexical.lookupBinding(graph)(name))(((b: Core.Binding) => ((_x) => _x.type)(b)))))((_m as any).value);
    default: return 0(_m);
  }
})());
}

export function etaReduceTerm(term: Core.Term): Core.Term {
  return (() => {
  const noChange = term;
  return (() => {
  const reduceLambda = ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return (() => {
  const d = ((_x) => _x.domain)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return (() => {
  const _m = etaReduceTerm(body);
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => reduceLambda(({
    parameter: v,
    domain: d,
    body: ((_x) => _x.body)(at)
  })))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const _m = etaReduceTerm(rhs);
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => reduceLambda(({
    parameter: v,
    domain: d,
    body: ({ tag: "application", value: ({
    function: lhs,
    argument: ((_x) => _x.body)(at)
  }) })
  })))((_m as any).value);
    case "variable": return ((v1: Core.Name) => LibLogic.ifElse(LibLogic.and(LibEquality.equal(((_x) => _x)(v))(((_x) => _x)(v1)))(LibLogic.not(Variables.isFreeVariableInTerm(v)(lhs))))(etaReduceTerm(lhs))(noChange))((_m as any).value);
    default: return noChange(_m);
  }
})();
})();
})())((_m as any).value);
    default: return noChange(_m);
  }
})();
})();
})();
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: etaReduceTerm(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => reduceLambda(l))((_m as any).value);
    default: return noChange(_m);
  }
})();
})();
})();
}

export function reduceTerm(cx: Context.Context): ((x: Graph.Graph) => ((x: boolean) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((graph: Graph.Graph) => ((eager: boolean) => ((term: Core.Term) => (() => {
  const reduce = ((eager2: boolean) => ((v1: Core.Term) => reduceTerm(cx)(graph)(eager2)(v1)));
  return (() => {
  const doRecurse = ((eager2: boolean) => ((term2: Core.Term) => (() => {
  const isNonLambdaTerm = (() => {
  const _m = term2;
  switch (_m.tag) {
    case "lambda": return ((_: Core.Lambda) => false)((_m as any).value);
    case "let": return ((_: Core.Let) => false)((_m as any).value);
    default: return true(_m);
  }
})();
  return LibLogic.and(eager2)(isNonLambdaTerm);
})()));
  return (() => {
  const reduceArg = ((eager2: boolean) => ((arg: Core.Term) => LibLogic.ifElse(eager2)(({ tag: "right", value: arg }))(reduce(false)(arg))));
  return (() => {
  const applyToArguments = ((fun: Core.Term) => ((args: ReadonlyArray<Core.Term>) => LibLogic.ifElse(LibLists.null_(args))(fun)(applyToArguments(({ tag: "application", value: ({
    function: fun,
    argument: LibLists.head(args)
  }) }))(LibLists.tail(args)))));
  return (() => {
  const mapErrorToString = ((e: Errors.Error) => ({ tag: "other", value: ShowErrors.error(e) }));
  return (() => {
  const applyProjection = ((proj: Core.Projection) => ((reducedArg: Core.Term) => LibEithers.bind(ExtractCore.record(((_x) => _x.typeName)(proj))(graph)(Strip.deannotateTerm(reducedArg)))(((fields: ReadonlyArray<Core.Field>) => (() => {
  const matchingFields = LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))(((_x) => _x.field)(proj))))(fields);
  return LibLogic.ifElse(LibLists.null_(matchingFields))(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: ((_x) => _x.field)(proj)
  }) }) }) }))(({ tag: "right", value: ((_x) => _x.term)(LibLists.head(matchingFields)) }));
})()))));
  return (() => {
  const applyCases = ((cs: Core.CaseStatement) => ((reducedArg: Core.Term) => LibEithers.bind(ExtractCore.injection(((_x) => _x.typeName)(cs))(graph)(reducedArg))(((field: Core.Field) => (() => {
  const matchingFields = LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))(((_x) => _x.name)(field))))(((_x) => _x.cases)(cs));
  return LibLogic.ifElse(LibLists.null_(matchingFields))(LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: ((_x) => _x.name)(field)
  }) }) }) }))(((x: Core.Term) => ({ tag: "right", value: x })))(((_x) => _x.default)(cs)))(({ tag: "right", value: ({ tag: "application", value: ({
    function: ((_x) => _x.term)(LibLists.head(matchingFields)),
    argument: ((_x) => _x.term)(field)
  }) }) }));
})()))));
  return (() => {
  const applyIfNullary = ((eager2: boolean) => ((original: Core.Term) => ((args: ReadonlyArray<Core.Term>) => (() => {
  const stripped = Strip.deannotateTerm(original);
  return (() => {
  const forProjection = ((proj: Core.Projection) => ((args2: ReadonlyArray<Core.Term>) => (() => {
  const arg = LibLists.head(args2);
  return (() => {
  const remainingArgs = LibLists.tail(args2);
  return LibEithers.bind(reduceArg(eager2)(Strip.deannotateTerm(arg)))(((reducedArg: Core.Term) => LibEithers.bind(LibEithers.bind(applyProjection(proj)(reducedArg))(((v1: Core.Term) => reduce(eager2)(v1))))(((reducedResult: Core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))));
})();
})()));
  return (() => {
  const forCases = ((cs: Core.CaseStatement) => ((args2: ReadonlyArray<Core.Term>) => (() => {
  const arg = LibLists.head(args2);
  return (() => {
  const remainingArgs = LibLists.tail(args2);
  return LibEithers.bind(reduceArg(eager2)(Strip.deannotateTerm(arg)))(((reducedArg: Core.Term) => LibEithers.bind(LibEithers.bind(applyCases(cs)(reducedArg))(((v1: Core.Term) => reduce(eager2)(v1))))(((reducedResult: Core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))));
})();
})()));
  return (() => {
  const forUnwrap = ((name: Core.Name) => ((args2: ReadonlyArray<Core.Term>) => (() => {
  const arg = LibLists.head(args2);
  return (() => {
  const remainingArgs = LibLists.tail(args2);
  return LibEithers.bind(reduceArg(eager2)(Strip.deannotateTerm(arg)))(((reducedArg: Core.Term) => LibEithers.bind(LibEithers.bind(ExtractCore.wrap(name)(graph)(reducedArg))(((v1: Core.Term) => reduce(eager2)(v1))))(((reducedResult: Core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))));
})();
})()));
  return (() => {
  const forLambda = ((l: Core.Lambda) => ((args2: ReadonlyArray<Core.Term>) => (() => {
  const param = ((_x) => _x.parameter)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return (() => {
  const arg = LibLists.head(args2);
  return (() => {
  const remainingArgs = LibLists.tail(args2);
  return LibEithers.bind(reduce(eager2)(Strip.deannotateTerm(arg)))(((reducedArg: Core.Term) => LibEithers.bind(reduce(eager2)(Variables.replaceFreeTermVariable(param)(reducedArg)(body)))(((reducedResult: Core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))));
})();
})();
})();
})()));
  return (() => {
  const forPrimitive = ((prim: Graph.Primitive) => ((arity: number) => ((args2: ReadonlyArray<Core.Term>) => (() => {
  const argList = LibLists.take(arity)(args2);
  return (() => {
  const remainingArgs = LibLists.drop(arity)(args2);
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Term) => reduceArg(eager2)(v1)))(argList))(((reducedArgs: ReadonlyArray<Core.Term>) => (() => {
  const strippedArgs = LibLists.map(Strip.deannotateTerm)(reducedArgs);
  return LibEithers.bind(LibEithers.bimap(mapErrorToString)(((x: Core.Term) => x))(((_x) => _x.implementation)(prim)(cx)(graph)(strippedArgs)))(((primResult: Core.Term) => LibEithers.bind(reduce(eager2)(primResult))(((reducedResult: Core.Term) => applyIfNullary(eager2)(reducedResult)(remainingArgs)))));
})()));
})();
})())));
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => applyIfNullary(eager2)(((_x) => _x.function)(app))(LibLists.cons(((_x) => _x.argument)(app))(args)))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => LibLogic.ifElse(LibLists.null_(args))(({ tag: "right", value: original }))(forCases(cs)(args)))((_m as any).value);
    case "project": return ((p: Core.Projection) => LibLogic.ifElse(LibLists.null_(args))(({ tag: "right", value: original }))(forProjection(p)(args)))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => LibLogic.ifElse(LibLists.null_(args))(({ tag: "right", value: original }))(forUnwrap(n)(args)))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => LibLogic.ifElse(LibLists.null_(args))(({ tag: "right", value: original }))(forLambda(l)(args)))((_m as any).value);
    case "variable": return ((v: Core.Name) => (() => {
  const mBinding = Lexical.lookupBinding(graph)(v);
  return LibMaybes.maybe((() => {
  const mPrim = Lexical.lookupPrimitive(graph)(v);
  return LibMaybes.maybe(({ tag: "right", value: applyToArguments(original)(args) }))(((prim: Graph.Primitive) => (() => {
  const arity = Arity.primitiveArity(prim);
  return LibLogic.ifElse(LibEquality.gt(arity)(LibLists.length(args)))(({ tag: "right", value: applyToArguments(original)(args) }))(forPrimitive(prim)(arity)(args));
})()))(mPrim);
})())(((binding: Core.Binding) => applyIfNullary(eager2)(((_x) => _x.term)(binding))(args)))(mBinding);
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return (() => {
  const letExpr = ((b: Core.Binding) => ({ tag: "let", value: ({
    bindings: [b],
    body: ({ tag: "variable", value: ((_x) => _x.name)(b) })
  }) }));
  return (() => {
  const expandBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: Variables.replaceFreeTermVariable(((_x) => _x.name)(b))(letExpr(b))(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return (() => {
  const expandedBindings = LibLists.map(expandBinding)(bindings);
  return (() => {
  const substituteBinding = ((term2: Core.Term) => ((b: Core.Binding) => Variables.replaceFreeTermVariable(((_x) => _x.name)(b))(((_x) => _x.term)(b))(term2)));
  return (() => {
  const substituteAll = ((bs: ReadonlyArray<Core.Binding>) => ((term2: Core.Term) => LibLists.foldl(substituteBinding)(term2)(bs)));
  return (() => {
  const expandedBody = substituteAll(expandedBindings)(body);
  return LibEithers.bind(reduce(eager2)(expandedBody))(((reducedBody: Core.Term) => applyIfNullary(eager2)(reducedBody)(args)));
})();
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    default: return ({ tag: "right", value: applyToArguments(original)(args) })(_m);
  }
})();
})();
})();
})();
})();
})();
})())));
  return (() => {
  const mapping = ((recurse: ((x: Core.Term) => Errors.Error | Core.Term)) => ((mid: Core.Term) => LibEithers.bind(LibLogic.ifElse(doRecurse(eager)(mid))(recurse(mid))(({ tag: "right", value: mid })))(((inner: Core.Term) => applyIfNullary(eager)(inner)([])))));
  return Rewriting.rewriteTermM(mapping)(term);
})();
})();
})();
})();
})();
})();
})();
})();
})())));
}

export function termIsClosed(term: Core.Term): boolean {
  return LibSets.null_(Variables.freeVariablesInTerm(term));
}

export function termIsValue(term: Core.Term): boolean {
  return (() => {
  const forList = ((els: ReadonlyArray<Core.Term>) => LibLists.foldl(((b: boolean) => ((t: Core.Term) => LibLogic.and(b)(termIsValue(t)))))(true)(els));
  return (() => {
  const checkField = ((f: Core.Field) => termIsValue(((_x) => _x.term)(f)));
  return (() => {
  const checkFields = ((fields: ReadonlyArray<Core.Field>) => LibLists.foldl(((b: boolean) => ((f: Core.Field) => LibLogic.and(b)(checkField(f)))))(true)(fields));
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "application": return ((_: Core.Application) => false)((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => LibLogic.and(checkFields(((_x) => _x.cases)(cs)))(LibMaybes.maybe(true)(termIsValue)(((_x) => _x.default)(cs))))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => termIsValue(l)))(((r: Core.Term) => termIsValue(r)))(e))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => termIsValue(((_x) => _x.body)(l)))((_m as any).value);
    case "literal": return ((_: Core.Literal) => true)((_m as any).value);
    case "project": return ((_: Core.Projection) => true)((_m as any).value);
    case "unwrap": return ((_: Core.Name) => true)((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => forList(els))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibLists.foldl(((b: boolean) => ((kv: readonly [Core.Term, Core.Term]) => LibLogic.and(b)(LibLogic.and(termIsValue(LibPairs.first(kv)))(termIsValue(LibPairs.second(kv)))))))(true)(LibMaps.toList(m)))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe(true)(termIsValue)(m))((_m as any).value);
    case "record": return ((r: Core.Record) => checkFields(((_x) => _x.fields)(r)))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => forList(LibSets.toList(s)))((_m as any).value);
    case "inject": return ((i: Core.Injection) => checkField(((_x) => _x.field)(i)))((_m as any).value);
    case "unit": return ((_: void) => true)((_m as any).value);
    case "variable": return ((_: Core.Name) => false)((_m as any).value);
    default: return false(_m);
  }
})();
})();
})();
})();
}
