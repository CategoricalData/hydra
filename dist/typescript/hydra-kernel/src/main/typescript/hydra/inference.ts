// Note: this is an automatically generated file. Do not edit.

/**
 * Type inference following Algorithm W, extended for nominal terms and types
 */



import * as Annotations from "./annotations.js";
import * as Ast from "./ast.js";
import * as Checking from "./checking.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as ExtractCore from "./extract/core.js";
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
import * as Reflect from "./reflect.js";
import * as Relational from "./relational.js";
import * as Resolution from "./resolution.js";
import * as Rewriting from "./rewriting.js";
import * as ShowCore from "./show/core.js";
import * as ShowErrors from "./show/errors.js";
import * as ShowTyping from "./show/typing.js";
import * as Sorting from "./sorting.js";
import * as Substitution from "./substitution.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Unification from "./unification.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function bindConstraints<t0>(flowCx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<Typing.TypeConstraint>) => Errors.Error | Typing.TypeSubst)) {
  return ((cx: Graph.Graph) => ((constraints: ReadonlyArray<Typing.TypeConstraint>) => LibEithers.bind(LibEithers.bimap(((_e: Errors.UnificationError) => ({ tag: "unification", value: _e })))(((_a: Typing.TypeSubst) => _a))(Unification.unifyTypeConstraints(flowCx)(((_x) => _x.schemaTypes)(cx))(constraints)))(((s: Typing.TypeSubst) => LibEithers.bind(Checking.checkTypeSubst(flowCx)(cx)(s))(((_: Typing.TypeSubst) => ({ tag: "right", value: s })))))));
}

export function bindUnboundTypeVariables(cx: Graph.Graph): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const svars = LibSets.fromList(LibMaps.keys(((_x) => _x.schemaTypes)(cx)));
  return (() => {
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "let": return ((l: Core.Let) => (() => {
  const forBinding = ((b: Core.Binding) => (() => {
  const bname = ((_x) => _x.name)(b);
  return (() => {
  const bterm = ((_x) => _x.term)(b);
  return LibMaybes.maybe(({
    name: bname,
    term: bindUnboundTypeVariables(cx)(bterm),
    type: null
  }))(((ts: Core.TypeScheme) => (() => {
  const bvars = LibSets.fromList(((_x) => _x.variables)(ts));
  return (() => {
  const excluded = LibSets.union(svars)(bvars);
  return (() => {
  const inType = LibSets.difference(Variables.freeVariablesInType(((_x) => _x.type)(ts)))(excluded);
  return (() => {
  const phantoms = LibSets.difference(Variables.freeTypeVariablesInTerm(bterm))(LibSets.union(excluded)(inType));
  return (() => {
  const phantomSubst = LibMaps.fromList(LibLists.map(((v: Core.Name) => [v, ({ tag: "unit" })]))(LibSets.toList(phantoms)));
  return (() => {
  const bterm1 = Substitution.substTypesInTerm(phantomSubst)(bterm);
  return (() => {
  const unbound = LibSets.toList(inType);
  return (() => {
  const ts2 = ({
    variables: LibLists.concat2(((_x) => _x.variables)(ts))(unbound),
    type: ((_x) => _x.type)(ts),
    constraints: ((_x) => _x.constraints)(ts)
  });
  return (() => {
  const bterm2 = LibLists.foldl(((t: Core.Term) => ((v: Core.Name) => ({ tag: "typeLambda", value: ({
    parameter: v,
    body: t
  }) }))))(bterm1)(unbound);
  return ({
    name: bname,
    term: bterm2,
    type: ts2
  });
})();
})();
})();
})();
})();
})();
})();
})();
})()))(((_x) => _x.type)(b));
})();
})());
  return ({ tag: "let", value: ({
    bindings: LibLists.map(forBinding)(((_x) => _x.bindings)(l)),
    body: bindUnboundTypeVariables(cx)(((_x) => _x.body)(l))
  }) });
})())((_m as any).value);
    default: return recurse(term)(_m);
  }
})()));
  return Rewriting.rewriteTerm(rewrite)(term0);
})();
})());
}

export function buildTypeApplicationTerm(tvars: ReadonlyArray<Core.Name>): ((x: Core.Term) => Core.Term) {
  return ((body: Core.Term) => LibLists.foldl(((t: Core.Term) => ((v: Core.Name) => ({ tag: "typeApplication", value: ({
    body: t,
    type: ({ tag: "variable", value: v })
  }) }))))(body)(tvars));
}

export function extendContext(pairs: ReadonlyArray<readonly [Core.Name, Core.TypeScheme]>): ((x: Graph.Graph) => Graph.Graph) {
  return ((cx: Graph.Graph) => ({
    boundTerms: ((_x) => _x.boundTerms)(cx),
    boundTypes: LibMaps.union(LibMaps.fromList(pairs))(((_x) => _x.boundTypes)(cx)),
    classConstraints: ((_x) => _x.classConstraints)(cx),
    lambdaVariables: ((_x) => _x.lambdaVariables)(cx),
    metadata: ((_x) => _x.metadata)(cx),
    primitives: ((_x) => _x.primitives)(cx),
    schemaTypes: ((_x) => _x.schemaTypes)(cx),
    typeVariables: ((_x) => _x.typeVariables)(cx)
  }));
}

export function finalizeInferredTerm<t0>(flowCx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((cx: Graph.Graph) => ((term: Core.Term) => (() => {
  const term2 = bindUnboundTypeVariables(cx)(term);
  return LibEithers.bind(Checking.checkForUnboundTypeVariables(flowCx)(cx)(term2))(((_: void) => ({ tag: "right", value: Variables.normalizeTypeVariablesInTerm(term2) })));
})()));
}

export function forInferredTerm<t0>(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => ((x: string) => ((x: ((x: Typing.InferenceResult) => t0)) => Errors.Error | readonly [t0, Context.Context])))) {
  return ((cx: Graph.Graph) => ((term: Core.Term) => ((desc: string) => ((f: ((x: Typing.InferenceResult) => t0)) => LibEithers.bind(inferTypeOfTerm(fcx)(cx)(term)(desc))(((rp: Typing.InferenceResult) => ({ tag: "right", value: [f(rp), ((_x) => _x.context)(rp)] })))))));
}

export function freeVariablesInContext(cx: Graph.Graph): ReadonlySet<Core.Name> {
  return LibLists.foldl(LibSets.union)(LibSets.empty)(LibLists.map(Variables.freeVariablesInTypeSchemeSimple)(LibMaps.elems(((_x) => _x.boundTypes)(cx))));
}

export function freshVariableType(cx: Context.Context): readonly [Core.Type, Context.Context] {
  return (() => {
  const result = Names.freshName(cx);
  return (() => {
  const name = LibPairs.first(result);
  return (() => {
  const cx2 = LibPairs.second(result);
  return [({ tag: "variable", value: name }), cx2];
})();
})();
})();
}

export function generalize(cx: Graph.Graph): ((x: Core.Type) => Core.TypeScheme) {
  return ((typ: Core.Type) => (() => {
  const isTypeVarName = ((name: Core.Name) => (() => {
  const parts = LibStrings.splitOn(".")(((_x) => _x)(name));
  return LibEquality.lte(LibLists.length(parts))(1);
})());
  return (() => {
  const vars = LibLists.nub(LibLists.filter(((v: Core.Name) => LibLogic.and(isUnbound(cx)(v))(isTypeVarName(v))))(Variables.freeVariablesInTypeOrdered(typ)));
  return (() => {
  const allConstraints = ((_x) => _x.classConstraints)(cx);
  return (() => {
  const relevantConstraints = LibMaps.fromList(LibMaybes.cat(LibLists.map(((v: Core.Name) => LibMaybes.map(((meta: Core.TypeVariableMetadata) => [v, meta]))(LibMaps.lookup(v)(allConstraints))))(vars)));
  return (() => {
  const constraintsMaybe = LibLogic.ifElse(LibMaps.null_(relevantConstraints))(null)(relevantConstraints);
  return ({
    variables: vars,
    type: typ,
    constraints: constraintsMaybe
  });
})();
})();
})();
})();
})());
}

export function inferGraphTypes(fcx0: Context.Context): ((x: ReadonlyArray<Core.Binding>) => ((x: Graph.Graph) => Errors.Error | readonly [readonly [Graph.Graph, ReadonlyArray<Core.Binding>], Context.Context])) {
  return ((bindings0: ReadonlyArray<Core.Binding>) => ((g0: Graph.Graph) => (() => {
  const fcx = ({
    trace: LibLists.cons("graph inference")(((_x) => _x.trace)(fcx0)),
    messages: ((_x) => _x.messages)(fcx0),
    other: ((_x) => _x.other)(fcx0)
  });
  return (() => {
  const let0 = ({
    bindings: bindings0,
    body: ({ tag: "unit" })
  });
  return (() => {
  const fromLetTerm = ((l: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(l);
  return (() => {
  const prims = ((_x) => _x.primitives)(g0);
  return (() => {
  const schemaTypes = ((_x) => _x.schemaTypes)(g0);
  return (() => {
  const rawG = Lexical.buildGraph(bindings)(LibMaps.empty)(prims);
  return (() => {
  const g = ({
    boundTerms: ((_x) => _x.boundTerms)(rawG),
    boundTypes: ((_x) => _x.boundTypes)(rawG),
    classConstraints: ((_x) => _x.classConstraints)(rawG),
    lambdaVariables: ((_x) => _x.lambdaVariables)(rawG),
    metadata: ((_x) => _x.metadata)(rawG),
    primitives: ((_x) => _x.primitives)(rawG),
    schemaTypes: schemaTypes,
    typeVariables: ((_x) => _x.typeVariables)(rawG)
  });
  return [g, bindings];
})();
})();
})();
})();
})());
  return LibEithers.bind(inferTypeOfTerm(fcx)(g0)(({ tag: "let", value: let0 }))("graph term"))(((result: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(result);
  return (() => {
  const term = ((_x) => _x.term)(result);
  return LibEithers.bind(finalizeInferredTerm(fcx2)(g0)(term))(((finalized: Core.Term) => (() => {
  const _m = finalized;
  switch (_m.tag) {
    case "let": return ((l: Core.Let) => ({ tag: "right", value: [fromLetTerm(l), fcx2] }))((_m as any).value);
    case "variable": return ((_: Core.Name) => ({ tag: "left", value: ({ tag: "other", value: "Expected inferred graph as let term" }) }))((_m as any).value);
  }
})()));
})();
})()));
})();
})();
})()));
}

export function inferInGraphContext(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((term: Core.Term) => inferTypeOfTerm(fcx)(cx)(term)("single term")));
}

export function inferMany(fcx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<readonly [Core.Term, string]>) => Errors.Error | readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context])) {
  return ((cx: Graph.Graph) => ((pairs: ReadonlyArray<readonly [Core.Term, string]>) => LibLogic.ifElse(LibLists.null_(pairs))(({ tag: "right", value: [[[], [[], [Substitution.idTypeSubst, LibMaps.empty]]], fcx] }))((() => {
  const dflt = (() => {
  const e = LibPairs.first(LibLists.head(pairs));
  return (() => {
  const desc = LibPairs.second(LibLists.head(pairs));
  return (() => {
  const tl = LibLists.tail(pairs);
  return LibEithers.bind(inferTypeOfTerm(fcx)(cx)(e)(desc))(((result1: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(result1);
  return (() => {
  const e1 = ((_x) => _x.term)(result1);
  return (() => {
  const t1 = ((_x) => _x.type)(result1);
  return (() => {
  const s1 = ((_x) => _x.subst)(result1);
  return (() => {
  const c1 = ((_x) => _x.classConstraints)(result1);
  return LibEithers.bind(inferMany(fcx2)(Substitution.substInContext(s1)(cx))(tl))(((rp2: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const result2 = LibPairs.first(rp2);
  return (() => {
  const fcx3 = LibPairs.second(rp2);
  return (() => {
  const e2 = LibPairs.first(result2);
  return (() => {
  const t2 = LibPairs.first(LibPairs.second(result2));
  return (() => {
  const s2 = LibPairs.first(LibPairs.second(LibPairs.second(result2)));
  return (() => {
  const c2 = LibPairs.second(LibPairs.second(LibPairs.second(result2)));
  return (() => {
  const c1Subst = Substitution.substInClassConstraints(s2)(c1);
  return (() => {
  const mergedConstraints = mergeClassConstraints(c1Subst)(c2);
  return ({ tag: "right", value: [[LibLists.cons(Substitution.substTypesInTerm(s2)(e1))(e2), [LibLists.cons(Substitution.substInType(s2)(t1))(t2), [Substitution.composeTypeSubst(s1)(s2), mergedConstraints]]], fcx3] });
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})()));
})();
})();
})();
  return dflt;
})())));
}

export function inferTypeOf(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | readonly [readonly [Core.Term, Core.TypeScheme], Context.Context])) {
  return ((cx: Graph.Graph) => ((term: Core.Term) => (() => {
  const letTerm = ({ tag: "let", value: ({
    bindings: [({
    name: "ignoredVariableName",
    term: term,
    type: null
  })],
    body: ({ tag: "literal", value: ({ tag: "string", value: "ignoredBody" }) })
  }) });
  return LibEithers.bind(inferTypeOfTerm(fcx)(cx)(letTerm)("infer type of term"))(((result: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(result);
  return LibEithers.bind(finalizeInferredTerm(fcx2)(cx)(((_x) => _x.term)(result)))(((finalized: Core.Term) => LibEithers.bind(ExtractCore.let_(cx)(finalized))(((letResult: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(letResult);
  return LibLogic.ifElse(LibEquality.equal(1)(LibLists.length(bindings)))((() => {
  const binding = LibLists.head(bindings);
  return (() => {
  const term1 = ((_x) => _x.term)(binding);
  return (() => {
  const mts = ((_x) => _x.type)(binding);
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: "Expected a type scheme" }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: [[term1, ts], fcx2] })))(mts);
})();
})();
})())(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat(["Expected a single binding with a type scheme, but got: ", LibLiterals.showInt32(LibLists.length(bindings)), " bindings"]) }) }));
})()))));
})()));
})()));
}

export function inferTypeOfAnnotatedTerm(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.AnnotatedTerm) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((at: Core.AnnotatedTerm) => (() => {
  const term = ((_x) => _x.body)(at);
  return (() => {
  const ann = ((_x) => _x.annotation)(at);
  return LibEithers.bind(inferTypeOfTerm(fcx)(cx)(term)("annotated term"))(((result: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(result);
  return (() => {
  const iterm = ((_x) => _x.term)(result);
  return (() => {
  const itype = ((_x) => _x.type)(result);
  return (() => {
  const isubst = ((_x) => _x.subst)(result);
  return (() => {
  const iconstraints = ((_x) => _x.classConstraints)(result);
  return ({ tag: "right", value: ({
    term: ({ tag: "annotated", value: ({
    body: iterm,
    annotation: ann
  }) }),
    type: itype,
    subst: isubst,
    classConstraints: iconstraints,
    context: fcx2
  }) });
})();
})();
})();
})();
})()));
})();
})()));
}

export function inferTypeOfApplication(fcx0: Context.Context): ((x: Graph.Graph) => ((x: Core.Application) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((app: Core.Application) => (() => {
  const fcx = ({
    trace: LibLists.cons("application")(((_x) => _x.trace)(fcx0)),
    messages: ((_x) => _x.messages)(fcx0),
    other: ((_x) => _x.other)(fcx0)
  });
  return (() => {
  const e0 = ((_x) => _x.function)(app);
  return (() => {
  const e1 = ((_x) => _x.argument)(app);
  return LibEithers.bind(inferTypeOfTerm(fcx)(cx)(e0)("lhs"))(((lhsResult: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(lhsResult);
  return (() => {
  const a = ((_x) => _x.term)(lhsResult);
  return (() => {
  const t0 = ((_x) => _x.type)(lhsResult);
  return (() => {
  const s0 = ((_x) => _x.subst)(lhsResult);
  return (() => {
  const c0 = ((_x) => _x.classConstraints)(lhsResult);
  return LibEithers.bind(inferTypeOfTerm(fcx2)(Substitution.substInContext(s0)(cx))(e1)("rhs"))(((rhsResult: Typing.InferenceResult) => (() => {
  const fcx3 = ((_x) => _x.context)(rhsResult);
  return (() => {
  const b = ((_x) => _x.term)(rhsResult);
  return (() => {
  const t1 = ((_x) => _x.type)(rhsResult);
  return (() => {
  const s1 = ((_x) => _x.subst)(rhsResult);
  return (() => {
  const c1 = ((_x) => _x.classConstraints)(rhsResult);
  return (() => {
  const vResult = Names.freshName(fcx3);
  return (() => {
  const v = LibPairs.first(vResult);
  return (() => {
  const fcx4 = LibPairs.second(vResult);
  return LibEithers.bind(LibEithers.bimap(((_e: Errors.UnificationError) => ({ tag: "unification", value: _e })))(((_a: Typing.TypeSubst) => _a))(Unification.unifyTypes(fcx4)(((_x) => _x.schemaTypes)(cx))(Substitution.substInType(s1)(t0))(({ tag: "function", value: ({
    domain: t1,
    codomain: ({ tag: "variable", value: v })
  }) }))("application lhs")))(((s2: Typing.TypeSubst) => LibEithers.bind(Checking.checkTypeSubst(fcx4)(cx)(s2))(((_: Typing.TypeSubst) => (() => {
  const rExpr = ({ tag: "application", value: ({
    function: Substitution.substTypesInTerm(Substitution.composeTypeSubst(s1)(s2))(a),
    argument: Substitution.substTypesInTerm(s2)(b)
  }) });
  return (() => {
  const rType = Substitution.substInType(s2)(({ tag: "variable", value: v }));
  return (() => {
  const rSubst = Substitution.composeTypeSubstList([s0, s1, s2]);
  return (() => {
  const c0Subst = Substitution.substInClassConstraints(s2)(Substitution.substInClassConstraints(s1)(c0));
  return (() => {
  const c1Subst = Substitution.substInClassConstraints(s2)(c1);
  return (() => {
  const rConstraints = mergeClassConstraints(c0Subst)(c1Subst);
  return ({ tag: "right", value: ({
    term: rExpr,
    type: rType,
    subst: rSubst,
    classConstraints: rConstraints,
    context: fcx4
  }) });
})();
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})()));
})();
})();
})()));
}

export function inferTypeOfCaseStatement(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.CaseStatement) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((caseStmt: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(caseStmt);
  return (() => {
  const dflt = ((_x) => _x.default)(caseStmt);
  return (() => {
  const cases = ((_x) => _x.cases)(caseStmt);
  return (() => {
  const fnames = LibLists.map(((_x) => _x.name))(cases);
  return LibEithers.bind(Resolution.requireSchemaType(fcx)(((_x) => _x.schemaTypes)(cx))(tname))(((stRp: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(stRp);
  return (() => {
  const fcx2 = LibPairs.second(stRp);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const stype = ((_x) => _x.type)(schemaType);
  return LibEithers.bind(ExtractCore.unionType(tname)(stype))(((sfields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(LibEithers.mapMaybe(((t: Core.Term) => inferTypeOfTerm(fcx2)(cx)(t)(LibStrings.cat(["case ", ((_x) => _x)(tname), ".<default>"]))))(dflt))(((dfltRp: Typing.InferenceResult | null) => (() => {
  const dfltResult = dfltRp;
  return (() => {
  const fcx3 = LibMaybes.fromMaybe(fcx2)(LibMaybes.map(((_x) => _x.context))(dfltRp));
  return LibEithers.bind(inferMany(fcx3)(cx)(LibLists.map(((f: Core.Field) => [((_x) => _x.term)(f), LibStrings.cat(["case ", ((_x) => _x)(tname), ".", ((_x) => _x)(((_x) => _x.name)(f))])]))(cases)))(((caseRp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const caseResults = LibPairs.first(caseRp);
  return (() => {
  const fcx4 = LibPairs.second(caseRp);
  return (() => {
  const iterms = LibPairs.first(caseResults);
  return (() => {
  const itypes = LibPairs.first(LibPairs.second(caseResults));
  return (() => {
  const isubst = LibPairs.first(LibPairs.second(LibPairs.second(caseResults)));
  return (() => {
  const caseElemConstraints = LibPairs.second(LibPairs.second(LibPairs.second(caseResults)));
  return (() => {
  const codvResult = Names.freshName(fcx4);
  return (() => {
  const codv = LibPairs.first(codvResult);
  return (() => {
  const fcx5 = LibPairs.second(codvResult);
  return (() => {
  const cod = ({ tag: "variable", value: codv });
  return (() => {
  const caseMap = LibMaps.fromList(LibLists.map(((ft: Core.FieldType) => [((_x) => _x.name)(ft), ((_x) => _x.type)(ft)]))(sfields));
  return (() => {
  const dfltConstraints = LibMaybes.toList(LibMaybes.map(((r: Typing.InferenceResult) => ({
    left: cod,
    right: Substitution.substInType(isubst)(((_x) => _x.type)(r)),
    comment: "match default"
  })))(dfltResult));
  return (() => {
  const caseConstraints = LibMaybes.cat(LibLists.zipWith(((fname: Core.Name) => ((itype: Core.Type) => LibMaybes.map(((ftype: Core.Type) => ({
    left: itype,
    right: ({ tag: "function", value: ({
    domain: ftype,
    codomain: cod
  }) }),
    comment: "case type"
  })))(LibMaps.lookup(fname)(caseMap)))))(fnames)(itypes));
  return (() => {
  const dfltClassConstraints = LibMaybes.fromMaybe(LibMaps.empty)(LibMaybes.map(((_x) => _x.classConstraints))(dfltResult));
  return (() => {
  const allElemConstraints = mergeClassConstraints(caseElemConstraints)(dfltClassConstraints);
  return LibEithers.bind(mapConstraints(fcx5)(cx)(((subst: Typing.TypeSubst) => yieldWithConstraints(fcx5)(buildTypeApplicationTerm(svars)(({ tag: "cases", value: ({
    typeName: tname,
    default: LibMaybes.map(((_x) => _x.term))(dfltResult),
    cases: LibLists.zipWith(((n: Core.Name) => ((t: Core.Term) => ({
    name: n,
    term: t
  }))))(fnames)(iterms)
  }) })))(({ tag: "function", value: ({
    domain: Resolution.nominalApplication(tname)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(svars)),
    codomain: cod
  }) }))(Substitution.composeTypeSubstList(LibLists.concat([LibMaybes.toList(LibMaybes.map(((_x) => _x.subst))(dfltResult)), [isubst, subst]])))(Substitution.substInClassConstraints(subst)(allElemConstraints))))(LibLists.concat([dfltConstraints, caseConstraints])))(((mcResult: Typing.InferenceResult) => ({ tag: "right", value: mcResult })));
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
})();
})();
})()));
})();
})()))));
})();
})();
})();
})()));
})();
})();
})();
})()));
}

export function inferTypeOfCollection(fcx: Context.Context): ((x: Graph.Graph) => ((x: ((x: Core.Type) => Core.Type)) => ((x: ((x: ReadonlyArray<Core.Term>) => Core.Term)) => ((x: string) => ((x: ReadonlySet<Core.Name>) => ((x: ReadonlyArray<Core.Term>) => Errors.Error | Typing.InferenceResult)))))) {
  return ((cx: Graph.Graph) => ((typCons: ((x: Core.Type) => Core.Type)) => ((trmCons: ((x: ReadonlyArray<Core.Term>) => Core.Term)) => ((desc: string) => ((classNames: ReadonlySet<Core.Name>) => ((els: ReadonlyArray<Core.Term>) => (() => {
  const varResult = Names.freshName(fcx);
  return (() => {
  const var_ = LibPairs.first(varResult);
  return (() => {
  const fcx2 = LibPairs.second(varResult);
  return (() => {
  const classConstraints = LibLogic.ifElse(LibSets.null_(classNames))(LibMaps.empty)(LibMaps.singleton(var_)(({
    classes: classNames
  })));
  return LibLogic.ifElse(LibLists.null_(els))(({ tag: "right", value: yieldWithConstraints(fcx2)(buildTypeApplicationTerm([var_])(trmCons([])))(typCons(({ tag: "variable", value: var_ })))(Substitution.idTypeSubst)(classConstraints) }))(LibEithers.bind(inferMany(fcx2)(cx)(LibLists.zip(els)(LibLists.map(((i: number) => LibStrings.cat(["#", LibLiterals.showInt32(i)])))(LibMath.range(1)(LibMath.add(LibLists.length(els))(1))))))(((resultsRp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const results = LibPairs.first(resultsRp);
  return (() => {
  const fcx3 = LibPairs.second(resultsRp);
  return (() => {
  const terms = LibPairs.first(results);
  return (() => {
  const types = LibPairs.first(LibPairs.second(results));
  return (() => {
  const subst1 = LibPairs.first(LibPairs.second(LibPairs.second(results)));
  return (() => {
  const elemConstraints = LibPairs.second(LibPairs.second(LibPairs.second(results)));
  return (() => {
  const constraints = LibLists.map(((t: Core.Type) => ({
    left: ({ tag: "variable", value: var_ }),
    right: t,
    comment: desc
  })))(types);
  return (() => {
  const allConstraints = mergeClassConstraints(classConstraints)(elemConstraints);
  return LibEithers.bind(mapConstraints(fcx3)(cx)(((subst2: Typing.TypeSubst) => (() => {
  const iterm = trmCons(terms);
  return (() => {
  const itype = typCons(({ tag: "variable", value: var_ }));
  return (() => {
  const isubst = Substitution.composeTypeSubst(subst1)(subst2);
  return yieldWithConstraints(fcx3)(iterm)(itype)(isubst)(Substitution.substInClassConstraints(subst2)(allConstraints));
})();
})();
})()))(constraints))(((mcResult: Typing.InferenceResult) => ({ tag: "right", value: mcResult })));
})();
})();
})();
})();
})();
})();
})();
})())));
})();
})();
})();
})()))))));
}

export function inferTypeOfEither(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term | Core.Term) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => LibEithers.bind(inferTypeOfTerm(fcx)(cx)(l)("either left value"))(((r1: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(r1);
  return (() => {
  const iterm = ((_x) => _x.term)(r1);
  return (() => {
  const leftType = ((_x) => _x.type)(r1);
  return (() => {
  const subst = ((_x) => _x.subst)(r1);
  return (() => {
  const fvResult = freshVariableType(fcx2);
  return (() => {
  const rightType = LibPairs.first(fvResult);
  return (() => {
  const fcx3 = LibPairs.second(fvResult);
  return (() => {
  const eitherTerm = ({ tag: "either", value: ({ tag: "left", value: iterm }) });
  return (() => {
  const termWithLeftType = ({ tag: "typeApplication", value: ({
    body: eitherTerm,
    type: leftType
  }) });
  return (() => {
  const termWithBothTypes = ({ tag: "typeApplication", value: ({
    body: termWithLeftType,
    type: rightType
  }) });
  return (() => {
  const eitherType = ({ tag: "either", value: ({
    left: leftType,
    right: rightType
  }) });
  return ({ tag: "right", value: yieldChecked(fcx3)(termWithBothTypes)(eitherType)(subst) });
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
})()))))(((r: Core.Term) => LibEithers.bind(inferTypeOfTerm(fcx)(cx)(r)("either right value"))(((r1: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(r1);
  return (() => {
  const iterm = ((_x) => _x.term)(r1);
  return (() => {
  const rightType = ((_x) => _x.type)(r1);
  return (() => {
  const subst = ((_x) => _x.subst)(r1);
  return (() => {
  const fvResult = freshVariableType(fcx2);
  return (() => {
  const leftType = LibPairs.first(fvResult);
  return (() => {
  const fcx3 = LibPairs.second(fvResult);
  return (() => {
  const eitherTerm = ({ tag: "either", value: ({ tag: "right", value: iterm }) });
  return (() => {
  const termWithLeftType = ({ tag: "typeApplication", value: ({
    body: eitherTerm,
    type: leftType
  }) });
  return (() => {
  const termWithBothTypes = ({ tag: "typeApplication", value: ({
    body: termWithLeftType,
    type: rightType
  }) });
  return (() => {
  const eitherType = ({ tag: "either", value: ({
    left: leftType,
    right: rightType
  }) });
  return ({ tag: "right", value: yieldChecked(fcx3)(termWithBothTypes)(eitherType)(subst) });
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
})()))))(e)));
}

export function inferTypeOfInjection(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Injection) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((injection: Core.Injection) => (() => {
  const tname = ((_x) => _x.typeName)(injection);
  return (() => {
  const field = ((_x) => _x.field)(injection);
  return (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const term = ((_x) => _x.term)(field);
  return LibEithers.bind(inferTypeOfTerm(fcx)(cx)(term)("injected term"))(((result: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(result);
  return LibEithers.bind(Resolution.requireSchemaType(fcx2)(((_x) => _x.schemaTypes)(cx))(tname))(((stRp: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(stRp);
  return (() => {
  const fcx3 = LibPairs.second(stRp);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const stype = ((_x) => _x.type)(schemaType);
  return (() => {
  const iterm = ((_x) => _x.term)(result);
  return (() => {
  const ityp = ((_x) => _x.type)(result);
  return (() => {
  const isubst = ((_x) => _x.subst)(result);
  return LibEithers.bind(ExtractCore.unionType(tname)(stype))(((sfields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(Resolution.findFieldType(fcx3)(fname)(sfields))(((ftyp: Core.Type) => LibEithers.bind(mapConstraints(fcx3)(cx)(((subst: Typing.TypeSubst) => yield_(fcx3)(buildTypeApplicationTerm(svars)(({ tag: "inject", value: ({
    typeName: tname,
    field: ({
    name: fname,
    term: iterm
  })
  }) })))(Resolution.nominalApplication(tname)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(svars)))(Substitution.composeTypeSubst(isubst)(subst))))([({
    left: ftyp,
    right: ityp,
    comment: "schema type of injected field"
  })]))(((mcResult: Typing.InferenceResult) => ({ tag: "right", value: mcResult })))))));
})();
})();
})();
})();
})();
})();
})()));
})()));
})();
})();
})();
})()));
}

export function inferTypeOfLambda(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Lambda) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((lambda: Core.Lambda) => (() => {
  const var_ = ((_x) => _x.parameter)(lambda);
  return (() => {
  const body = ((_x) => _x.body)(lambda);
  return (() => {
  const vdomResult = Names.freshName(fcx);
  return (() => {
  const vdom = LibPairs.first(vdomResult);
  return (() => {
  const fcx2 = LibPairs.second(vdomResult);
  return (() => {
  const dom = ({ tag: "variable", value: vdom });
  return (() => {
  const cx2 = extendContext([[var_, ({
    variables: [],
    type: dom,
    constraints: null
  })]])(cx);
  return LibEithers.bind(inferTypeOfTerm(fcx2)(cx2)(body)("lambda body"))(((result: Typing.InferenceResult) => (() => {
  const fcx3 = ((_x) => _x.context)(result);
  return (() => {
  const iterm = ((_x) => _x.term)(result);
  return (() => {
  const icod = ((_x) => _x.type)(result);
  return (() => {
  const isubst = ((_x) => _x.subst)(result);
  return (() => {
  const rdom = Substitution.substInType(isubst)(dom);
  return (() => {
  const rterm = ({ tag: "lambda", value: ({
    parameter: var_,
    domain: rdom,
    body: iterm
  }) });
  return (() => {
  const rtype = ({ tag: "function", value: ({
    domain: rdom,
    codomain: icod
  }) });
  return (() => {
  const vars = LibSets.unions([Variables.freeVariablesInType(rdom), Variables.freeVariablesInType(icod), freeVariablesInContext(Substitution.substInContext(isubst)(cx2))]);
  return (() => {
  const cx3 = Substitution.substInContext(isubst)(cx);
  return (() => {
  const iconstraints = Substitution.substInClassConstraints(isubst)(((_x) => _x.classConstraints)(result));
  return ({ tag: "right", value: ({
    term: rterm,
    type: rtype,
    subst: isubst,
    classConstraints: iconstraints,
    context: fcx3
  }) });
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})();
})();
})()));
}

export function inferTypeOfLet(fcx0: Context.Context): ((x: Graph.Graph) => ((x: Core.Let) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((let0: Core.Let) => (() => {
  const fcx = ({
    trace: LibLists.cons("let")(((_x) => _x.trace)(fcx0)),
    messages: ((_x) => _x.messages)(fcx0),
    other: ((_x) => _x.other)(fcx0)
  });
  return (() => {
  const bindings0 = ((_x) => _x.bindings)(let0);
  return (() => {
  const body0 = ((_x) => _x.body)(let0);
  return (() => {
  const names = LibLists.map(((_x) => _x.name))(bindings0);
  return (() => {
  const nameSet = LibSets.fromList(names);
  return (() => {
  const toPair = ((binding: Core.Binding) => (() => {
  const name = ((_x) => _x.name)(binding);
  return (() => {
  const term = ((_x) => _x.term)(binding);
  return [name, LibLists.filter(((n: Core.Name) => LibSets.member(n)(nameSet)))(LibSets.toList(Variables.freeVariablesInTerm(term)))];
})();
})());
  return (() => {
  const adjList = LibLists.map(toPair)(bindings0);
  return (() => {
  const groups = Sorting.topologicalSortComponents(adjList);
  return (() => {
  const bindingMap = LibMaps.fromList(LibLists.zip(names)(bindings0));
  return (() => {
  const createLet = ((e: Core.Term) => ((group: ReadonlyArray<Core.Name>) => ({ tag: "let", value: ({
    bindings: LibMaybes.cat(LibLists.map(((n: Core.Name) => LibMaps.lookup(n)(bindingMap)))(group)),
    body: e
  }) })));
  return (() => {
  const rewrittenLet = LibLists.foldl(createLet)(body0)(LibLists.reverse(groups));
  return (() => {
  const restoreLet = ((iterm: Core.Term) => (() => {
  const helper = ((level: number) => ((bins: ReadonlyArray<Core.Binding>) => ((term: Core.Term) => (() => {
  const nonzero = ((term2: Core.Term) => (() => {
  const _m = term2;
  switch (_m.tag) {
    case "let": return ((l: Core.Let) => (() => {
  const bs = ((_x) => _x.bindings)(l);
  return (() => {
  const letBody = ((_x) => _x.body)(l);
  return helper(LibMath.sub(level)(1))(LibLists.concat([bs, bins]))(letBody);
})();
})())((_m as any).value);
  }
})());
  return LibLogic.ifElse(LibEquality.equal(level)(0))([bins, term])(nonzero(term));
})())));
  return (() => {
  const result = helper(LibLists.length(groups))([])(iterm);
  return (() => {
  const bindingList = LibPairs.first(result);
  return (() => {
  const e = LibPairs.second(result);
  return (() => {
  const bindingMap2 = LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), b]))(bindingList));
  return ({ tag: "let", value: ({
    bindings: LibMaybes.cat(LibLists.map(((n: Core.Name) => LibMaps.lookup(n)(bindingMap2)))(names)),
    body: e
  }) });
})();
})();
})();
})();
})());
  return (() => {
  const rewriteResult = ((iresult: Typing.InferenceResult) => (() => {
  const fcxR = ((_x) => _x.context)(iresult);
  return (() => {
  const iterm = ((_x) => _x.term)(iresult);
  return (() => {
  const itype = ((_x) => _x.type)(iresult);
  return (() => {
  const isubst = ((_x) => _x.subst)(iresult);
  return (() => {
  const iconstraints = ((_x) => _x.classConstraints)(iresult);
  return ({
    term: restoreLet(iterm),
    type: itype,
    subst: isubst,
    classConstraints: iconstraints,
    context: fcxR
  });
})();
})();
})();
})();
})());
  return (() => {
  const res = (() => {
  const _m = rewrittenLet;
  switch (_m.tag) {
    case "let": return ((l: Core.Let) => inferTypeOfLetNormalized(fcx)(cx)(l))((_m as any).value);
    default: return inferTypeOfTerm(fcx)(cx)(rewrittenLet)("empty let term")(_m);
  }
})();
  return LibEithers.map(rewriteResult)(res);
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
})();
})()));
}

export function inferTypeOfLetNormalized(fcx0: Context.Context): ((x: Graph.Graph) => ((x: Core.Let) => Errors.Error | Typing.InferenceResult)) {
  return ((cx0: Graph.Graph) => ((letTerm: Core.Let) => (() => {
  const fcx = ({
    trace: LibLists.cons("let-normalized")(((_x) => _x.trace)(fcx0)),
    messages: ((_x) => _x.messages)(fcx0),
    other: ((_x) => _x.other)(fcx0)
  });
  return (() => {
  const bins0 = ((_x) => _x.bindings)(letTerm);
  return (() => {
  const body0 = ((_x) => _x.body)(letTerm);
  return (() => {
  const bnames = LibLists.map(((_x) => _x.name))(bins0);
  return (() => {
  const bvarsResult = Names.freshNames(LibLists.length(bins0))(fcx);
  return (() => {
  const bvars = LibPairs.first(bvarsResult);
  return (() => {
  const fcx2 = LibPairs.second(bvarsResult);
  return (() => {
  const tbins0 = LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(bvars);
  return (() => {
  const cx1 = extendContext(LibLists.zip(bnames)(LibLists.map(((t: Core.Type) => ({
    variables: [],
    type: t,
    constraints: null
  })))(tbins0)))(cx0);
  return LibEithers.bind(inferTypesOfTemporaryBindings(fcx2)(cx1)(bins0))(((irRp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const inferredResult = LibPairs.first(irRp);
  return (() => {
  const fcx3 = LibPairs.second(irRp);
  return (() => {
  const bterms1 = LibPairs.first(inferredResult);
  return (() => {
  const tbins1 = LibPairs.first(LibPairs.second(inferredResult));
  return (() => {
  const substAndConstraints = LibPairs.second(LibPairs.second(inferredResult));
  return (() => {
  const s1 = LibPairs.first(substAndConstraints);
  return (() => {
  const inferredConstraints = LibPairs.second(substAndConstraints);
  return LibEithers.bind(LibEithers.bimap(((_e: Errors.UnificationError) => ({ tag: "unification", value: _e })))(((_a: Typing.TypeSubst) => _a))(Unification.unifyTypeLists(fcx3)(((_x) => _x.schemaTypes)(cx0))(LibLists.map(((v1: Core.Type) => Substitution.substInType(s1)(v1)))(tbins0))(tbins1)("temporary type bindings")))(((s2: Typing.TypeSubst) => LibEithers.bind(Checking.checkTypeSubst(fcx3)(cx0)(s2))(((_: Typing.TypeSubst) => (() => {
  const g2base = Substitution.substInContext(Substitution.composeTypeSubst(s1)(s2))(cx0);
  return (() => {
  const constraintsWithS2 = Substitution.substInClassConstraints(s2)(inferredConstraints);
  return (() => {
  const composedSubst = Substitution.composeTypeSubst(s1)(s2);
  return (() => {
  const originalBindingConstraints = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => ((b: Core.Binding) => LibMaybes.maybe(acc)(((ts: Core.TypeScheme) => LibMaybes.maybe(acc)(((c: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => mergeClassConstraints(acc)(c)))(((_x) => _x.constraints)(ts))))(((_x) => _x.type)(b)))))(LibMaps.empty)(bins0);
  return (() => {
  const originalConstraintsSubst = Substitution.substInClassConstraints(composedSubst)(originalBindingConstraints);
  return (() => {
  const allInferredConstraints = mergeClassConstraints(constraintsWithS2)(originalConstraintsSubst);
  return (() => {
  const mergedConstraints = mergeClassConstraints(((_x) => _x.classConstraints)(g2base))(allInferredConstraints);
  return (() => {
  const g2 = ({
    boundTerms: ((_x) => _x.boundTerms)(g2base),
    boundTypes: ((_x) => _x.boundTypes)(g2base),
    classConstraints: mergedConstraints,
    lambdaVariables: ((_x) => _x.lambdaVariables)(g2base),
    metadata: ((_x) => _x.metadata)(g2base),
    primitives: ((_x) => _x.primitives)(g2base),
    schemaTypes: ((_x) => _x.schemaTypes)(g2base),
    typeVariables: ((_x) => _x.typeVariables)(g2base)
  });
  return (() => {
  const bterms1Subst = LibLists.map(((v1: Core.Term) => Substitution.substTypesInTerm(s2)(v1)))(bterms1);
  return (() => {
  const tsbins1 = LibLists.zip(bnames)(LibLists.map(((t: Core.Type) => generalize(g2)(Substitution.substInType(s2)(t))))(tbins1));
  return LibEithers.bind(inferTypeOfTerm(fcx3)(extendContext(tsbins1)(g2))(body0)("let body"))(((bodyResult: Typing.InferenceResult) => (() => {
  const fcx4 = ((_x) => _x.context)(bodyResult);
  return (() => {
  const body1 = ((_x) => _x.term)(bodyResult);
  return (() => {
  const tbody = ((_x) => _x.type)(bodyResult);
  return (() => {
  const sbody = ((_x) => _x.subst)(bodyResult);
  return (() => {
  const st1 = LibMaps.fromList(LibLists.map(((pair: readonly [Core.Name, Core.TypeScheme]) => (() => {
  const name = LibPairs.first(pair);
  return (() => {
  const ts = LibPairs.second(pair);
  return [name, buildTypeApplicationTerm(((_x) => _x.variables)(ts))(({ tag: "variable", value: name }))];
})();
})()))(tsbins1));
  return (() => {
  const createBinding = ((bindingPair: readonly [readonly [Core.Name, Core.TypeScheme], Core.Term]) => (() => {
  const nameTsPair = LibPairs.first(bindingPair);
  return (() => {
  const term = LibPairs.second(bindingPair);
  return (() => {
  const name = LibPairs.first(nameTsPair);
  return (() => {
  const ts = LibPairs.second(nameTsPair);
  return (() => {
  const finalTs = Substitution.substInTypeScheme(sbody)(ts);
  return (() => {
  const typeLambdaTerm = LibLists.foldl(((b: Core.Term) => ((v: Core.Name) => ({ tag: "typeLambda", value: ({
    parameter: v,
    body: b
  }) }))))(Substitution.substituteInTerm(st1)(term))(LibLists.reverse(((_x) => _x.variables)(finalTs)));
  return ({
    name: name,
    term: Substitution.substTypesInTerm(Substitution.composeTypeSubst(sbody)(s2))(typeLambdaTerm),
    type: finalTs
  });
})();
})();
})();
})();
})();
})());
  return (() => {
  const bins1 = LibLists.map(createBinding)(LibLists.zip(tsbins1)(bterms1Subst));
  return (() => {
  const bodyConstraints = Substitution.substInClassConstraints(sbody)(((_x) => _x.classConstraints)(bodyResult));
  return (() => {
  const bindingConstraintsSubst = Substitution.substInClassConstraints(sbody)(constraintsWithS2);
  return (() => {
  const allConstraints = mergeClassConstraints(bindingConstraintsSubst)(bodyConstraints);
  return ({ tag: "right", value: ({
    term: ({ tag: "let", value: ({
    bindings: bins1,
    body: body1
  }) }),
    type: tbody,
    subst: Substitution.composeTypeSubstList([s1, s2, sbody]),
    classConstraints: allConstraints,
    context: fcx4
  }) });
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})();
})();
})();
})();
})()));
}

export function inferTypeOfList(fcx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Term>) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((v1: ReadonlyArray<Core.Term>) => inferTypeOfCollection(fcx)(cx)(((x: Core.Type) => ({ tag: "list", value: x })))(((x: ReadonlyArray<Core.Term>) => ({ tag: "list", value: x })))("list element")(LibSets.empty)(v1)));
}

export function inferTypeOfLiteral(fcx: Context.Context): ((x: Core.Literal) => Typing.InferenceResult) {
  return ((lit: Core.Literal) => ({
    term: ({ tag: "literal", value: lit }),
    type: ({ tag: "literal", value: Reflect.literalType(lit) }),
    subst: Substitution.idTypeSubst,
    classConstraints: LibMaps.empty,
    context: fcx
  }));
}

export function inferTypeOfMap(fcx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyMap<Core.Term, Core.Term>) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const kvarResult = Names.freshName(fcx);
  return (() => {
  const kvar = LibPairs.first(kvarResult);
  return (() => {
  const fcx2 = LibPairs.second(kvarResult);
  return (() => {
  const vvarResult = Names.freshName(fcx2);
  return (() => {
  const vvar = LibPairs.first(vvarResult);
  return (() => {
  const fcx3 = LibPairs.second(vvarResult);
  return (() => {
  const keyConstraints = LibMaps.singleton(kvar)(({
    classes: LibSets.singleton("ordering")
  }));
  return LibLogic.ifElse(LibMaps.null_(m))(({ tag: "right", value: yieldWithConstraints(fcx3)(buildTypeApplicationTerm([kvar, vvar])(({ tag: "map", value: LibMaps.empty })))(({ tag: "map", value: ({
    keys: ({ tag: "variable", value: kvar }),
    values: ({ tag: "variable", value: vvar })
  }) }))(Substitution.idTypeSubst)(keyConstraints) }))(LibEithers.bind(inferMany(fcx3)(cx)(LibLists.map(((k: Core.Term) => [k, "map key"]))(LibMaps.keys(m))))(((kRp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const kResults = LibPairs.first(kRp);
  return (() => {
  const fcx4 = LibPairs.second(kRp);
  return (() => {
  const kterms = LibPairs.first(kResults);
  return (() => {
  const ktypes = LibPairs.first(LibPairs.second(kResults));
  return (() => {
  const ksubst = LibPairs.first(LibPairs.second(LibPairs.second(kResults)));
  return (() => {
  const kElemConstraints = LibPairs.second(LibPairs.second(LibPairs.second(kResults)));
  return LibEithers.bind(inferMany(fcx4)(Substitution.substInContext(ksubst)(cx))(LibLists.map(((v: Core.Term) => [v, "map value"]))(LibMaps.elems(m))))(((vRp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const vResults = LibPairs.first(vRp);
  return (() => {
  const fcx5 = LibPairs.second(vRp);
  return (() => {
  const vterms = LibPairs.first(vResults);
  return (() => {
  const vtypes = LibPairs.first(LibPairs.second(vResults));
  return (() => {
  const vsubst = LibPairs.first(LibPairs.second(LibPairs.second(vResults)));
  return (() => {
  const vElemConstraints = LibPairs.second(LibPairs.second(LibPairs.second(vResults)));
  return (() => {
  const kcons = LibLists.map(((t: Core.Type) => ({
    left: ({ tag: "variable", value: kvar }),
    right: t,
    comment: "map key"
  })))(ktypes);
  return (() => {
  const vcons = LibLists.map(((t: Core.Type) => ({
    left: ({ tag: "variable", value: vvar }),
    right: t,
    comment: "map value"
  })))(vtypes);
  return (() => {
  const allMapConstraints = mergeClassConstraints(keyConstraints)(mergeClassConstraints(kElemConstraints)(vElemConstraints));
  return LibEithers.bind(mapConstraints(fcx5)(cx)(((subst: Typing.TypeSubst) => yieldWithConstraints(fcx5)(({ tag: "map", value: LibMaps.fromList(LibLists.zip(kterms)(vterms)) }))(({ tag: "map", value: ({
    keys: ({ tag: "variable", value: kvar }),
    values: ({ tag: "variable", value: vvar })
  }) }))(Substitution.composeTypeSubstList([ksubst, vsubst, subst]))(Substitution.substInClassConstraints(subst)(allMapConstraints))))(LibLists.concat([kcons, vcons])))(((mcResult: Typing.InferenceResult) => ({ tag: "right", value: mcResult })));
})();
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})();
})())));
})();
})();
})();
})();
})();
})();
})()));
}

export function inferTypeOfOptional(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term | null) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((m: Core.Term | null) => (() => {
  const trmCons = ((terms: ReadonlyArray<Core.Term>) => LibLogic.ifElse(LibLists.null_(terms))(({ tag: "maybe", value: null }))(({ tag: "maybe", value: LibLists.head(terms) })));
  return inferTypeOfCollection(fcx)(cx)(((x: Core.Type) => ({ tag: "maybe", value: x })))(trmCons)("optional element")(LibSets.empty)(LibMaybes.maybe([])(LibLists.singleton)(m));
})()));
}

export function inferTypeOfPair(fcx: Context.Context): ((x: Graph.Graph) => ((x: readonly [Core.Term, Core.Term]) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(inferMany(fcx)(cx)([[LibPairs.first(p), "pair first element"], [LibPairs.second(p), "pair second element"]]))(((rp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const results = LibPairs.first(rp);
  return (() => {
  const fcx2 = LibPairs.second(rp);
  return (() => {
  const iterms = LibPairs.first(results);
  return (() => {
  const itypes = LibPairs.first(LibPairs.second(results));
  return (() => {
  const isubst = LibPairs.first(LibPairs.second(LibPairs.second(results)));
  return (() => {
  const pairElemConstraints = LibPairs.second(LibPairs.second(LibPairs.second(results)));
  return (() => {
  const ifst = LibLists.head(iterms);
  return (() => {
  const isnd = LibLists.head(LibLists.tail(iterms));
  return (() => {
  const tyFst = LibLists.head(itypes);
  return (() => {
  const tySnd = LibLists.head(LibLists.tail(itypes));
  return (() => {
  const pairTerm = ({ tag: "pair", value: [ifst, isnd] });
  return (() => {
  const termWithTypes = ({ tag: "typeApplication", value: ({
    body: ({ tag: "typeApplication", value: ({
    body: pairTerm,
    type: tyFst
  }) }),
    type: tySnd
  }) });
  return ({ tag: "right", value: yieldWithConstraints(fcx2)(termWithTypes)(({ tag: "pair", value: ({
    first: tyFst,
    second: tySnd
  }) }))(isubst)(pairElemConstraints) });
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
})()))));
}

export function inferTypeOfPrimitive(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((name: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchPrimitive", value: ({
    name: name
  }) }) }) }))(((scheme: Core.TypeScheme) => (() => {
  const tsResult = Resolution.instantiateTypeScheme(fcx)(scheme);
  return (() => {
  const ts = LibPairs.first(tsResult);
  return (() => {
  const fcx2 = LibPairs.second(tsResult);
  return (() => {
  const constraints = LibMaybes.fromMaybe(LibMaps.empty)(((_x) => _x.constraints)(ts));
  return ({ tag: "right", value: yieldCheckedWithConstraints(fcx2)(buildTypeApplicationTerm(((_x) => _x.variables)(ts))(({ tag: "variable", value: name })))(((_x) => _x.type)(ts))(Substitution.idTypeSubst)(constraints) });
})();
})();
})();
})()))(LibMaybes.map(((_x) => _x.type))(LibMaps.lookup(name)(((_x) => _x.primitives)(cx))))));
}

export function inferTypeOfProjection(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Projection) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((proj: Core.Projection) => (() => {
  const tname = ((_x) => _x.typeName)(proj);
  return (() => {
  const fname = ((_x) => _x.field)(proj);
  return LibEithers.bind(Resolution.requireSchemaType(fcx)(((_x) => _x.schemaTypes)(cx))(tname))(((stRp: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(stRp);
  return (() => {
  const fcx2 = LibPairs.second(stRp);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const stype = ((_x) => _x.type)(schemaType);
  return LibEithers.bind(ExtractCore.recordType(tname)(stype))(((sfields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(Resolution.findFieldType(fcx2)(fname)(sfields))(((ftyp: Core.Type) => ({ tag: "right", value: yield_(fcx2)(buildTypeApplicationTerm(svars)(({ tag: "project", value: ({
    typeName: tname,
    field: fname
  }) })))(({ tag: "function", value: ({
    domain: Resolution.nominalApplication(tname)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(svars)),
    codomain: ftyp
  }) }))(Substitution.idTypeSubst) })))));
})();
})();
})();
})()));
})();
})()));
}

export function inferTypeOfRecord(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Record) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((record: Core.Record) => (() => {
  const tname = ((_x) => _x.typeName)(record);
  return (() => {
  const fields = ((_x) => _x.fields)(record);
  return (() => {
  const fnames = LibLists.map(((_x) => _x.name))(fields);
  return LibEithers.bind(Resolution.requireSchemaType(fcx)(((_x) => _x.schemaTypes)(cx))(tname))(((stRp: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(stRp);
  return (() => {
  const fcx2 = LibPairs.second(stRp);
  return LibEithers.bind(inferMany(fcx2)(cx)(LibLists.map(((f: Core.Field) => [((_x) => _x.term)(f), LibStrings.cat2("field ")(((_x) => _x)(((_x) => _x.name)(f)))]))(fields)))(((rp: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const results = LibPairs.first(rp);
  return (() => {
  const fcx3 = LibPairs.second(rp);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const stype = ((_x) => _x.type)(schemaType);
  return (() => {
  const iterms = LibPairs.first(results);
  return (() => {
  const itypes = LibPairs.first(LibPairs.second(results));
  return (() => {
  const isubst = LibPairs.first(LibPairs.second(LibPairs.second(results)));
  return (() => {
  const recElemConstraints = LibPairs.second(LibPairs.second(LibPairs.second(results)));
  return (() => {
  const ityp = ({ tag: "record", value: LibLists.zipWith(((n: Core.Name) => ((t: Core.Type) => ({
    name: n,
    type: t
  }))))(fnames)(itypes) });
  return LibEithers.bind(mapConstraints(fcx3)(cx)(((subst: Typing.TypeSubst) => yieldWithConstraints(fcx3)(buildTypeApplicationTerm(svars)(({ tag: "record", value: ({
    typeName: tname,
    fields: LibLists.zipWith(((n: Core.Name) => ((t: Core.Term) => ({
    name: n,
    term: t
  }))))(fnames)(iterms)
  }) })))(Resolution.nominalApplication(tname)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(svars)))(Substitution.composeTypeSubst(isubst)(subst))(Substitution.substInClassConstraints(subst)(recElemConstraints))))([({
    left: stype,
    right: ityp,
    comment: "schema type of record"
  })]))(((mcResult: Typing.InferenceResult) => ({ tag: "right", value: mcResult })));
})();
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})()));
})();
})();
})()));
}

export function inferTypeOfSet(fcx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlySet<Core.Term>) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((s: ReadonlySet<Core.Term>) => inferTypeOfCollection(fcx)(cx)(((x: Core.Type) => ({ tag: "set", value: x })))(((terms: ReadonlyArray<Core.Term>) => ({ tag: "set", value: LibSets.fromList(terms) })))("set element")(LibSets.singleton("ordering"))(LibSets.toList(s))));
}

export function inferTypeOfTerm(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => ((x: string) => Errors.Error | Typing.InferenceResult))) {
  return ((cx: Graph.Graph) => ((term: Core.Term) => ((desc: string) => (() => {
  const fcx2 = ({
    trace: LibLists.cons(desc)(((_x) => _x.trace)(fcx)),
    messages: ((_x) => _x.messages)(fcx),
    other: ((_x) => _x.other)(fcx)
  });
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((a: Core.AnnotatedTerm) => inferTypeOfAnnotatedTerm(fcx2)(cx)(a))((_m as any).value);
    case "application": return ((a: Core.Application) => inferTypeOfApplication(fcx2)(cx)(a))((_m as any).value);
    case "cases": return ((c: Core.CaseStatement) => inferTypeOfCaseStatement(fcx2)(cx)(c))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => inferTypeOfEither(fcx2)(cx)(e))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => inferTypeOfLambda(fcx2)(cx)(l))((_m as any).value);
    case "let": return ((l: Core.Let) => inferTypeOfLet(fcx2)(cx)(l))((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => inferTypeOfList(fcx2)(cx)(els))((_m as any).value);
    case "literal": return ((l: Core.Literal) => ({ tag: "right", value: inferTypeOfLiteral(fcx2)(l) }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => inferTypeOfMap(fcx2)(cx)(m))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => inferTypeOfOptional(fcx2)(cx)(m))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => inferTypeOfPair(fcx2)(cx)(p))((_m as any).value);
    case "project": return ((p: Core.Projection) => inferTypeOfProjection(fcx2)(cx)(p))((_m as any).value);
    case "record": return ((r: Core.Record) => inferTypeOfRecord(fcx2)(cx)(r))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => inferTypeOfSet(fcx2)(cx)(s))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => inferTypeOfTypeApplication(fcx2)(cx)(tt))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => inferTypeOfTypeLambda(fcx2)(cx)(ta))((_m as any).value);
    case "inject": return ((i: Core.Injection) => inferTypeOfInjection(fcx2)(cx)(i))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: inferTypeOfUnit(fcx2) }))((_m as any).value);
    case "unwrap": return ((tname: Core.Name) => inferTypeOfUnwrap(fcx2)(cx)(tname))((_m as any).value);
    case "variable": return ((name: Core.Name) => inferTypeOfVariable(fcx2)(cx)(name))((_m as any).value);
    case "wrap": return ((w: Core.WrappedTerm) => inferTypeOfWrappedTerm(fcx2)(cx)(w))((_m as any).value);
  }
})();
})())));
}

export function inferTypeOfTypeApplication(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.TypeApplicationTerm) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((tt: Core.TypeApplicationTerm) => inferTypeOfTerm(fcx)(cx)(((_x) => _x.body)(tt))("type application term")));
}

export function inferTypeOfTypeLambda(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.TypeLambda) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((ta: Core.TypeLambda) => inferTypeOfTerm(fcx)(cx)(((_x) => _x.body)(ta))("type abstraction")));
}

export function inferTypeOfUnit(fcx: Context.Context): Typing.InferenceResult {
  return ({
    term: ({ tag: "unit" }),
    type: ({ tag: "unit" }),
    subst: Substitution.idTypeSubst,
    classConstraints: LibMaps.empty,
    context: fcx
  });
}

export function inferTypeOfUnwrap(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((tname: Core.Name) => LibEithers.bind(Resolution.requireSchemaType(fcx)(((_x) => _x.schemaTypes)(cx))(tname))(((stRp: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(stRp);
  return (() => {
  const fcx2 = LibPairs.second(stRp);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const stype = ((_x) => _x.type)(schemaType);
  return LibEithers.bind(ExtractCore.wrappedType(tname)(stype))(((wtyp: Core.Type) => ({ tag: "right", value: yield_(fcx2)(buildTypeApplicationTerm(svars)(({ tag: "unwrap", value: tname })))(({ tag: "function", value: ({
    domain: Resolution.nominalApplication(tname)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(svars)),
    codomain: wtyp
  }) }))(Substitution.idTypeSubst) })));
})();
})();
})();
})()))));
}

export function inferTypeOfVariable(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((name: Core.Name) => LibMaybes.maybe(LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noSuchBinding", value: ({
    name: name
  }) }) }) }))(((scheme: Core.TypeScheme) => (() => {
  const tsResult = Resolution.instantiateTypeScheme(fcx)(scheme);
  return (() => {
  const ts = LibPairs.first(tsResult);
  return (() => {
  const fcx2 = LibPairs.second(tsResult);
  return (() => {
  const constraints = LibMaybes.fromMaybe(LibMaps.empty)(((_x) => _x.constraints)(ts));
  return ({ tag: "right", value: yieldCheckedWithConstraints(fcx2)(buildTypeApplicationTerm(((_x) => _x.variables)(ts))(({ tag: "variable", value: name })))(((_x) => _x.type)(ts))(Substitution.idTypeSubst)(constraints) });
})();
})();
})();
})()))(LibMaybes.map(((_x) => _x.type))(LibMaps.lookup(name)(((_x) => _x.primitives)(cx)))))(((scheme: Core.TypeScheme) => (() => {
  const tsResult = Resolution.instantiateTypeScheme(fcx)(scheme);
  return (() => {
  const ts = LibPairs.first(tsResult);
  return (() => {
  const fcx2 = LibPairs.second(tsResult);
  return (() => {
  const constraints = LibMaybes.fromMaybe(LibMaps.empty)(((_x) => _x.constraints)(ts));
  return ({ tag: "right", value: ({
    term: buildTypeApplicationTerm(((_x) => _x.variables)(ts))(({ tag: "variable", value: name })),
    type: ((_x) => _x.type)(ts),
    subst: Substitution.idTypeSubst,
    classConstraints: constraints,
    context: fcx2
  }) });
})();
})();
})();
})()))(LibMaps.lookup(name)(((_x) => _x.boundTypes)(cx)))));
}

export function inferTypeOfWrappedTerm(fcx: Context.Context): ((x: Graph.Graph) => ((x: Core.WrappedTerm) => Errors.Error | Typing.InferenceResult)) {
  return ((cx: Graph.Graph) => ((wt: Core.WrappedTerm) => (() => {
  const tname = ((_x) => _x.typeName)(wt);
  return (() => {
  const term = ((_x) => _x.body)(wt);
  return LibEithers.bind(Resolution.requireSchemaType(fcx)(((_x) => _x.schemaTypes)(cx))(tname))(((stRp: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(stRp);
  return (() => {
  const fcx2 = LibPairs.second(stRp);
  return LibEithers.bind(inferTypeOfTerm(fcx2)(cx)(term)("wrapped term"))(((result: Typing.InferenceResult) => (() => {
  const fcx3 = ((_x) => _x.context)(result);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const stype = ((_x) => _x.type)(schemaType);
  return (() => {
  const iterm = ((_x) => _x.term)(result);
  return (() => {
  const itype = ((_x) => _x.type)(result);
  return (() => {
  const isubst = ((_x) => _x.subst)(result);
  return (() => {
  const ityp = ({ tag: "wrap", value: itype });
  return LibEithers.bind(mapConstraints(fcx3)(cx)(((subst: Typing.TypeSubst) => yield_(fcx3)(buildTypeApplicationTerm(svars)(({ tag: "wrap", value: ({
    typeName: tname,
    body: iterm
  }) })))(Resolution.nominalApplication(tname)(LibLists.map(((x: Core.Name) => ({ tag: "variable", value: x })))(svars)))(Substitution.composeTypeSubst(isubst)(subst))))([({
    left: stype,
    right: ityp,
    comment: "schema type of wrapper"
  })]))(((mcResult: Typing.InferenceResult) => ({ tag: "right", value: mcResult })));
})();
})();
})();
})();
})();
})();
})()));
})();
})()));
})();
})()));
}

export function inferTypesOfTemporaryBindings(fcx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Binding>) => Errors.Error | readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context])) {
  return ((cx: Graph.Graph) => ((bins: ReadonlyArray<Core.Binding>) => LibLogic.ifElse(LibLists.null_(bins))(({ tag: "right", value: [[[], [[], [Substitution.idTypeSubst, LibMaps.empty]]], fcx] }))((() => {
  const dflt = (() => {
  const binding = LibLists.head(bins);
  return (() => {
  const k = ((_x) => _x.name)(binding);
  return (() => {
  const v = ((_x) => _x.term)(binding);
  return (() => {
  const tl = LibLists.tail(bins);
  return LibEithers.bind(inferTypeOfTerm(fcx)(cx)(v)(LibStrings.cat(["temporary let binding '", ((_x) => _x)(k), "'"])))(((result1: Typing.InferenceResult) => (() => {
  const fcx2 = ((_x) => _x.context)(result1);
  return (() => {
  const j = ((_x) => _x.term)(result1);
  return (() => {
  const u_prime = ((_x) => _x.type)(result1);
  return (() => {
  const u = ((_x) => _x.subst)(result1);
  return (() => {
  const c1Inferred = ((_x) => _x.classConstraints)(result1);
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: LibMaps.empty }))(((ts: Core.TypeScheme) => (() => {
  const tsResult = Resolution.instantiateTypeScheme(fcx2)(ts);
  return (() => {
  const instantiatedTs = LibPairs.first(tsResult);
  return (() => {
  const freshConstraints = LibMaybes.fromMaybe(LibMaps.empty)(((_x) => _x.constraints)(instantiatedTs));
  return LibEithers.bind(LibEithers.bimap(((_e: Errors.UnificationError) => ({ tag: "unification", value: _e })))(((_a: Typing.TypeSubst) => _a))(Unification.unifyTypes(fcx2)(((_x) => _x.schemaTypes)(cx))(((_x) => _x.type)(instantiatedTs))(u_prime)("original binding type")))(((unifySubst: Typing.TypeSubst) => ({ tag: "right", value: Substitution.substInClassConstraints(unifySubst)(freshConstraints) })));
})();
})();
})()))(((_x) => _x.type)(binding)))(((originalBindingConstraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => (() => {
  const c1 = mergeClassConstraints(c1Inferred)(originalBindingConstraints);
  return LibEithers.bind(inferTypesOfTemporaryBindings(fcx2)(Substitution.substInContext(u)(cx))(tl))(((rp2: readonly [readonly [ReadonlyArray<Core.Term>, readonly [ReadonlyArray<Core.Type>, readonly [Typing.TypeSubst, ReadonlyMap<Core.Name, Core.TypeVariableMetadata>]]], Context.Context]) => (() => {
  const result2 = LibPairs.first(rp2);
  return (() => {
  const fcx3 = LibPairs.second(rp2);
  return (() => {
  const h = LibPairs.first(result2);
  return (() => {
  const r_prime = LibPairs.first(LibPairs.second(result2));
  return (() => {
  const restPair = LibPairs.second(LibPairs.second(result2));
  return (() => {
  const r = LibPairs.first(restPair);
  return (() => {
  const c2 = LibPairs.second(restPair);
  return (() => {
  const c1Subst = Substitution.substInClassConstraints(r)(c1);
  return (() => {
  const mergedConstraints = mergeClassConstraints(c1Subst)(c2);
  return ({ tag: "right", value: [[LibLists.cons(Substitution.substTypesInTerm(r)(j))(h), [LibLists.cons(Substitution.substInType(r)(u_prime))(r_prime), [Substitution.composeTypeSubst(u)(r), mergedConstraints]]], fcx3] });
})();
})();
})();
})();
})();
})();
})();
})();
})()));
})()));
})();
})();
})();
})();
})()));
})();
})();
})();
})();
  return dflt;
})())));
}

export function isUnbound(cx: Graph.Graph): ((x: Core.Name) => boolean) {
  return ((v: Core.Name) => LibLogic.and(LibLogic.not(LibSets.member(v)(freeVariablesInContext(cx))))(LibLogic.not(LibMaps.member(v)(((_x) => _x.schemaTypes)(cx)))));
}

export function mapConstraints<t0, t1>(flowCx: t0): ((x: Graph.Graph) => ((x: ((x: Typing.TypeSubst) => t1)) => ((x: ReadonlyArray<Typing.TypeConstraint>) => Errors.Error | t1))) {
  return ((cx: Graph.Graph) => ((f: ((x: Typing.TypeSubst) => t1)) => ((constraints: ReadonlyArray<Typing.TypeConstraint>) => LibEithers.bind(LibEithers.bimap(((_e: Errors.UnificationError) => ({ tag: "unification", value: _e })))(((_a: Typing.TypeSubst) => _a))(Unification.unifyTypeConstraints(flowCx)(((_x) => _x.schemaTypes)(cx))(constraints)))(((s: Typing.TypeSubst) => LibEithers.bind(Checking.checkTypeSubst(flowCx)(cx)(s))(((_: Typing.TypeSubst) => ({ tag: "right", value: f(s) }))))))));
}

export function mergeClassConstraints<t0>(m1: ReadonlyMap<t0, Core.TypeVariableMetadata>): ((x: ReadonlyMap<t0, Core.TypeVariableMetadata>) => ReadonlyMap<t0, Core.TypeVariableMetadata>) {
  return ((m2: ReadonlyMap<t0, Core.TypeVariableMetadata>) => LibLists.foldl(((acc: ReadonlyMap<t0, Core.TypeVariableMetadata>) => ((pair: readonly [t0, Core.TypeVariableMetadata]) => (() => {
  const k = LibPairs.first(pair);
  return (() => {
  const v = LibPairs.second(pair);
  return LibMaybes.maybe(LibMaps.insert(k)(v)(acc))(((existing: Core.TypeVariableMetadata) => (() => {
  const merged = ({
    classes: LibSets.union(((_x) => _x.classes)(existing))(((_x) => _x.classes)(v))
  });
  return LibMaps.insert(k)(merged)(acc);
})()))(LibMaps.lookup(k)(acc));
})();
})())))(m1)(LibMaps.toList(m2)));
}

export function showInferenceResult(result: Typing.InferenceResult): string {
  return (() => {
  const term = ((_x) => _x.term)(result);
  return (() => {
  const typ = ((_x) => _x.type)(result);
  return (() => {
  const subst = ((_x) => _x.subst)(result);
  return LibStrings.cat(["{term=", ShowCore.term(term), ", type=", ShowCore.type(typ), ", subst=", ShowTyping.typeSubst(subst), "}"]);
})();
})();
})();
}

export function yield_(fcx: Context.Context): ((x: Core.Term) => ((x: Core.Type) => ((x: Typing.TypeSubst) => Typing.InferenceResult))) {
  return ((term: Core.Term) => ((typ: Core.Type) => ((subst: Typing.TypeSubst) => ({
    term: Substitution.substTypesInTerm(subst)(term),
    type: Substitution.substInType(subst)(typ),
    subst: subst,
    classConstraints: LibMaps.empty,
    context: fcx
  }))));
}

export function yieldChecked(fcx: Context.Context): ((x: Core.Term) => ((x: Core.Type) => ((x: Typing.TypeSubst) => Typing.InferenceResult))) {
  return ((term: Core.Term) => ((typ: Core.Type) => ((subst: Typing.TypeSubst) => (() => {
  const iterm = Substitution.substTypesInTerm(subst)(term);
  return (() => {
  const itype = Substitution.substInType(subst)(typ);
  return ({
    term: iterm,
    type: itype,
    subst: subst,
    classConstraints: LibMaps.empty,
    context: fcx
  });
})();
})())));
}

export function yieldCheckedWithConstraints(fcx: Context.Context): ((x: Core.Term) => ((x: Core.Type) => ((x: Typing.TypeSubst) => ((x: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => Typing.InferenceResult)))) {
  return ((term: Core.Term) => ((typ: Core.Type) => ((subst: Typing.TypeSubst) => ((constraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => (() => {
  const iterm = Substitution.substTypesInTerm(subst)(term);
  return (() => {
  const itype = Substitution.substInType(subst)(typ);
  return (() => {
  const iconstraints = Substitution.substInClassConstraints(subst)(constraints);
  return ({
    term: iterm,
    type: itype,
    subst: subst,
    classConstraints: iconstraints,
    context: fcx
  });
})();
})();
})()))));
}

export function yieldDebug<t0>(fcx: Context.Context): ((x: t0) => ((x: string) => ((x: Core.Term) => ((x: Core.Type) => ((x: Typing.TypeSubst) => Errors.Error | Typing.InferenceResult))))) {
  return ((cx: t0) => ((debugId: string) => ((term: Core.Term) => ((typ: Core.Type) => ((subst: Typing.TypeSubst) => (() => {
  const rterm = Substitution.substTypesInTerm(subst)(term);
  return (() => {
  const rtyp = Substitution.substInType(subst)(typ);
  return LibEithers.bind(Annotations.debugIf(fcx)(debugId)(LibStrings.cat(["\n\tterm: ", ShowCore.term(term), "\n\ttyp: ", ShowCore.type(typ), "\n\tsubst: ", ShowTyping.typeSubst(subst), "\n\trterm: ", ShowCore.term(rterm), "\n\trtyp: ", ShowCore.type(rtyp)])))(((result: void) => ({ tag: "right", value: ({
    term: rterm,
    type: rtyp,
    subst: subst,
    classConstraints: LibMaps.empty,
    context: fcx
  }) })));
})();
})())))));
}

export function yieldWithConstraints(fcx: Context.Context): ((x: Core.Term) => ((x: Core.Type) => ((x: Typing.TypeSubst) => ((x: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => Typing.InferenceResult)))) {
  return ((term: Core.Term) => ((typ: Core.Type) => ((subst: Typing.TypeSubst) => ((constraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => ({
    term: Substitution.substTypesInTerm(subst)(term),
    type: Substitution.substInType(subst)(typ),
    subst: subst,
    classConstraints: constraints,
    context: fcx
  })))));
}
