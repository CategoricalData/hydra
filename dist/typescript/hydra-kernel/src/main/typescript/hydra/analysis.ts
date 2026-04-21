// Note: this is an automatically generated file. Do not edit.

/**
 * Module dependency namespace analysis
 */



import * as Annotations from "./annotations.js";
import * as Arity from "./arity.js";
import * as Ast from "./ast.js";
import * as Checking from "./checking.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as Dependencies from "./dependencies.js";
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
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Predicates from "./predicates.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as Scoping from "./scoping.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function addNamesToNamespaces<t0>(encodeNamespace: ((x: Packaging.Namespace) => t0)): ((x: ReadonlySet<Core.Name>) => ((x: Packaging.Namespaces<t0>) => Packaging.Namespaces<t0>)) {
  return ((names: ReadonlySet<Core.Name>) => ((ns0: Packaging.Namespaces<t0>) => (() => {
  const nss = LibSets.fromList(LibMaybes.cat(LibLists.map(Names.namespaceOf)(LibSets.toList(names))));
  return (() => {
  const toPair = ((ns: Packaging.Namespace) => [ns, encodeNamespace(ns)]);
  return ({
    focus: ((_x) => _x.focus)(ns0),
    mapping: LibMaps.union(((_x) => _x.mapping)(ns0))(LibMaps.fromList(LibLists.map(toPair)(LibSets.toList(nss))))
  });
})();
})()));
}

export function analyzeFunctionTerm<t0, t1>(cx: Context.Context): ((x: ((x: t0) => Graph.Graph)) => ((x: ((x: Graph.Graph) => ((x: t0) => t0))) => ((x: t0) => ((x: Core.Term) => t1 | Typing.FunctionStructure<t0>)))) {
  return ((getTC: ((x: t0) => Graph.Graph)) => ((setTC: ((x: Graph.Graph) => ((x: t0) => t0))) => ((env: t0) => ((term: Core.Term) => analyzeFunctionTermWith(cx)(((g: Graph.Graph) => ((b: Core.Binding) => LibLogic.ifElse(Predicates.isComplexBinding(g)(b))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(null))))(getTC)(setTC)(env)(term)))));
}

export function analyzeFunctionTermWith<t0, t1>(cx: Context.Context): ((x: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))) => ((x: ((x: t0) => Graph.Graph)) => ((x: ((x: Graph.Graph) => ((x: t0) => t0))) => ((x: t0) => ((x: Core.Term) => t1 | Typing.FunctionStructure<t0>))))) {
  return ((forBinding: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))) => ((getTC: ((x: t0) => Graph.Graph)) => ((setTC: ((x: Graph.Graph) => ((x: t0) => t0))) => ((env: t0) => ((term: Core.Term) => analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(true)(env)([])([])([])([])([])(term))))));
}

export function analyzeFunctionTermWith_finish<t0, t1>(cx: Context.Context): ((x: ((x: t0) => Graph.Graph)) => ((x: t0) => ((x: ReadonlyArray<Core.Name>) => ((x: ReadonlyArray<Core.Name>) => ((x: ReadonlyArray<Core.Binding>) => ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Term) => t1 | Typing.FunctionStructure<t0>)))))))) {
  return ((getTC: ((x: t0) => Graph.Graph)) => ((fEnv: t0) => ((tparams: ReadonlyArray<Core.Name>) => ((args: ReadonlyArray<Core.Name>) => ((bindings: ReadonlyArray<Core.Binding>) => ((doms: ReadonlyArray<Core.Type>) => ((tapps: ReadonlyArray<Core.Type>) => ((body: Core.Term) => (() => {
  const bodyWithTapps = LibLists.foldl(((trm: Core.Term) => ((typ: Core.Type) => ({ tag: "typeApplication", value: ({
    body: trm,
    type: typ
  }) }))))(body)(tapps);
  return (() => {
  const mcod = LibEithers.either(((_: Errors.Error) => null))(((c: Core.Type) => c))(Checking.typeOfTerm(cx)(getTC(fEnv))(bodyWithTapps));
  return ({ tag: "right", value: ({
    typeParams: LibLists.reverse(tparams),
    params: LibLists.reverse(args),
    bindings: bindings,
    body: bodyWithTapps,
    domains: LibLists.reverse(doms),
    codomain: mcod,
    environment: fEnv
  }) });
})();
})()))))))));
}

export function analyzeFunctionTermWith_gather<t0, t1>(cx: Context.Context): ((x: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))) => ((x: ((x: t0) => Graph.Graph)) => ((x: ((x: Graph.Graph) => ((x: t0) => t0))) => ((x: boolean) => ((x: t0) => ((x: ReadonlyArray<Core.Name>) => ((x: ReadonlyArray<Core.Name>) => ((x: ReadonlyArray<Core.Binding>) => ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Term) => t1 | Typing.FunctionStructure<t0>))))))))))) {
  return ((forBinding: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))) => ((getTC: ((x: t0) => Graph.Graph)) => ((setTC: ((x: Graph.Graph) => ((x: t0) => t0))) => ((argMode: boolean) => ((gEnv: t0) => ((tparams: ReadonlyArray<Core.Name>) => ((args: ReadonlyArray<Core.Name>) => ((bindings: ReadonlyArray<Core.Binding>) => ((doms: ReadonlyArray<Core.Type>) => ((tapps: ReadonlyArray<Core.Type>) => ((t: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibLogic.ifElse(argMode)((() => {
  const v = ((_x) => _x.parameter)(lam);
  return (() => {
  const dom = LibMaybes.maybe(({ tag: "variable", value: "_" }))(((x_: Core.Type) => x_))(((_x) => _x.domain)(lam));
  return (() => {
  const body = ((_x) => _x.body)(lam);
  return (() => {
  const newEnv = setTC(Scoping.extendGraphForLambda(getTC(gEnv))(lam))(gEnv);
  return analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(newEnv)(tparams)(LibLists.cons(v)(args))(bindings)(LibLists.cons(dom)(doms))(tapps)(body);
})();
})();
})();
})())(analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t)))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const newBindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return (() => {
  const newEnv = setTC(Scoping.extendGraphForLet(forBinding)(getTC(gEnv))(lt))(gEnv);
  return analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(false)(newEnv)(tparams)(args)(LibLists.concat2(bindings)(newBindings))(doms)(tapps)(body);
})();
})();
})())((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const taBody = ((_x) => _x.body)(ta);
  return (() => {
  const typ = ((_x) => _x.type)(ta);
  return analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(gEnv)(tparams)(args)(bindings)(doms)(LibLists.cons(typ)(tapps))(taBody);
})();
})())((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const tvar = ((_x) => _x.parameter)(tl);
  return (() => {
  const tlBody = ((_x) => _x.body)(tl);
  return (() => {
  const newEnv = setTC(Scoping.extendGraphForTypeLambda(getTC(gEnv))(tl))(gEnv);
  return analyzeFunctionTermWith_gather(cx)(forBinding)(getTC)(setTC)(argMode)(newEnv)(LibLists.cons(tvar)(tparams))(args)(bindings)(doms)(tapps)(tlBody);
})();
})();
})())((_m as any).value);
    default: return analyzeFunctionTermWith_finish(cx)(getTC)(gEnv)(tparams)(args)(bindings)(doms)(tapps)(t)(_m);
  }
})())))))))))));
}

export function definitionDependencyNamespaces(defs: ReadonlyArray<Packaging.Definition>): ReadonlySet<Packaging.Namespace> {
  return (() => {
  const defNames = ((def: Packaging.Definition) => (() => {
  const _m = def;
  switch (_m.tag) {
    case "type": return ((typeDef: Packaging.TypeDefinition) => Dependencies.typeDependencyNames(true)(((_x) => _x.type)(((_x) => _x.type)(typeDef))))((_m as any).value);
    case "term": return ((termDef: Packaging.TermDefinition) => Dependencies.termDependencyNames(true)(true)(true)(((_x) => _x.term)(termDef)))((_m as any).value);
  }
})());
  return (() => {
  const allNames = LibSets.unions(LibLists.map(defNames)(defs));
  return LibSets.fromList(LibMaybes.cat(LibLists.map(Names.namespaceOf)(LibSets.toList(allNames))));
})();
})();
}

export function dependencyNamespaces<t0>(cx: t0): ((x: Graph.Graph) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: ReadonlyArray<Core.Binding>) => Errors.Error | ReadonlySet<Packaging.Namespace>)))))) {
  return ((graph: Graph.Graph) => ((binds: boolean) => ((withPrims: boolean) => ((withNoms: boolean) => ((withSchema: boolean) => ((els: ReadonlyArray<Core.Binding>) => (() => {
  const depNames = ((el: Core.Binding) => (() => {
  const term = ((_x) => _x.term)(el);
  return (() => {
  const deannotatedTerm = Strip.deannotateTerm(term);
  return (() => {
  const dataNames = Dependencies.termDependencyNames(binds)(withPrims)(withNoms)(term);
  return (() => {
  const schemaNames = LibLogic.ifElse(withSchema)(LibMaybes.maybe(LibSets.empty)(((ts: Core.TypeScheme) => Dependencies.typeDependencyNames(true)(((_x) => _x.type)(ts))))(((_x) => _x.type)(el)))(LibSets.empty);
  return LibLogic.ifElse(Predicates.isEncodedType(deannotatedTerm))(LibEithers.map(((typ: Core.Type) => LibSets.unions([dataNames, schemaNames, Dependencies.typeDependencyNames(true)(typ)])))(LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((_a: Core.Type) => _a))(DecodeCore.type(graph)(term))))(LibLogic.ifElse(Predicates.isEncodedTerm(deannotatedTerm))(LibEithers.map(((decodedTerm: Core.Term) => LibSets.unions([dataNames, schemaNames, Dependencies.termDependencyNames(binds)(withPrims)(withNoms)(decodedTerm)])))(LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((_a: Core.Term) => _a))(DecodeCore.term(graph)(term))))(({ tag: "right", value: LibSets.unions([dataNames, schemaNames]) })));
})();
})();
})();
})());
  return LibEithers.map(((namesList: ReadonlyArray<ReadonlySet<Core.Name>>) => LibSets.fromList(LibMaybes.cat(LibLists.map(Names.namespaceOf)(LibSets.toList(LibSets.unions(namesList)))))))(LibEithers.mapList(depNames)(els));
})()))))));
}

export function gatherApplications(term: Core.Term): readonly [ReadonlyArray<Core.Term>, Core.Term] {
  return (() => {
  const go = ((args: ReadonlyArray<Core.Term>) => ((t: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return go(LibLists.cons(rhs)(args))(lhs);
})();
})())((_m as any).value);
    default: return [args, t](_m);
  }
})()));
  return go([])(term);
})();
}

export function gatherArgs(term: Core.Term): ((x: ReadonlyArray<Core.Term>) => readonly [Core.Term, ReadonlyArray<Core.Term>]) {
  return ((args: ReadonlyArray<Core.Term>) => (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return gatherArgs(lhs)(LibLists.cons(rhs)(args));
})();
})())((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const body = ((_x) => _x.body)(tl);
  return gatherArgs(body)(args);
})())((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const body = ((_x) => _x.body)(ta);
  return gatherArgs(body)(args);
})())((_m as any).value);
    default: return [term, args](_m);
  }
})());
}

export function gatherArgsWithTypeApps(term: Core.Term): ((x: ReadonlyArray<Core.Term>) => ((x: ReadonlyArray<Core.Type>) => readonly [Core.Term, readonly [ReadonlyArray<Core.Term>, ReadonlyArray<Core.Type>]])) {
  return ((args: ReadonlyArray<Core.Term>) => ((tyArgs: ReadonlyArray<Core.Type>) => (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return gatherArgsWithTypeApps(lhs)(LibLists.cons(rhs)(args))(tyArgs);
})();
})())((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const body = ((_x) => _x.body)(tl);
  return gatherArgsWithTypeApps(body)(args)(tyArgs);
})())((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const body = ((_x) => _x.body)(ta);
  return (() => {
  const typ = ((_x) => _x.type)(ta);
  return gatherArgsWithTypeApps(body)(args)(LibLists.cons(typ)(tyArgs));
})();
})())((_m as any).value);
    default: return [term, [args, tyArgs]](_m);
  }
})()));
}

export function isSelfTailRecursive(funcName: Core.Name): ((x: Core.Term) => boolean) {
  return ((body: Core.Term) => (() => {
  const callsSelf = LibLogic.not(Variables.isFreeVariableInTerm(funcName)(body));
  return LibLogic.ifElse(callsSelf)(isTailRecursiveInTailPosition(funcName)(body))(false);
})());
}

export function isSimpleAssignment(term: Core.Term): boolean {
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => isSimpleAssignment(((_x) => _x.body)(at)))((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => false)((_m as any).value);
    case "let": return ((_: Core.Let) => false)((_m as any).value);
    case "typeLambda": return ((_: Core.TypeLambda) => false)((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => isSimpleAssignment(((_x) => _x.body)(ta)))((_m as any).value);
    default: return (() => {
  const baseTerm = LibPairs.first(gatherArgs(term)([]));
  return (() => {
  const _m = baseTerm;
  switch (_m.tag) {
    case "cases": return ((_: Core.CaseStatement) => false)((_m as any).value);
    default: return true(_m);
  }
})();
})()(_m);
  }
})();
}

export function isTailRecursiveInTailPosition(funcName: Core.Name): ((x: Core.Term) => boolean) {
  return ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateAndDetypeTerm(term);
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const gathered = gatherApplications(stripped);
  return (() => {
  const gatherArgs = LibPairs.first(gathered);
  return (() => {
  const gatherFun = LibPairs.second(gathered);
  return (() => {
  const strippedFun = Strip.deannotateAndDetypeTerm(gatherFun);
  return (() => {
  const _m = strippedFun;
  switch (_m.tag) {
    case "variable": return ((vname: Core.Name) => LibLogic.ifElse(LibEquality.equal(vname)(funcName))((() => {
  const argsNoFunc = LibLists.foldl(((ok: boolean) => ((arg: Core.Term) => LibLogic.and(ok)(Variables.isFreeVariableInTerm(funcName)(arg)))))(true)(gatherArgs);
  return (() => {
  const argsNoLambda = LibLists.foldl(((ok: boolean) => ((arg: Core.Term) => LibLogic.and(ok)(LibLogic.not(Rewriting.foldOverTerm(({ tag: "pre" }))(((found: boolean) => ((t: Core.Term) => LibLogic.or(found)((() => {
  const _m = t;
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const ignore = ((_x) => _x.body)(lam);
  return true;
})())((_m as any).value);
    default: return false(_m);
  }
})()))))(false)(arg))))))(true)(gatherArgs);
  return LibLogic.and(argsNoFunc)(argsNoLambda);
})();
})())(Variables.isFreeVariableInTerm(funcName)(term)))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const branchesOk = LibLists.foldl(((ok: boolean) => ((field: Core.Field) => LibLogic.and(ok)(isTailRecursiveInTailPosition(funcName)(((_x) => _x.term)(field))))))(true)(cases_);
  return (() => {
  const dfltOk = LibMaybes.maybe(true)(((d: Core.Term) => isTailRecursiveInTailPosition(funcName)(d)))(dflt);
  return (() => {
  const argsOk = LibLists.foldl(((ok: boolean) => ((arg: Core.Term) => LibLogic.and(ok)(Variables.isFreeVariableInTerm(funcName)(arg)))))(true)(gatherArgs);
  return LibLogic.and(LibLogic.and(branchesOk)(dfltOk))(argsOk);
})();
})();
})();
})();
})())((_m as any).value);
    default: return Variables.isFreeVariableInTerm(funcName)(term)(_m);
  }
})();
})();
})();
})();
})())((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => isTailRecursiveInTailPosition(funcName)(((_x) => _x.body)(lam)))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindingsOk = LibLists.foldl(((ok: boolean) => ((b: Core.Binding) => LibLogic.and(ok)(Variables.isFreeVariableInTerm(funcName)(((_x) => _x.term)(b))))))(true)(((_x) => _x.bindings)(lt));
  return LibLogic.and(bindingsOk)(isTailRecursiveInTailPosition(funcName)(((_x) => _x.body)(lt)));
})())((_m as any).value);
    default: return Variables.isFreeVariableInTerm(funcName)(term)(_m);
  }
})();
})());
}

export function moduleContainsBinaryLiterals(mod: Packaging.Module): boolean {
  return (() => {
  const checkTerm = ((found: boolean) => ((term: Core.Term) => LibLogic.or(found)((() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "binary": return ((_: Uint8Array) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})())));
  return (() => {
  const termContainsBinary = ((term: Core.Term) => Rewriting.foldOverTerm(({ tag: "pre" }))(checkTerm)(false)(term));
  return (() => {
  const defTerms = LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ((_x) => _x.term)(td))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(mod)));
  return LibLists.foldl(((acc: boolean) => ((t: Core.Term) => LibLogic.or(acc)(termContainsBinary(t)))))(false)(defTerms);
})();
})();
})();
}

export function moduleDependencyNamespaces<t0>(cx: t0): ((x: Graph.Graph) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: Packaging.Module) => Errors.Error | ReadonlySet<Packaging.Namespace>)))))) {
  return ((graph: Graph.Graph) => ((binds: boolean) => ((withPrims: boolean) => ((withNoms: boolean) => ((withSchema: boolean) => ((mod: Packaging.Module) => (() => {
  const allBindings = LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => (() => {
  const schemaTerm = ({ tag: "variable", value: "hydra.core.Type" });
  return (() => {
  const dataTerm = Annotations.normalizeTermAnnotations(({ tag: "annotated", value: ({
    body: EncodeCore.type(((_x) => _x.type)(((_x) => _x.type)(td))),
    annotation: LibMaps.fromList([[Constants.key_type, schemaTerm]])
  }) }));
  return ({
    name: ((_x) => _x.name)(td),
    term: dataTerm,
    type: ({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.Type" }),
    constraints: null
  })
  });
})();
})())((_m as any).value);
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(mod)));
  return LibEithers.map(((deps: ReadonlySet<Packaging.Namespace>) => LibSets.delete_(((_x) => _x.namespace)(mod))(deps)))(dependencyNamespaces(cx)(graph)(binds)(withPrims)(withNoms)(withSchema)(allBindings));
})()))))));
}

export function namespacesForDefinitions<t0>(encodeNamespace: ((x: Packaging.Namespace) => t0)): ((x: Packaging.Namespace) => ((x: ReadonlyArray<Packaging.Definition>) => Packaging.Namespaces<t0>)) {
  return ((focusNs: Packaging.Namespace) => ((defs: ReadonlyArray<Packaging.Definition>) => (() => {
  const nss = LibSets.delete_(focusNs)(definitionDependencyNamespaces(defs));
  return (() => {
  const toPair = ((ns: Packaging.Namespace) => [ns, encodeNamespace(ns)]);
  return ({
    focus: toPair(focusNs),
    mapping: LibMaps.fromList(LibLists.map(toPair)(LibSets.toList(nss)))
  });
})();
})()));
}
