// Note: this is an automatically generated file. Do not edit.

/**
 * Variable substitution in type and term expressions.
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
import * as Rewriting from "./rewriting.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function composeTypeSubst(s1: Typing.TypeSubst): ((x: Typing.TypeSubst) => Typing.TypeSubst) {
  return ((s2: Typing.TypeSubst) => LibLogic.ifElse(LibMaps.null_(((_x) => _x)(s1)))(s2)(LibLogic.ifElse(LibMaps.null_(((_x) => _x)(s2)))(s1)(composeTypeSubstNonEmpty(s1)(s2))));
}

export function composeTypeSubstList(v1): Typing.TypeSubst {
  return LibLists.foldl(composeTypeSubst)(idTypeSubst)(v1);
}

export function composeTypeSubstNonEmpty(s1: Typing.TypeSubst): ((x: Typing.TypeSubst) => Typing.TypeSubst) {
  return ((s2: Typing.TypeSubst) => (() => {
  const isExtra = ((k: Core.Name) => ((v: t0) => LibMaybes.isNothing(LibMaps.lookup(k)(((_x) => _x)(s1)))));
  const withExtra = LibMaps.filterWithKey(isExtra)(((_x) => _x)(s2));
  return LibMaps.union(withExtra)(LibMaps.map(((v1: Core.Type) => substInType(s2)(v1)))(((_x) => _x)(s1)));
})());
}

export const idTypeSubst: Typing.TypeSubst = LibMaps.empty;

export function singletonTypeSubst(v: Core.Name): ((x: Core.Type) => Typing.TypeSubst) {
  return ((t: Core.Type) => LibMaps.singleton(v)(t));
}

export function substInClassConstraints(subst: Typing.TypeSubst): ((x: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) {
  return ((constraints: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => (() => {
  const substMap = ((_x) => _x)(subst);
  return (() => {
  const insertOrMerge = ((varName: t0) => ((metadata: Core.TypeVariableMetadata) => ((acc: ReadonlyMap<t0, Core.TypeVariableMetadata>) => LibMaybes.maybe(LibMaps.insert(varName)(metadata)(acc))(((existing: Core.TypeVariableMetadata) => (() => {
  const merged = ({
    classes: LibSets.union(((_x) => _x.classes)(existing))(((_x) => _x.classes)(metadata))
  });
  return LibMaps.insert(varName)(merged)(acc);
})()))(LibMaps.lookup(varName)(acc)))));
  return LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => ((pair: readonly [Core.Name, Core.TypeVariableMetadata]) => (() => {
  const varName = LibPairs.first(pair);
  return (() => {
  const metadata = LibPairs.second(pair);
  return LibMaybes.maybe(insertOrMerge(varName)(metadata)(acc))(((targetType: Core.Type) => (() => {
  const freeVars = LibSets.toList(Variables.freeVariablesInType(targetType));
  return LibLists.foldl(((acc2: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => ((freeVar: Core.Name) => insertOrMerge(freeVar)(metadata)(acc2))))(acc)(freeVars);
})()))(LibMaps.lookup(varName)(substMap));
})();
})())))(LibMaps.empty)(LibMaps.toList(constraints));
})();
})());
}

export function substInContext(subst: Typing.TypeSubst): ((x: Graph.Graph) => Graph.Graph) {
  return ((cx: Graph.Graph) => (() => {
  const newBoundTypes = LibMaps.map(((v1: Core.TypeScheme) => substInTypeScheme(subst)(v1)))(((_x) => _x.boundTypes)(cx));
  return (() => {
  const newClassConstraints = substInClassConstraints(subst)(((_x) => _x.classConstraints)(cx));
  return (() => {
  const cx2 = ({
    boundTerms: ((_x) => _x.boundTerms)(cx),
    boundTypes: newBoundTypes,
    classConstraints: ((_x) => _x.classConstraints)(cx),
    lambdaVariables: ((_x) => _x.lambdaVariables)(cx),
    metadata: ((_x) => _x.metadata)(cx),
    primitives: ((_x) => _x.primitives)(cx),
    schemaTypes: ((_x) => _x.schemaTypes)(cx),
    typeVariables: ((_x) => _x.typeVariables)(cx)
  });
  return ({
    boundTerms: ((_x) => _x.boundTerms)(cx2),
    boundTypes: ((_x) => _x.boundTypes)(cx2),
    classConstraints: newClassConstraints,
    lambdaVariables: ((_x) => _x.lambdaVariables)(cx2),
    metadata: ((_x) => _x.metadata)(cx2),
    primitives: ((_x) => _x.primitives)(cx2),
    schemaTypes: ((_x) => _x.schemaTypes)(cx2),
    typeVariables: ((_x) => _x.typeVariables)(cx2)
  });
})();
})();
})());
}

export function substInType(subst: Typing.TypeSubst): ((x: Core.Type) => Core.Type) {
  return ((typ0: Core.Type) => LibLogic.ifElse(LibMaps.null_(((_x) => _x)(subst)))(typ0)(substInTypeNonEmpty(subst)(typ0)));
}

export function substInTypeNonEmpty(subst: Typing.TypeSubst): ((x: Core.Type) => Core.Type) {
  return ((typ0: Core.Type) => (() => {
  const rewrite = ((recurse: ((x: Core.Type) => Core.Type)) => ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "forall": return ((lt: Core.ForallType) => LibMaybes.maybe(recurse(typ))(((styp: Core.Type) => ({ tag: "forall", value: ({
    parameter: ((_x) => _x.parameter)(lt),
    body: substInType(removeVar(((_x) => _x.parameter)(lt)))(((_x) => _x.body)(lt))
  }) })))(LibMaps.lookup(((_x) => _x.parameter)(lt))(((_x) => _x)(subst))))((_m as any).value);
    case "variable": return ((v: Core.Name) => LibMaybes.maybe(typ)(((styp: Core.Type) => styp))(LibMaps.lookup(v)(((_x) => _x)(subst))))((_m as any).value);
    default: return recurse(typ)(_m);
  }
})()));
  const removeVar = ((v: Core.Name) => LibMaps.delete_(v)(((_x) => _x)(subst)));
  return Rewriting.rewriteType(rewrite)(typ0);
})());
}

export function substInTypeScheme(subst: Typing.TypeSubst): ((x: Core.TypeScheme) => Core.TypeScheme) {
  return ((ts: Core.TypeScheme) => ({
    variables: ((_x) => _x.variables)(ts),
    type: substInType(subst)(((_x) => _x.type)(ts)),
    constraints: LibMaybes.map(((v1: ReadonlyMap<Core.Name, Core.TypeVariableMetadata>) => substInClassConstraints(subst)(v1)))(((_x) => _x.constraints)(ts))
  }));
}

export function substTypesInTerm(subst: Typing.TypeSubst): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const dflt = recurse(term);
  const forLambda = ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: LibMaybes.map(((v1: Core.Type) => substInType(subst)(v1)))(((_x) => _x.domain)(l)),
    body: substTypesInTerm(subst)(((_x) => _x.body)(l))
  }) }));
  const forLet = ((l: Core.Let) => (() => {
  const rewriteBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: substTypesInTerm(subst)(((_x) => _x.term)(b)),
    type: LibMaybes.map(((v1: Core.TypeScheme) => substInTypeScheme(subst)(v1)))(((_x) => _x.type)(b))
  }));
  return ({ tag: "let", value: ({
    bindings: LibLists.map(rewriteBinding)(((_x) => _x.bindings)(l)),
    body: substTypesInTerm(subst)(((_x) => _x.body)(l))
  }) });
})());
  const forTypeApplication = ((tt: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: substTypesInTerm(subst)(((_x) => _x.body)(tt)),
    type: substInType(subst)(((_x) => _x.type)(tt))
  }) }));
  const forTypeLambda = ((ta: Core.TypeLambda) => (() => {
  const param = ((_x) => _x.parameter)(ta);
  const subst2 = LibMaps.delete_(param)(((_x) => _x)(subst));
  return ({ tag: "typeLambda", value: ({
    parameter: param,
    body: substTypesInTerm(subst2)(((_x) => _x.body)(ta))
  }) });
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => forLambda(l))((_m as any).value);
    case "let": return ((l: Core.Let) => forLet(l))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => forTypeApplication(ta))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => forTypeLambda(tl))((_m as any).value);
    default: return dflt(_m);
  }
})();
})()));
  return Rewriting.rewriteTerm(rewrite)(term0);
})());
}

export function substituteInBinding(subst: Typing.TermSubst): ((x: Core.Binding) => Core.Binding) {
  return ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: substituteInTerm(subst)(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
}

export function substituteInConstraint(subst: Typing.TypeSubst): ((x: Typing.TypeConstraint) => Typing.TypeConstraint) {
  return ((c: Typing.TypeConstraint) => ({
    left: substInType(subst)(((_x) => _x.left)(c)),
    right: substInType(subst)(((_x) => _x.right)(c)),
    comment: ((_x) => _x.comment)(c)
  }));
}

export function substituteInConstraints(subst: Typing.TypeSubst): ((x: ReadonlyArray<Typing.TypeConstraint>) => ReadonlyArray<Typing.TypeConstraint>) {
  return ((cs: ReadonlyArray<Typing.TypeConstraint>) => LibLists.map(((v1: Typing.TypeConstraint) => substituteInConstraint(subst)(v1)))(cs));
}

export function substituteInTerm(subst: Typing.TermSubst): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const s = ((_x) => _x)(subst);
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const withLambda = ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  const subst2 = LibMaps.delete_(v)(s);
  return ({ tag: "lambda", value: ({
    parameter: v,
    domain: ((_x) => _x.domain)(l),
    body: substituteInTerm(subst2)(((_x) => _x.body)(l))
  }) });
})());
  const withLet = ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  const names = LibSets.fromList(LibLists.map(((_x) => _x.name))(bindings));
  const subst2 = LibMaps.filterWithKey(((k: Core.Name) => ((v: Core.Term) => LibLogic.not(LibSets.member(k)(names)))))(s);
  const rewriteBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: substituteInTerm(subst2)(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return ({ tag: "let", value: ({
    bindings: LibLists.map(rewriteBinding)(bindings),
    body: substituteInTerm(subst2)(((_x) => _x.body)(lt))
  }) });
})());
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => withLambda(l))((_m as any).value);
    case "let": return ((l: Core.Let) => withLet(l))((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(recurse(term))(((sterm: Core.Term) => sterm))(LibMaps.lookup(name)(s)))((_m as any).value);
    default: return recurse(term)(_m);
  }
})();
})()));
  return Rewriting.rewriteTerm(rewrite)(term0);
})());
}
