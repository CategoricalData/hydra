// Note: this is an automatically generated file. Do not edit.

/**
 * Free variable analysis, term-level substitution, and unshadowing
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
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function freeTypeVariablesInTerm(term0: Core.Term): ReadonlySet<Core.Name> {
  return (() => {
  const allOf = ((sets: ReadonlyArray<ReadonlySet<t0>>) => LibLists.foldl(LibSets.union)(LibSets.empty)(sets));
  return (() => {
  const tryType = ((tvars: ReadonlySet<Core.Name>) => ((typ: Core.Type) => LibSets.difference(freeVariablesInType(typ))(tvars)));
  return (() => {
  const getAll = ((vars: ReadonlySet<Core.Name>) => ((term: Core.Term) => (() => {
  const recurse = ((v1: Core.Term) => getAll(vars)(v1));
  return (() => {
  const dflt = allOf(LibLists.map(recurse)(Rewriting.subterms(term)));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const domt = LibMaybes.maybe(LibSets.empty)(((v1: Core.Type) => tryType(vars)(v1)))(((_x) => _x.domain)(l));
  return LibSets.union(domt)(recurse(((_x) => _x.body)(l)));
})())((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const forBinding = ((b: Core.Binding) => (() => {
  const newVars = LibMaybes.maybe(vars)(((ts: Core.TypeScheme) => LibSets.union(vars)(LibSets.fromList(((_x) => _x.variables)(ts)))))(((_x) => _x.type)(b));
  return LibSets.union(getAll(newVars)(((_x) => _x.term)(b)))(LibMaybes.maybe(LibSets.empty)(((ts: Core.TypeScheme) => tryType(newVars)(((_x) => _x.type)(ts))))(((_x) => _x.type)(b)));
})());
  return LibSets.union(allOf(LibLists.map(forBinding)(((_x) => _x.bindings)(l))))(recurse(((_x) => _x.body)(l)));
})())((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => LibSets.union(tryType(vars)(((_x) => _x.type)(tt)))(recurse(((_x) => _x.body)(tt))))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => LibSets.union(tryType(vars)(({ tag: "variable", value: ((_x) => _x.parameter)(tl) })))(recurse(((_x) => _x.body)(tl))))((_m as any).value);
    default: return dflt(_m);
  }
})();
})();
})()));
  return getAll(LibSets.empty)(term0);
})();
})();
})();
}

export function freeVariablesInTerm(term: Core.Term): ReadonlySet<Core.Name> {
  return (() => {
  const dfltVars = ((_: t0) => LibLists.foldl(((s: ReadonlySet<Core.Name>) => ((t: Core.Term) => LibSets.union(s)(freeVariablesInTerm(t)))))(LibSets.empty)(Rewriting.subterms(term)));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => LibSets.delete_(((_x) => _x.parameter)(l))(freeVariablesInTerm(((_x) => _x.body)(l))))((_m as any).value);
    case "let": return ((l: Core.Let) => LibSets.difference(dfltVars(undefined))(LibSets.fromList(LibLists.map(((_x) => _x.name))(((_x) => _x.bindings)(l)))))((_m as any).value);
    case "variable": return ((v: Core.Name) => LibSets.singleton(v))((_m as any).value);
    default: return dfltVars(undefined)(_m);
  }
})();
})();
}

export function freeVariablesInType(typ: Core.Type): ReadonlySet<Core.Name> {
  return (() => {
  const dfltVars = LibLists.foldl(((s: ReadonlySet<Core.Name>) => ((t: Core.Type) => LibSets.union(s)(freeVariablesInType(t)))))(LibSets.empty)(Rewriting.subtypes(typ));
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "forall": return ((lt: Core.ForallType) => LibSets.delete_(((_x) => _x.parameter)(lt))(freeVariablesInType(((_x) => _x.body)(lt))))((_m as any).value);
    case "variable": return ((v: Core.Name) => LibSets.singleton(v))((_m as any).value);
    default: return dfltVars(_m);
  }
})();
})();
}

export function freeVariablesInTypeOrdered(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const collectVars = ((boundVars: ReadonlySet<Core.Name>) => ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibSets.member(v)(boundVars))([])([v]))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => collectVars(LibSets.insert(((_x) => _x.parameter)(ft))(boundVars))(((_x) => _x.body)(ft)))((_m as any).value);
    default: return LibLists.concat(LibLists.map(((v1: Core.Type) => collectVars(boundVars)(v1)))(Rewriting.subtypes(t)))(_m);
  }
})()));
  return LibLists.nub(collectVars(LibSets.empty)(typ));
})();
}

export function freeVariablesInTypeScheme(ts: Core.TypeScheme): ReadonlySet<Core.Name> {
  return (() => {
  const vars = ((_x) => _x.variables)(ts);
  return (() => {
  const t = ((_x) => _x.type)(ts);
  return LibSets.difference(freeVariablesInType(t))(LibSets.fromList(vars));
})();
})();
}

export function freeVariablesInTypeSchemeSimple(ts: Core.TypeScheme): ReadonlySet<Core.Name> {
  return (() => {
  const vars = ((_x) => _x.variables)(ts);
  return (() => {
  const t = ((_x) => _x.type)(ts);
  return LibSets.difference(freeVariablesInTypeSimple(t))(LibSets.fromList(vars));
})();
})();
}

export function freeVariablesInTypeSimple(typ: Core.Type): ReadonlySet<Core.Name> {
  return (() => {
  const helper = ((types: ReadonlySet<Core.Name>) => ((typ2: Core.Type) => (() => {
  const _m = typ2;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibSets.insert(v)(types))((_m as any).value);
    default: return types(_m);
  }
})()));
  return Rewriting.foldOverType(({ tag: "pre" }))(helper)(LibSets.empty)(typ);
})();
}

export function isFreeVariableInTerm(v: Core.Name): ((x: Core.Term) => boolean) {
  return ((term: Core.Term) => LibLogic.not(LibSets.member(v)(freeVariablesInTerm(term))));
}

export function normalizeTypeVariablesInTerm(term: Core.Term): Core.Term {
  return (() => {
  const replaceName = ((subst: ReadonlyMap<t0, t0>) => ((v: t0) => LibMaybes.fromMaybe(v)(LibMaps.lookup(v)(subst))));
  return (() => {
  const substType = ((subst: ReadonlyMap<Core.Name, Core.Name>) => ((typ: Core.Type) => (() => {
  const rewrite = ((recurse: ((x: Core.Type) => Core.Type)) => ((typ2: Core.Type) => (() => {
  const _m = typ2;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => ({ tag: "variable", value: replaceName(subst)(v) }))((_m as any).value);
    default: return recurse(typ2)(_m);
  }
})()));
  return Rewriting.rewriteType(rewrite)(typ);
})()));
  return (() => {
  const rewriteWithSubst = ((state: readonly [readonly [ReadonlyMap<Core.Name, Core.Name>, ReadonlySet<Core.Name>], number]) => ((term0: Core.Term) => (() => {
  const sb = LibPairs.first(state);
  return (() => {
  const next = LibPairs.second(state);
  return (() => {
  const subst = LibPairs.first(sb);
  return (() => {
  const boundVars = LibPairs.second(sb);
  return (() => {
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term2: Core.Term) => (() => {
  const _m = term2;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const domain = ((_x) => _x.domain)(l);
  return ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: LibMaybes.map(((v1: Core.Type) => substType(subst)(v1)))(domain),
    body: rewriteWithSubst([[subst, boundVars], next])(((_x) => _x.body)(l))
  }) });
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings0 = ((_x) => _x.bindings)(lt);
  return (() => {
  const body0 = ((_x) => _x.body)(lt);
  return (() => {
  const step = ((acc: ReadonlyArray<Core.Binding>) => ((bs: ReadonlyArray<Core.Binding>) => LibLogic.ifElse(LibLists.null_(bs))(LibLists.reverse(acc))((() => {
  const b = LibLists.head(bs);
  return (() => {
  const tl = LibLists.tail(bs);
  return (() => {
  const noType = (() => {
  const newVal = rewriteWithSubst([[subst, boundVars], next])(((_x) => _x.term)(b));
  return (() => {
  const b1 = ({
    name: ((_x) => _x.name)(b),
    term: newVal,
    type: null
  });
  return step(LibLists.cons(b1)(acc))(tl);
})();
})();
  return (() => {
  const withType = ((ts: Core.TypeScheme) => (() => {
  const vars = ((_x) => _x.variables)(ts);
  return (() => {
  const typ = ((_x) => _x.type)(ts);
  return (() => {
  const k = LibLists.length(vars);
  return (() => {
  const gen = ((i: number) => ((rem: number) => ((acc2: ReadonlyArray<Core.Name>) => (() => {
  const ti = LibStrings.cat2("t")(LibLiterals.showInt32(LibMath.add(next)(i)));
  return LibLogic.ifElse(LibEquality.equal(rem)(0))(LibLists.reverse(acc2))(gen(LibMath.add(i)(1))(LibMath.sub(rem)(1))(LibLists.cons(ti)(acc2)));
})())));
  return (() => {
  const newVars = gen(0)(k)([]);
  return (() => {
  const newSubst = LibMaps.union(LibMaps.fromList(LibLists.zip(vars)(newVars)))(subst);
  return (() => {
  const newBound = LibSets.union(boundVars)(LibSets.fromList(newVars));
  return (() => {
  const newVal = rewriteWithSubst([[newSubst, newBound], LibMath.add(next)(k)])(((_x) => _x.term)(b));
  return (() => {
  const renameConstraintKeys = ((constraintMap: ReadonlyMap<Core.Name, t0>) => LibMaps.fromList(LibLists.map(((p: readonly [Core.Name, t0]) => (() => {
  const oldName = LibPairs.first(p);
  return (() => {
  const meta = LibPairs.second(p);
  return (() => {
  const newName = LibMaybes.fromMaybe(oldName)(LibMaps.lookup(oldName)(newSubst));
  return [newName, meta];
})();
})();
})()))(LibMaps.toList(constraintMap))));
  return (() => {
  const oldConstraints = ((_x) => _x.constraints)(ts);
  return (() => {
  const newConstraints = LibMaybes.map(renameConstraintKeys)(oldConstraints);
  return (() => {
  const b1 = ({
    name: ((_x) => _x.name)(b),
    term: newVal,
    type: ({
    variables: newVars,
    type: substType(newSubst)(typ),
    constraints: newConstraints
  })
  });
  return step(LibLists.cons(b1)(acc))(tl);
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
})());
  return LibMaybes.maybe(noType)(((ts: Core.TypeScheme) => withType(ts)))(((_x) => _x.type)(b));
})();
})();
})();
})())));
  return (() => {
  const bindings1 = step([])(bindings0);
  return ({ tag: "let", value: ({
    bindings: bindings1,
    body: rewriteWithSubst([[subst, boundVars], next])(body0)
  }) });
})();
})();
})();
})())((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: rewriteWithSubst([[subst, boundVars], next])(((_x) => _x.body)(tt)),
    type: substType(subst)(((_x) => _x.type)(tt))
  }) }))((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: replaceName(subst)(((_x) => _x.parameter)(ta)),
    body: rewriteWithSubst([[subst, boundVars], next])(((_x) => _x.body)(ta))
  }) }))((_m as any).value);
    default: return recurse(term2)(_m);
  }
})()));
  return Rewriting.rewriteTerm(rewrite)(term0);
})();
})();
})();
})();
})()));
  return rewriteWithSubst([[LibMaps.empty, LibSets.empty], 0])(term);
})();
})();
})();
}

export function replaceFreeTermVariable(vold: Core.Name): ((x: Core.Term) => ((x: Core.Term) => Core.Term)) {
  return ((tnew: Core.Term) => ((term: Core.Term) => (() => {
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return LibLogic.ifElse(LibEquality.equal(v)(vold))(t)(recurse(t));
})())((_m as any).value);
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibEquality.equal(v)(vold))(tnew)(({ tag: "variable", value: v })))((_m as any).value);
    default: return recurse(t)(_m);
  }
})()));
  return Rewriting.rewriteTerm(rewrite)(term);
})()));
}

export function replaceFreeTypeVariable(v: Core.Name): ((x: Core.Type) => ((x: Core.Type) => Core.Type)) {
  return ((rep: Core.Type) => ((typ: Core.Type) => (() => {
  const mapExpr = ((recurse: ((x: Core.Type) => Core.Type)) => ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => LibLogic.ifElse(LibEquality.equal(v)(((_x) => _x.parameter)(ft)))(t)(({ tag: "forall", value: ({
    parameter: ((_x) => _x.parameter)(ft),
    body: recurse(((_x) => _x.body)(ft))
  }) })))((_m as any).value);
    case "variable": return ((v_: Core.Name) => LibLogic.ifElse(LibEquality.equal(v)(v_))(rep)(t))((_m as any).value);
    default: return recurse(t)(_m);
  }
})()));
  return Rewriting.rewriteType(mapExpr)(typ);
})()));
}

export function substituteTypeVariables(subst: ReadonlyMap<Core.Name, Core.Name>): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const replace = ((recurse: ((x: Core.Type) => Core.Type)) => ((typ2: Core.Type) => (() => {
  const _m = typ2;
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => ({ tag: "variable", value: LibMaybes.fromMaybe(n)(LibMaps.lookup(n)(subst)) }))((_m as any).value);
    default: return recurse(typ2)(_m);
  }
})()));
  return Rewriting.rewriteType(replace)(typ);
})());
}

export function substituteTypeVariablesInTerm(subst: ReadonlyMap<Core.Name, Core.Name>): ((x: Core.Term) => Core.Term) {
  return ((term: Core.Term) => (() => {
  const st = ((v1: Core.Type) => substituteTypeVariables(subst)(v1));
  return (() => {
  const stOpt = ((mt: Core.Type | null) => LibMaybes.map(st)(mt));
  return (() => {
  const stScheme = ((ts: Core.TypeScheme) => ({
    variables: ((_x) => _x.variables)(ts),
    type: st(((_x) => _x.type)(ts)),
    constraints: ((_x) => _x.constraints)(ts)
  }));
  return (() => {
  const stSchemeOpt = ((mts: Core.TypeScheme | null) => LibMaybes.map(stScheme)(mts));
  return (() => {
  const replace = ((recurse: ((x: Core.Term) => Core.Term)) => ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: stOpt(((_x) => _x.domain)(l)),
    body: recurse(((_x) => _x.body)(l))
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const mapBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: recurse(((_x) => _x.term)(b)),
    type: stSchemeOpt(((_x) => _x.type)(b))
  }));
  return ({ tag: "let", value: ({
    bindings: LibLists.map(mapBinding)(((_x) => _x.bindings)(lt)),
    body: recurse(((_x) => _x.body)(lt))
  }) });
})())((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: recurse(((_x) => _x.body)(tt)),
    type: st(((_x) => _x.type)(tt))
  }) }))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: LibMaybes.fromMaybe(((_x) => _x.parameter)(tl))(LibMaps.lookup(((_x) => _x.parameter)(tl))(subst)),
    body: recurse(((_x) => _x.body)(tl))
  }) }))((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: recurse(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    default: return recurse(t)(_m);
  }
})()));
  return Rewriting.rewriteTerm(replace)(term);
})();
})();
})();
})();
})());
}

export function substituteVariable(from: Core.Name): ((x: Core.Name) => ((x: Core.Term) => Core.Term)) {
  return ((to: Core.Name) => ((term: Core.Term) => (() => {
  const replace = ((recurse: ((x: Core.Term) => Core.Term)) => ((term2: Core.Term) => (() => {
  const _m = term2;
  switch (_m.tag) {
    case "variable": return ((x: Core.Name) => ({ tag: "variable", value: LibLogic.ifElse(LibEquality.equal(x)(from))(to)(x) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.parameter)(l))(from))(term2)(recurse(term2)))((_m as any).value);
    default: return recurse(term2)(_m);
  }
})()));
  return Rewriting.rewriteTerm(replace)(term);
})()));
}

export function substituteVariables(subst: ReadonlyMap<Core.Name, Core.Name>): ((x: Core.Term) => Core.Term) {
  return ((term: Core.Term) => (() => {
  const replace = ((recurse: ((x: Core.Term) => Core.Term)) => ((term2: Core.Term) => (() => {
  const _m = term2;
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => ({ tag: "variable", value: LibMaybes.fromMaybe(n)(LibMaps.lookup(n)(subst)) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => LibMaybes.maybe(recurse(term2))(((_: Core.Name) => term2))(LibMaps.lookup(((_x) => _x.parameter)(l))(subst)))((_m as any).value);
    default: return recurse(term2)(_m);
  }
})()));
  return Rewriting.rewriteTerm(replace)(term);
})());
}

export function unshadowVariables(term0: Core.Term): Core.Term {
  return (() => {
  const freshName = ((base: Core.Name) => ((i: number) => ((m: ReadonlyMap<Core.Name, t0>) => (() => {
  const candidate = LibStrings.cat2(((_x) => _x)(base))(LibLiterals.showInt32(i));
  return LibLogic.ifElse(LibMaps.member(candidate)(m))(freshName(base)(LibMath.add(i)(1))(m))(candidate);
})())));
  return (() => {
  const f = ((recurse: ((x: ReadonlyMap<Core.Name, Core.Name>) => ((x: Core.Term) => Core.Term))) => ((m: ReadonlyMap<Core.Name, Core.Name>) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return (() => {
  const domain = ((_x) => _x.domain)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return LibLogic.ifElse(LibMaps.member(v)(m))((() => {
  const v2 = freshName(v)(2)(m);
  return (() => {
  const m2 = LibMaps.insert(v)(v2)(LibMaps.insert(v2)(v2)(m));
  return ({ tag: "lambda", value: ({
    parameter: v2,
    domain: domain,
    body: f(recurse)(m2)(body)
  }) });
})();
})())(({ tag: "lambda", value: ({
    parameter: v,
    domain: domain,
    body: f(recurse)(LibMaps.insert(v)(v)(m))(body)
  }) }));
})();
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const m2 = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Name>) => ((b: Core.Binding) => (() => {
  const bname = ((_x) => _x.name)(b);
  return LibLogic.ifElse(LibMaps.member(bname)(acc))(acc)(LibMaps.insert(bname)(bname)(acc));
})())))(m)(((_x) => _x.bindings)(lt));
  return recurse(m2)(term);
})())((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "variable", value: LibMaybes.maybe(v)(((renamed: Core.Name) => renamed))(LibMaps.lookup(v)(m)) }))((_m as any).value);
    default: return recurse(m)(term)(_m);
  }
})())));
  return Rewriting.rewriteTermWithContext(f)(LibMaps.empty)(term0);
})();
})();
}
