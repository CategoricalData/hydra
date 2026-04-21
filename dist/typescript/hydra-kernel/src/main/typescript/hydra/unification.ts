// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for type unification.
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
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as ShowCore from "./show/core.js";
import * as Strip from "./strip.js";
import * as Substitution from "./substitution.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function joinTypes<t0>(cx: t0): ((x: Core.Type) => ((x: Core.Type) => ((x: string) => Errors.UnificationError | ReadonlyArray<Typing.TypeConstraint>))) {
  return ((left: Core.Type) => ((right: Core.Type) => ((comment: string) => (() => {
  const sleft = Strip.deannotateType(left);
  return (() => {
  const sright = Strip.deannotateType(right);
  return (() => {
  const joinOne = ((l: Core.Type) => ((r: Core.Type) => ({
    left: l,
    right: r,
    comment: LibStrings.cat2("join types; ")(comment)
  })));
  return (() => {
  const cannotUnify = ({ tag: "left", value: ({
    leftType: sleft,
    rightType: sright,
    message: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("cannot unify ")(ShowCore.type(sleft)))(" with "))(ShowCore.type(sright))
  }) });
  return (() => {
  const assertEqual = LibLogic.ifElse(LibEquality.equal(sleft)(sright))(({ tag: "right", value: [] }))(cannotUnify);
  return (() => {
  const joinList = ((lefts: ReadonlyArray<Core.Type>) => ((rights: ReadonlyArray<Core.Type>) => LibLogic.ifElse(LibEquality.equal(LibLists.length(lefts))(LibLists.length(rights)))(({ tag: "right", value: LibLists.zipWith(joinOne)(lefts)(rights) }))(cannotUnify)));
  return (() => {
  const joinRowTypes = ((left2: ReadonlyArray<Core.FieldType>) => ((right2: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(LibLogic.and(LibEquality.equal(LibLists.length(LibLists.map(((_x) => _x.name))(left2)))(LibLists.length(LibLists.map(((_x) => _x.name))(right2))))(LibLists.foldl(LibLogic.and)(true)(LibLists.zipWith(((left3: Core.Name) => ((right3: Core.Name) => LibEquality.equal(((_x) => _x)(left3))(((_x) => _x)(right3)))))(LibLists.map(((_x) => _x.name))(left2))(LibLists.map(((_x) => _x.name))(right2)))))(joinList(LibLists.map(((_x) => _x.type))(left2))(LibLists.map(((_x) => _x.type))(right2)))(cannotUnify)));
  return (() => {
  const _m = sleft;
  switch (_m.tag) {
    case "application": return ((l: Core.ApplicationType) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "application": return ((r: Core.ApplicationType) => ({ tag: "right", value: [joinOne(((_x) => _x.function)(l))(((_x) => _x.function)(r)), joinOne(((_x) => _x.argument)(l))(((_x) => _x.argument)(r))] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "either": return ((l: Core.EitherType) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "either": return ((r: Core.EitherType) => ({ tag: "right", value: [joinOne(((_x) => _x.left)(l))(((_x) => _x.left)(r)), joinOne(((_x) => _x.right)(l))(((_x) => _x.right)(r))] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "function": return ((l: Core.FunctionType) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "function": return ((r: Core.FunctionType) => ({ tag: "right", value: [joinOne(((_x) => _x.domain)(l))(((_x) => _x.domain)(r)), joinOne(((_x) => _x.codomain)(l))(((_x) => _x.codomain)(r))] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "list": return ((l: Core.Type) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "list": return ((r: Core.Type) => ({ tag: "right", value: [joinOne(l)(r)] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => assertEqual)((_m as any).value);
    case "map": return ((l: Core.MapType) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "map": return ((r: Core.MapType) => ({ tag: "right", value: [joinOne(((_x) => _x.keys)(l))(((_x) => _x.keys)(r)), joinOne(((_x) => _x.values)(l))(((_x) => _x.values)(r))] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "maybe": return ((l: Core.Type) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "maybe": return ((r: Core.Type) => ({ tag: "right", value: [joinOne(l)(r)] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "pair": return ((l: Core.PairType) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "pair": return ((r: Core.PairType) => ({ tag: "right", value: [joinOne(((_x) => _x.first)(l))(((_x) => _x.first)(r)), joinOne(((_x) => _x.second)(l))(((_x) => _x.second)(r))] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "record": return ((l: ReadonlyArray<Core.FieldType>) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "record": return ((r: ReadonlyArray<Core.FieldType>) => joinRowTypes(l)(r))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "set": return ((l: Core.Type) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "set": return ((r: Core.Type) => ({ tag: "right", value: [joinOne(l)(r)] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "union": return ((l: ReadonlyArray<Core.FieldType>) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "union": return ((r: ReadonlyArray<Core.FieldType>) => joinRowTypes(l)(r))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "unit": return ((_: void) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "unit": return ((_2: void) => ({ tag: "right", value: [] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "void": return ((_: void) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "void": return ((_2: void) => ({ tag: "right", value: [] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    case "wrap": return ((l: Core.Type) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "wrap": return ((r: Core.Type) => ({ tag: "right", value: [joinOne(l)(r)] }))((_m as any).value);
    default: return cannotUnify(_m);
  }
})())((_m as any).value);
    default: return cannotUnify(_m);
  }
})();
})();
})();
})();
})();
})();
})();
})())));
}

export function unifyTypeConstraints<t0, t1>(cx: t0): ((x: ReadonlyMap<Core.Name, t1>) => ((x: ReadonlyArray<Typing.TypeConstraint>) => Errors.UnificationError | Typing.TypeSubst)) {
  return ((schemaTypes: ReadonlyMap<Core.Name, t1>) => ((constraints: ReadonlyArray<Typing.TypeConstraint>) => (() => {
  const withConstraint = ((c: Typing.TypeConstraint) => ((rest: ReadonlyArray<Typing.TypeConstraint>) => (() => {
  const sleft = Strip.deannotateType(((_x) => _x.left)(c));
  return (() => {
  const sright = Strip.deannotateType(((_x) => _x.right)(c));
  return (() => {
  const comment = ((_x) => _x.comment)(c);
  return (() => {
  const bind = ((v: Core.Name) => ((t: Core.Type) => (() => {
  const subst = Substitution.singletonTypeSubst(v)(t);
  return (() => {
  const withResult = ((s: Typing.TypeSubst) => Substitution.composeTypeSubst(subst)(s));
  return LibEithers.map(withResult)(unifyTypeConstraints(cx)(schemaTypes)(Substitution.substituteInConstraints(subst)(rest)));
})();
})()));
  return (() => {
  const tryBinding = ((v: Core.Name) => ((t: Core.Type) => LibLogic.ifElse(variableOccursInType(v)(t))(({ tag: "left", value: ({
    leftType: sleft,
    rightType: sright,
    message: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("Variable ")(((_x) => _x)(v)))(" appears free in type "))(ShowCore.type(t)))(" ("))(comment))(")")
  }) }))(bind(v)(t))));
  return (() => {
  const noVars = (() => {
  const withConstraints = ((constraints2: ReadonlyArray<Typing.TypeConstraint>) => unifyTypeConstraints(cx)(schemaTypes)(LibLists.concat2(constraints2)(rest)));
  return LibEithers.bind(joinTypes(cx)(sleft)(sright)(comment))(withConstraints);
})();
  return (() => {
  const dflt = (() => {
  const _m = sright;
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => tryBinding(name)(sleft))((_m as any).value);
    default: return noVars(_m);
  }
})();
  return (() => {
  const _m = sleft;
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => (() => {
  const _m = sright;
  switch (_m.tag) {
    case "variable": return ((name2: Core.Name) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(name))(((_x) => _x)(name2)))(unifyTypeConstraints(cx)(schemaTypes)(rest))(LibLogic.ifElse(LibMaybes.isJust(LibMaps.lookup(name)(schemaTypes)))(LibLogic.ifElse(LibMaybes.isJust(LibMaps.lookup(name2)(schemaTypes)))(({ tag: "left", value: ({
    leftType: sleft,
    rightType: sright,
    message: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("Attempted to unify schema names ")(((_x) => _x)(name)))(" and "))(((_x) => _x)(name2)))(" ("))(comment))(")")
  }) }))(bind(name2)(sleft)))(bind(name)(sright))))((_m as any).value);
    default: return tryBinding(name)(sright)(_m);
  }
})())((_m as any).value);
    default: return dflt(_m);
  }
})();
})();
})();
})();
})();
})();
})();
})()));
  return LibLogic.ifElse(LibLists.null_(constraints))(({ tag: "right", value: Substitution.idTypeSubst }))(withConstraint(LibLists.head(constraints))(LibLists.tail(constraints)));
})()));
}

export function unifyTypeLists<t0, t1>(cx: t0): ((x: ReadonlyMap<Core.Name, t1>) => ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<Core.Type>) => ((x: string) => Errors.UnificationError | Typing.TypeSubst)))) {
  return ((schemaTypes: ReadonlyMap<Core.Name, t1>) => ((l: ReadonlyArray<Core.Type>) => ((r: ReadonlyArray<Core.Type>) => ((comment: string) => (() => {
  const toConstraint = ((l2: Core.Type) => ((r2: Core.Type) => ({
    left: l2,
    right: r2,
    comment: comment
  })));
  return unifyTypeConstraints(cx)(schemaTypes)(LibLists.zipWith(toConstraint)(l)(r));
})()))));
}

export function unifyTypes<t0, t1>(cx: t0): ((x: ReadonlyMap<Core.Name, t1>) => ((x: Core.Type) => ((x: Core.Type) => ((x: string) => Errors.UnificationError | Typing.TypeSubst)))) {
  return ((schemaTypes: ReadonlyMap<Core.Name, t1>) => ((l: Core.Type) => ((r: Core.Type) => ((comment: string) => unifyTypeConstraints(cx)(schemaTypes)([({
    left: l,
    right: r,
    comment: comment
  })])))));
}

export function variableOccursInType(var_: Core.Name): ((x: Core.Type) => boolean) {
  return ((typ0: Core.Type) => (() => {
  const tryType = ((b: boolean) => ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.or(b)(LibEquality.equal(((_x) => _x)(v))(((_x) => _x)(var_))))((_m as any).value);
    default: return b(_m);
  }
})()));
  return Rewriting.foldOverType(({ tag: "pre" }))(tryType)(false)(typ0);
})());
}
