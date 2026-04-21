// Note: this is an automatically generated file. Do not edit.

/**
 * Utility functions for constructing Scala AST nodes
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMath from "../lib/math.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ScalaLanguage from "./language.js";
import * as ScalaSyntax from "./syntax.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function nameOfType<t0>(cx: t0): ((x: Core.Type) => Core.Name | null) {
  return ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => name)((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => nameOfType(cx)(((_x) => _x.body)(ft)))((_m as any).value);
    default: return null(_m);
  }
})());
}

export function qualifyUnionFieldName(dlft: string): ((x: Core.Name | null) => ((x: Core.Name) => string)) {
  return ((sname: Core.Name | null) => ((fname: Core.Name) => LibStrings.cat2(LibMaybes.maybe(dlft)(((n: Core.Name) => LibStrings.cat2(scalaTypeName(true)(n))(".")))(sname))(scalaEscapeName(((_x) => _x)(fname)))));
}

export function sapply(fun: ScalaSyntax.Data): ((x: ReadonlyArray<ScalaSyntax.Data>) => ScalaSyntax.Data) {
  return ((args: ReadonlyArray<ScalaSyntax.Data>) => ({ tag: "apply", value: ({
    fun: fun,
    args: args
  }) }));
}

export function sapplyTypes(fun: ScalaSyntax.Data): ((x: ReadonlyArray<ScalaSyntax.Type>) => ScalaSyntax.Data) {
  return ((typeArgs: ReadonlyArray<ScalaSyntax.Type>) => (() => {
  const typeToStr = ((t: ScalaSyntax.Type) => typeToString(t));
  const typeStrings = LibLists.map(typeToStr)(typeArgs);
  const typeArgStr = LibStrings.cat(["[", LibStrings.intercalate(", ")(typeStrings), "]"]);
  return (() => {
  const _m = fun;
  switch (_m.tag) {
    case "ref": return ((ref: ScalaSyntax.Data_Ref) => (() => {
  const _m = ref;
  switch (_m.tag) {
    case "name": return ((dn: ScalaSyntax.Data_Name) => (() => {
  const nameStr = ((_x) => _x.value)(dn);
  const rawName = ((_x) => _x)(nameStr);
  return sname(LibStrings.cat2(rawName)(typeArgStr));
})())((_m as any).value);
    default: return fun(_m);
  }
})())((_m as any).value);
    default: return fun(_m);
  }
})();
})());
}

export function sassign(lhs: ScalaSyntax.Data): ((x: ScalaSyntax.Data) => ScalaSyntax.Data) {
  return ((rhs: ScalaSyntax.Data) => ({ tag: "assign", value: ({
    lhs: lhs,
    rhs: rhs
  }) }));
}

export function scalaEscapeName(s: string): string {
  return (() => {
  const sanitized = LibStrings.fromList(LibLists.map(((c: number) => LibLogic.ifElse(LibEquality.equal(c)(39))(95)(c)))(LibStrings.toList(s)));
  const sanitized2 = LibLogic.ifElse(LibEquality.equal(sanitized)("_"))("_x")(sanitized);
  const sanitized3 = LibLogic.ifElse(LibEquality.equal(sanitized2)("toString"))("toString_")(sanitized2);
  const needsBackticks = LibLogic.or(LibSets.member(sanitized3)(scalaReservedWords))(LibLogic.and(LibEquality.gt(LibStrings.length(sanitized3))(0))(LibEquality.equal(LibStrings.charAt(LibMath.sub(LibStrings.length(sanitized3))(1))(sanitized3))(95)));
  return LibLogic.ifElse(needsBackticks)(LibStrings.cat(["`", sanitized3, "`"]))(sanitized3);
})();
}

export const scalaReservedWords: ReadonlySet<string> = ScalaLanguage.scalaReservedWords;

export function scalaTypeName(qualify: boolean): ((x: Core.Name) => string) {
  return ((name: Core.Name) => LibLogic.ifElse(LibLogic.or(qualify)(LibSets.member(Names.localNameOf(name))(scalaReservedWords)))(((_x) => _x)(name))(Names.localNameOf(name)));
}

export function slambda(v: string): ((x: ScalaSyntax.Data) => ((x: ScalaSyntax.Type | null) => ScalaSyntax.Data)) {
  return ((body: ScalaSyntax.Data) => ((sdom: ScalaSyntax.Type | null) => ({ tag: "functionData", value: ({ tag: "function", value: ({
    params: [({
    mods: [],
    name: ({ tag: "value", value: v }),
    decltpe: sdom,
    default: null
  })],
    body: body
  }) }) })));
}

export function sname(s: string): ScalaSyntax.Data {
  return ({ tag: "ref", value: ({ tag: "name", value: ({
    value: s
  }) }) });
}

export function sprim(name: Core.Name): ScalaSyntax.Data {
  return (() => {
  const qname = Names.qualifyName(name);
  const prefix = ((_x) => _x)(LibMaybes.fromJust(((_x) => _x.namespace)(qname)));
  const local = scalaEscapeName(((_x) => _x.local)(qname));
  return sname(LibStrings.cat2(LibStrings.cat2(prefix)("."))(local));
})();
}

export function stapply(t: ScalaSyntax.Type): ((x: ReadonlyArray<ScalaSyntax.Type>) => ScalaSyntax.Type) {
  return ((args: ReadonlyArray<ScalaSyntax.Type>) => ({ tag: "apply", value: ({
    tpe: t,
    args: args
  }) }));
}

export function stapply1(t1: ScalaSyntax.Type): ((x: ScalaSyntax.Type) => ScalaSyntax.Type) {
  return ((t2: ScalaSyntax.Type) => stapply(t1)([t2]));
}

export function stapply2(t1: ScalaSyntax.Type): ((x: ScalaSyntax.Type) => ((x: ScalaSyntax.Type) => ScalaSyntax.Type)) {
  return ((t2: ScalaSyntax.Type) => ((t3: ScalaSyntax.Type) => stapply(t1)([t2, t3])));
}

export function stparam(name: Core.Name): ScalaSyntax.Type_Param {
  return (() => {
  const v = Formatting.capitalize(((_x) => _x)(name));
  return ({
    mods: [],
    name: ({ tag: "value", value: v }),
    tparams: [],
    tbounds: [],
    vbounds: [],
    cbounds: []
  });
})();
}

export function stref(s: string): ScalaSyntax.Type {
  return ({ tag: "ref", value: ({ tag: "name", value: ({
    value: s
  }) }) });
}

export function svar(name: Core.Name): ScalaSyntax.Pat {
  return (() => {
  const v = ((_x) => _x)(name);
  return ({ tag: "var", value: ({
    name: ({
    value: v
  })
  }) });
})();
}

export function typeToString(t: ScalaSyntax.Type): string {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "ref": return ((tr: ScalaSyntax.Type_Ref) => (() => {
  const _m = tr;
  switch (_m.tag) {
    case "name": return ((tn: ScalaSyntax.Type_Name) => ((_x) => _x.value)(tn))((_m as any).value);
    default: return "Any"(_m);
  }
})())((_m as any).value);
    case "var": return ((tv: ScalaSyntax.Type_Var) => ((_x) => _x.value)(((_x) => _x.name)(tv)))((_m as any).value);
    case "functionType": return ((ft: ScalaSyntax.Type_FunctionType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "function": return ((fn: ScalaSyntax.Type_Function) => (() => {
  const params = LibLists.map(typeToString)(((_x) => _x.params)(fn));
  const res = typeToString(((_x) => _x.res)(fn));
  return LibStrings.cat(["(", LibStrings.intercalate(", ")(params), ") => ", res]);
})())((_m as any).value);
    default: return "Any"(_m);
  }
})())((_m as any).value);
    case "apply": return ((ta: ScalaSyntax.Type_Apply) => (() => {
  const base = typeToString(((_x) => _x.tpe)(ta));
  const argStrs = LibLists.map(typeToString)(((_x) => _x.args)(ta));
  return LibStrings.cat([base, "[", LibStrings.intercalate(", ")(argStrs), "]"]);
})())((_m as any).value);
    default: return "Any"(_m);
  }
})();
}
