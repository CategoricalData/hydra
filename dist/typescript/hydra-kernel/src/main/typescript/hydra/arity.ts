// Note: this is an automatically generated file. Do not edit.

/**
 * Functions dealing with arguments and arity.
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
import * as LibMath from "./lib/math.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function primitiveArity(arg_: Graph.Primitive): number {
  return typeArity(((_x) => _x.type)(((_x) => _x.type)(arg_)));
}

export function termArity(v1: Core.Term): number {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "application": return ((arg_: Core.Application) => LibMath.sub(termArity(((_x) => _x.function)(arg_)))(1))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => 1)((_m as any).value);
    case "lambda": return ((arg_: Core.Lambda) => LibMath.add(1)(termArity(((_x) => _x.body)(arg_))))((_m as any).value);
    case "project": return ((_: Core.Projection) => 1)((_m as any).value);
    case "unwrap": return ((_: Core.Name) => 1)((_m as any).value);
    default: return 0(_m);
  }
})();
}

export function typeArity(v1: Core.Type): number {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((arg_: Core.AnnotatedType) => typeArity(((_x) => _x.body)(arg_)))((_m as any).value);
    case "application": return ((arg_: Core.ApplicationType) => typeArity(((_x) => _x.function)(arg_)))((_m as any).value);
    case "forall": return ((arg_: Core.ForallType) => typeArity(((_x) => _x.body)(arg_)))((_m as any).value);
    case "function": return ((f: Core.FunctionType) => LibMath.add(1)(typeArity(((_x) => _x.codomain)(f))))((_m as any).value);
    default: return 0(_m);
  }
})();
}

export function typeSchemeArity(arg_: Core.TypeScheme): number {
  return typeArity(((_x) => _x.type)(arg_));
}

export function uncurryType(t: Core.Type): ReadonlyArray<Core.Type> {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((arg_: Core.AnnotatedType) => uncurryType(((_x) => _x.body)(arg_)))((_m as any).value);
    case "application": return ((arg_: Core.ApplicationType) => uncurryType(((_x) => _x.function)(arg_)))((_m as any).value);
    case "forall": return ((arg_: Core.ForallType) => uncurryType(((_x) => _x.body)(arg_)))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => LibLists.cons(((_x) => _x.domain)(ft))(uncurryType(((_x) => _x.codomain)(ft))))((_m as any).value);
    default: return [t](_m);
  }
})();
}
