// Note: this is an automatically generated file. Do not edit.

/**
 * Graph context extension and type scheme conversion
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
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibSets from "./lib/sets.js";
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

export function extendGraphForLambda(g: Graph.Graph): ((x: Core.Lambda) => Graph.Graph) {
  return ((lam: Core.Lambda) => (() => {
  const var_ = ((_x) => _x.parameter)(lam);
  return ({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: LibMaybes.maybe(((_x) => _x.boundTypes)(g))(((dom: Core.Type) => LibMaps.insert(var_)(fTypeToTypeScheme(dom))(((_x) => _x.boundTypes)(g))))(((_x) => _x.domain)(lam)),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: LibSets.insert(var_)(((_x) => _x.lambdaVariables)(g)),
    metadata: LibMaps.delete_(var_)(((_x) => _x.metadata)(g)),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(g),
    typeVariables: ((_x) => _x.typeVariables)(g)
  });
})());
}

export function extendGraphForLet(forBinding: ((x: Graph.Graph) => ((x: Core.Binding) => Core.Term | null))): ((x: Graph.Graph) => ((x: Core.Let) => Graph.Graph)) {
  return ((g: Graph.Graph) => ((letrec: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(letrec);
  return (() => {
  const g2 = extendGraphWithBindings(bindings)(g);
  return ({
    boundTerms: LibMaps.union(LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), ((_x) => _x.term)(b)]))(bindings)))(((_x) => _x.boundTerms)(g)),
    boundTypes: LibMaps.union(LibMaps.fromList(LibMaybes.cat(LibLists.map(((b: Core.Binding) => LibMaybes.map(((ts: Core.TypeScheme) => [((_x) => _x.name)(b), ts]))(((_x) => _x.type)(b))))(bindings))))(((_x) => _x.boundTypes)(g)),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: LibLists.foldl(((s: ReadonlySet<Core.Name>) => ((b: Core.Binding) => LibSets.delete_(((_x) => _x.name)(b))(s))))(((_x) => _x.lambdaVariables)(g))(bindings),
    metadata: ((_x) => _x.metadata)(LibLists.foldl(((gAcc: Graph.Graph) => ((b: Core.Binding) => (() => {
  const m = ((_x) => _x.metadata)(gAcc);
  return (() => {
  const newMeta = LibMaybes.maybe(LibMaps.delete_(((_x) => _x.name)(b))(m))(((t: Core.Term) => LibMaps.insert(((_x) => _x.name)(b))(t)(m)))(forBinding(gAcc)(b));
  return ({
    boundTerms: ((_x) => _x.boundTerms)(gAcc),
    boundTypes: ((_x) => _x.boundTypes)(gAcc),
    classConstraints: ((_x) => _x.classConstraints)(gAcc),
    lambdaVariables: ((_x) => _x.lambdaVariables)(gAcc),
    metadata: newMeta,
    primitives: ((_x) => _x.primitives)(gAcc),
    schemaTypes: ((_x) => _x.schemaTypes)(gAcc),
    typeVariables: ((_x) => _x.typeVariables)(gAcc)
  });
})();
})())))(g2)(bindings)),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(g),
    typeVariables: ((_x) => _x.typeVariables)(g)
  });
})();
})()));
}

export function extendGraphForTypeLambda(g: Graph.Graph): ((x: Core.TypeLambda) => Graph.Graph) {
  return ((tlam: Core.TypeLambda) => (() => {
  const name = ((_x) => _x.parameter)(tlam);
  return ({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: ((_x) => _x.boundTypes)(g),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(g),
    typeVariables: LibSets.insert(name)(((_x) => _x.typeVariables)(g))
  });
})());
}

export function extendGraphWithBindings(bindings: ReadonlyArray<Core.Binding>): ((x: Graph.Graph) => Graph.Graph) {
  return ((g: Graph.Graph) => (() => {
  const newTerms = LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), ((_x) => _x.term)(b)]))(bindings));
  return (() => {
  const newTypes = LibMaps.fromList(LibMaybes.cat(LibLists.map(((b: Core.Binding) => LibMaybes.map(((ts: Core.TypeScheme) => [((_x) => _x.name)(b), ts]))(((_x) => _x.type)(b))))(bindings)));
  return ({
    boundTerms: LibMaps.union(newTerms)(((_x) => _x.boundTerms)(g)),
    boundTypes: LibMaps.union(newTypes)(((_x) => _x.boundTypes)(g)),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(g),
    typeVariables: ((_x) => _x.typeVariables)(g)
  });
})();
})());
}

export function fTypeToTypeScheme(typ: Core.Type): Core.TypeScheme {
  return (() => {
  const stripAnnotations = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => stripAnnotations(((_x) => _x.body)(at)))((_m as any).value);
    default: return t(_m);
  }
})());
  return (() => {
  const gatherForall = ((vars: ReadonlyArray<Core.Name>) => ((typ2: Core.Type) => (() => {
  const _m = stripAnnotations(typ2);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => gatherForall(LibLists.cons(((_x) => _x.parameter)(ft))(vars))(((_x) => _x.body)(ft)))((_m as any).value);
    default: return ({
    variables: LibLists.reverse(vars),
    type: typ2,
    constraints: null
  })(_m);
  }
})()));
  return gatherForall([])(typ);
})();
})();
}

export function typeSchemeToFType(ts: Core.TypeScheme): Core.Type {
  return (() => {
  const vars = ((_x) => _x.variables)(ts);
  return (() => {
  const body = ((_x) => _x.type)(ts);
  return LibLists.foldl(((t: Core.Type) => ((v: Core.Name) => ({ tag: "forall", value: ({
    parameter: v,
    body: t
  }) }))))(body)(LibLists.reverse(vars));
})();
})();
}
