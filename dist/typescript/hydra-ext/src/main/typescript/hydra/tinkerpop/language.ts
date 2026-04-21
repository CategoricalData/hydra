// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints based on TinkerPop Graph.Features
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
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibSets from "../lib/sets.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as TinkerpopFeatures from "./features.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function tinkerpopLanguage<t0>(name: Coders.LanguageName): ((x: TinkerpopFeatures.Features) => ((x: TinkerpopFeatures.ExtraFeatures<t0>) => Coders.Language)) {
  return ((features: TinkerpopFeatures.Features) => ((extras: TinkerpopFeatures.ExtraFeatures<t0>) => (() => {
  const vpFeatures = ((_x) => _x.dataTypeFeatures)(((_x) => _x.properties)(((_x) => _x.vertex)(features)));
  const cond = ((v: t1) => ((b: boolean) => LibLogic.ifElse(b)(LibMaybes.pure(v))(null)));
  const supportsLists = LibLogic.or(((_x) => _x.supportsBooleanArrayValues)(vpFeatures))(LibLogic.or(((_x) => _x.supportsByteArrayValues)(vpFeatures))(LibLogic.or(((_x) => _x.supportsDoubleArrayValues)(vpFeatures))(LibLogic.or(((_x) => _x.supportsFloatArrayValues)(vpFeatures))(LibLogic.or(((_x) => _x.supportsIntegerArrayValues)(vpFeatures))(LibLogic.or(((_x) => _x.supportsLongArrayValues)(vpFeatures))(((_x) => _x.supportsStringArrayValues)(vpFeatures)))))));
  const supportsLiterals = true;
  const supportsMaps = ((_x) => _x.supportsMapValues)(vpFeatures);
  const eliminationVariants = LibSets.empty;
  const literalVariants = LibSets.fromList(LibMaybes.cat([cond(({ tag: "binary" }))(((_x) => _x.supportsByteArrayValues)(vpFeatures)), cond(({ tag: "boolean" }))(((_x) => _x.supportsBooleanValues)(vpFeatures)), cond(({ tag: "float" }))(LibLogic.or(((_x) => _x.supportsFloatValues)(vpFeatures))(((_x) => _x.supportsDoubleValues)(vpFeatures))), cond(({ tag: "integer" }))(LibLogic.or(((_x) => _x.supportsIntegerValues)(vpFeatures))(((_x) => _x.supportsLongValues)(vpFeatures))), cond(({ tag: "string" }))(((_x) => _x.supportsStringValues)(vpFeatures))]));
  const floatTypes = LibSets.fromList(LibMaybes.cat([cond(({ tag: "float32" }))(((_x) => _x.supportsFloatValues)(vpFeatures)), cond(({ tag: "float64" }))(((_x) => _x.supportsDoubleValues)(vpFeatures))]));
  const functionVariants = LibSets.empty;
  const integerTypes = LibSets.fromList(LibMaybes.cat([cond(({ tag: "int32" }))(((_x) => _x.supportsIntegerValues)(vpFeatures)), cond(({ tag: "int64" }))(((_x) => _x.supportsLongValues)(vpFeatures))]));
  const termVariants = LibSets.fromList(LibMaybes.cat([cond(({ tag: "list" }))(supportsLists), cond(({ tag: "literal" }))(supportsLiterals), cond(({ tag: "map" }))(supportsMaps), LibMaybes.pure(({ tag: "maybe" }))]));
  const typeVariants = LibSets.fromList(LibMaybes.cat([cond(({ tag: "list" }))(supportsLists), cond(({ tag: "literal" }))(supportsLiterals), cond(({ tag: "map" }))(supportsMaps), LibMaybes.pure(({ tag: "maybe" })), LibMaybes.pure(({ tag: "wrap" }))]));
  const typePredicate = ((typ: Core.Type) => (() => {
  const dt = Strip.deannotateType(typ);
  return (() => {
  const _m = dt;
  switch (_m.tag) {
    case "list": return ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "boolean": return ((_: void) => ((_x) => _x.supportsBooleanArrayValues)(vpFeatures))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "float64": return ((_: void) => ((_x) => _x.supportsDoubleArrayValues)(vpFeatures))((_m as any).value);
    case "float32": return ((_: void) => ((_x) => _x.supportsFloatArrayValues)(vpFeatures))((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "uint8": return ((_: void) => ((_x) => _x.supportsByteArrayValues)(vpFeatures))((_m as any).value);
    case "int32": return ((_: void) => ((_x) => _x.supportsIntegerArrayValues)(vpFeatures))((_m as any).value);
    case "int64": return ((_: void) => ((_x) => _x.supportsLongArrayValues)(vpFeatures))((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    case "string": return ((_: void) => ((_x) => _x.supportsStringArrayValues)(vpFeatures))((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => true)((_m as any).value);
    case "map": return ((mt: Core.MapType) => ((_x) => _x.supportsMapKey)(extras)(((_x) => _x.keys)(mt)))((_m as any).value);
    case "wrap": return ((_: Core.Type) => true)((_m as any).value);
    case "maybe": return ((ot: Core.Type) => (() => {
  const _m = Strip.deannotateType(ot);
  switch (_m.tag) {
    case "literal": return ((_: Core.LiteralType) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return true(_m);
  }
})();
})());
  return ({
    name: name,
    constraints: ({
    eliminationVariants: eliminationVariants,
    literalVariants: literalVariants,
    floatTypes: floatTypes,
    functionVariants: functionVariants,
    integerTypes: integerTypes,
    termVariants: termVariants,
    typeVariants: typeVariants,
    types: typePredicate
  })
  });
})()));
}
