// Note: this is an automatically generated file. Do not edit.

/**
 * A utility which instantiates a nonrecursive type with default values
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as ShowCore from "./show/core.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function graphToSchema<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Binding>) => Errors.DecodingError | ReadonlyMap<Core.Name, Core.Type>)) {
  return ((graph: Graph.Graph) => ((els: ReadonlyArray<Core.Binding>) => (() => {
  const toPair = ((el: Core.Binding) => (() => {
  const name = ((_x) => _x.name)(el);
  return LibEithers.bind(DecodeCore.type(graph)(((_x) => _x.term)(el)))(((t: Core.Type) => ({ tag: "right", value: [name, t] })));
})());
  return LibEithers.bind(LibEithers.mapList(toPair)(els))(((pairs: ReadonlyArray<readonly [Core.Name, Core.Type]>) => ({ tag: "right", value: LibMaps.fromList(pairs) })));
})()));
}

export function instantiateTemplate<t0>(cx: t0): ((x: boolean) => ((x: ReadonlyMap<Core.Name, Core.Type>) => ((x: Core.Name) => ((x: Core.Type) => Errors.Error | Core.Term)))) {
  return ((minimal: boolean) => ((schema: ReadonlyMap<Core.Name, Core.Type>) => ((tname: Core.Name) => ((t: Core.Type) => (() => {
  const inst = ((tn: Core.Name) => ((v1: Core.Type) => instantiateTemplate(cx)(minimal)(schema)(tn)(v1)));
  return (() => {
  const noPoly = ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "non-polymorphic type",
    actual: "polymorphic or function type"
  }) }) }) });
  return (() => {
  const forFloat = ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => ({ tag: "bigfloat", value: 0.0 }))((_m as any).value);
    case "float32": return ((_: void) => ({ tag: "float32", value: 0.0 }))((_m as any).value);
    case "float64": return ((_: void) => ({ tag: "float64", value: 0.0 }))((_m as any).value);
  }
})());
  return (() => {
  const forInteger = ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "bigint", value: 0n }))((_m as any).value);
    case "int8": return ((_: void) => ({ tag: "int8", value: 0 }))((_m as any).value);
    case "int16": return ((_: void) => ({ tag: "int16", value: 0n }))((_m as any).value);
    case "int32": return ((_: void) => ({ tag: "int32", value: 0 }))((_m as any).value);
    case "int64": return ((_: void) => ({ tag: "int64", value: 0n }))((_m as any).value);
    case "uint8": return ((_: void) => ({ tag: "uint8", value: 0n }))((_m as any).value);
    case "uint16": return ((_: void) => ({ tag: "uint16", value: 0 }))((_m as any).value);
    case "uint32": return ((_: void) => ({ tag: "uint32", value: 0n }))((_m as any).value);
    case "uint64": return ((_: void) => ({ tag: "uint64", value: 0n }))((_m as any).value);
  }
})());
  return (() => {
  const forLiteral = ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "string", value: "" }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "boolean", value: false }))((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => ({ tag: "integer", value: forInteger(it) }))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => ({ tag: "float", value: forFloat(ft) }))((_m as any).value);
    case "string": return ((_: void) => ({ tag: "string", value: "" }))((_m as any).value);
  }
})());
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => inst(tname)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((_: Core.ApplicationType) => noPoly)((_m as any).value);
    case "function": return ((_: Core.FunctionType) => noPoly)((_m as any).value);
    case "forall": return ((_: Core.ForallType) => noPoly)((_m as any).value);
    case "list": return ((et: Core.Type) => LibLogic.ifElse(minimal)(({ tag: "right", value: ({ tag: "list", value: [] }) }))(LibEithers.bind(inst(tname)(et))(((e: Core.Term) => ({ tag: "right", value: ({ tag: "list", value: [e] }) })))))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => ({ tag: "right", value: ({ tag: "literal", value: forLiteral(lt) }) }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => (() => {
  const kt = ((_x) => _x.keys)(mt);
  return (() => {
  const vt = ((_x) => _x.values)(mt);
  return LibLogic.ifElse(minimal)(({ tag: "right", value: ({ tag: "map", value: LibMaps.empty }) }))(LibEithers.bind(inst(tname)(kt))(((ke: Core.Term) => LibEithers.bind(inst(tname)(vt))(((ve: Core.Term) => ({ tag: "right", value: ({ tag: "map", value: LibMaps.singleton(ke)(ve) }) }))))));
})();
})())((_m as any).value);
    case "maybe": return ((ot: Core.Type) => LibLogic.ifElse(minimal)(({ tag: "right", value: ({ tag: "maybe", value: null }) }))(LibEithers.bind(inst(tname)(ot))(((e: Core.Term) => ({ tag: "right", value: ({ tag: "maybe", value: e }) })))))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const toField = ((ft: Core.FieldType) => LibEithers.bind(inst(tname)(((_x) => _x.type)(ft)))(((e: Core.Term) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(ft),
    term: e
  }) }))));
  return LibEithers.bind(LibEithers.mapList(toField)(rt))(((dfields: ReadonlyArray<Core.Field>) => ({ tag: "right", value: ({ tag: "record", value: ({
    typeName: tname,
    fields: dfields
  }) }) })));
})())((_m as any).value);
    case "set": return ((et: Core.Type) => LibLogic.ifElse(minimal)(({ tag: "right", value: ({ tag: "set", value: LibSets.empty }) }))(LibEithers.bind(inst(tname)(et))(((e: Core.Term) => ({ tag: "right", value: ({ tag: "set", value: LibSets.fromList([e]) }) })))))((_m as any).value);
    case "variable": return ((vname: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "unexpectedShape", value: ({
    expected: "bound type variable",
    actual: LibStrings.cat2("unbound variable ")(((_x) => _x)(vname))
  }) }) }) }))(((v1: Core.Type) => inst(vname)(v1)))(LibMaps.lookup(vname)(schema)))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => LibEithers.bind(inst(tname)(wt))(((e: Core.Term) => ({ tag: "right", value: ({ tag: "wrap", value: ({
    typeName: tname,
    body: e
  }) }) }))))((_m as any).value);
  }
})();
})();
})();
})();
})();
})()))));
}
