// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for generating term encoders from type modules
 */



import * as Annotations from "./annotations.js";
import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as EncodeCore from "./encode/core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Formatting from "./formatting.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Predicates from "./predicates.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function encodeBinding<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.DecodingError | Core.Binding)) {
  return ((graph: Graph.Graph) => ((b: Core.Binding) => LibEithers.bind(DecodeCore.type(graph)(((_x) => _x.term)(b)))(((typ: Core.Type) => ({ tag: "right", value: ({
    name: encodeBindingName(((_x) => _x.name)(b)),
    term: encodeTypeNamed(((_x) => _x.name)(b))(typ),
    type: encoderTypeSchemeNamed(((_x) => _x.name)(b))(typ)
  }) })))));
}

export function encodeBindingName(n: Core.Name): Core.Name {
  return LibLogic.ifElse(LibLogic.not(LibLists.null_(LibLists.tail(LibStrings.splitOn(".")(((_x) => _x)(n))))))(LibStrings.intercalate(".")(LibLists.concat2(["hydra", "encode"])(LibLists.concat2(LibLists.tail(LibLists.init(LibStrings.splitOn(".")(((_x) => _x)(n)))))([Formatting.decapitalize(Names.localNameOf(n))]))))(Formatting.decapitalize(Names.localNameOf(n)));
}

export function encodeEitherType(et: Core.EitherType): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: "e",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "either",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.eithers.bimap" }),
    argument: encodeType(((_x) => _x.left)(et))
  }) }),
    argument: encodeType(((_x) => _x.right)(et))
  }) }),
    argument: ({ tag: "variable", value: "e" })
  }) })
  })
  }) })
  }) });
}

export function encodeFieldValue(typeName: Core.Name): ((x: Core.Name) => ((x: Core.Type) => Core.Term)) {
  return ((fieldName: Core.Name) => ((fieldType: Core.Type) => ({ tag: "lambda", value: ({
    parameter: "y",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "inject",
    term: encodeInjection(typeName)(fieldName)(({ tag: "application", value: ({
    function: encodeType(fieldType),
    argument: ({ tag: "variable", value: "y" })
  }) }))
  })
  }) })
  }) })));
}

export function encodeFloatValue(floatType: Core.FloatType): ((x: Core.Term) => Core.Term) {
  return ((valTerm: Core.Term) => ({ tag: "inject", value: ({
    typeName: "hydra.core.FloatValue",
    field: ({
    name: (() => {
  const _m = floatType;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => "bigfloat")((_m as any).value);
    case "float32": return ((_: void) => "float32")((_m as any).value);
    case "float64": return ((_: void) => "float64")((_m as any).value);
  }
})(),
    term: valTerm
  })
  }) }));
}

export function encodeForallType(ft: Core.ForallType): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: encodeBindingName(((_x) => _x.parameter)(ft)),
    domain: null,
    body: encodeType(((_x) => _x.body)(ft))
  }) });
}

export function encodeInjection(typeName: Core.Name): ((x: Core.Name) => ((x: Core.Term) => Core.Term)) {
  return ((fieldName: Core.Name) => ((fieldTerm: Core.Term) => ({ tag: "record", value: ({
    typeName: "hydra.core.Injection",
    fields: [({
    name: "typeName",
    term: encodeName(typeName)
  }), ({
    name: "field",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Field",
    fields: [({
    name: "name",
    term: encodeName(fieldName)
  }), ({
    name: "term",
    term: fieldTerm
  })]
  }) })
  })]
  }) })));
}

export function encodeIntegerValue(intType: Core.IntegerType): ((x: Core.Term) => Core.Term) {
  return ((valTerm: Core.Term) => ({ tag: "inject", value: ({
    typeName: "hydra.core.IntegerValue",
    field: ({
    name: (() => {
  const _m = intType;
  switch (_m.tag) {
    case "bigint": return ((_: void) => "bigint")((_m as any).value);
    case "int8": return ((_: void) => "int8")((_m as any).value);
    case "int16": return ((_: void) => "int16")((_m as any).value);
    case "int32": return ((_: void) => "int32")((_m as any).value);
    case "int64": return ((_: void) => "int64")((_m as any).value);
    case "uint8": return ((_: void) => "uint8")((_m as any).value);
    case "uint16": return ((_: void) => "uint16")((_m as any).value);
    case "uint32": return ((_: void) => "uint32")((_m as any).value);
    case "uint64": return ((_: void) => "uint64")((_m as any).value);
  }
})(),
    term: valTerm
  })
  }) }));
}

export function encodeListType(elemType: Core.Type): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: "xs",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "list",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.lists.map" }),
    argument: encodeType(elemType)
  }) }),
    argument: ({ tag: "variable", value: "xs" })
  }) })
  })
  }) })
  }) });
}

export function encodeLiteralType(v1: Core.LiteralType): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "binary",
    term: ({ tag: "variable", value: "x" })
  })
  }) })
  })
  }) })
  }) }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "boolean",
    term: ({ tag: "variable", value: "x" })
  })
  }) })
  })
  }) })
  }) }))((_m as any).value);
    case "string": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "string",
    term: ({ tag: "variable", value: "x" })
  })
  }) })
  })
  }) })
  }) }))((_m as any).value);
    case "integer": return ((intType: Core.IntegerType) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "integer",
    term: encodeIntegerValue(intType)(({ tag: "variable", value: "x" }))
  })
  }) })
  })
  }) })
  }) }))((_m as any).value);
    case "float": return ((floatType: Core.FloatType) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "literal",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Literal",
    field: ({
    name: "float",
    term: encodeFloatValue(floatType)(({ tag: "variable", value: "x" }))
  })
  }) })
  })
  }) })
  }) }))((_m as any).value);
    default: return ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })(_m);
  }
})();
}

export function encodeMapType(mt: Core.MapType): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: "m",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "map",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maps.bimap" }),
    argument: encodeType(((_x) => _x.keys)(mt))
  }) }),
    argument: encodeType(((_x) => _x.values)(mt))
  }) }),
    argument: ({ tag: "variable", value: "m" })
  }) })
  })
  }) })
  }) });
}

export function encodeModule(cx: Context.Context): ((x: Graph.Graph) => ((x: Packaging.Module) => Errors.Error | Packaging.Module | null)) {
  return ((graph: Graph.Graph) => ((mod: Packaging.Module) => LibEithers.bind(filterTypeBindings(cx)(graph)(LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
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
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(mod)))))(((typeBindings: ReadonlyArray<Core.Binding>) => LibLogic.ifElse(LibLists.null_(typeBindings))(({ tag: "right", value: null }))(LibEithers.bind(LibEithers.mapList(((b: Core.Binding) => LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((x: Core.Binding) => x))(encodeBinding(cx)(graph)(b))))(typeBindings))(((encodedBindings: ReadonlyArray<Core.Binding>) => ({ tag: "right", value: ({
    namespace: encodeNamespace(((_x) => _x.namespace)(mod)),
    definitions: LibLists.map(((b: Core.Binding) => ({ tag: "term", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }) })))(encodedBindings),
    termDependencies: LibLists.nub(LibLists.concat2(LibLists.map(encodeNamespace)(((_x) => _x.typeDependencies)(mod)))(LibLists.map(encodeNamespace)(((_x) => _x.termDependencies)(mod)))),
    typeDependencies: [((_x) => _x.namespace)(mod)],
    description: LibStrings.cat(["Term encoders for ", ((_x) => _x)(((_x) => _x.namespace)(mod))])
  }) }))))))));
}

export function encodeName(n: Core.Name): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(n) }) })
  }) });
}

export function encodeNamespace(ns: Packaging.Namespace): Packaging.Namespace {
  return LibStrings.cat(["hydra.encode.", LibStrings.intercalate(".")(LibLists.tail(LibStrings.splitOn(".")(((_x) => _x)(ns))))]);
}

export function encodeOptionalType(elemType: Core.Type): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: "opt",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "maybe",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.maybes.map" }),
    argument: encodeType(elemType)
  }) }),
    argument: ({ tag: "variable", value: "opt" })
  }) })
  })
  }) })
  }) });
}

export function encodePairType(pt: Core.PairType): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: "p",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "pair",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.pairs.bimap" }),
    argument: encodeType(((_x) => _x.first)(pt))
  }) }),
    argument: encodeType(((_x) => _x.second)(pt))
  }) }),
    argument: ({ tag: "variable", value: "p" })
  }) })
  })
  }) })
  }) });
}

export function encodeRecordType(rt: ReadonlyArray<Core.FieldType>): Core.Term {
  return encodeRecordTypeNamed("unknown")(rt);
}

export function encodeRecordTypeNamed(ename: Core.Name): ((x: ReadonlyArray<Core.FieldType>) => Core.Term) {
  return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "record",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Record",
    fields: [({
    name: "typeName",
    term: encodeName(ename)
  }), ({
    name: "fields",
    term: ({ tag: "list", value: LibLists.map(((ft: Core.FieldType) => ({ tag: "record", value: ({
    typeName: "hydra.core.Field",
    fields: [({
    name: "name",
    term: encodeName(((_x) => _x.name)(ft))
  }), ({
    name: "term",
    term: ({ tag: "application", value: ({
    function: encodeType(((_x) => _x.type)(ft)),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "project", value: ({
    typeName: ename,
    field: ((_x) => _x.name)(ft)
  }) }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  })]
  }) })))(rt) })
  })]
  }) })
  })
  }) })
  }) }));
}

export function encodeSetType(elemType: Core.Type): Core.Term {
  return ({ tag: "lambda", value: ({
    parameter: "s",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "set",
    term: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.map" }),
    argument: encodeType(elemType)
  }) }),
    argument: ({ tag: "variable", value: "s" })
  }) })
  })
  }) })
  }) });
}

export function encodeType(v1: Core.Type): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encodeType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: encodeType(((_x) => _x.function)(appType)),
    argument: encodeType(((_x) => _x.argument)(appType))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => encodeEitherType(et))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => encodeForallType(ft))((_m as any).value);
    case "function": return ((_: Core.FunctionType) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => encodeListType(elemType))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => encodeLiteralType(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => encodeMapType(mt))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => encodeOptionalType(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => encodePairType(pt))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => encodeRecordType(rt))((_m as any).value);
    case "set": return ((elemType: Core.Type) => encodeSetType(elemType))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => encodeUnionType(rt))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => encodeWrappedType(wt))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) })
  }) }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) })
  }) }))((_m as any).value);
    case "variable": return ((typeName: Core.Name) => ({ tag: "variable", value: encodeBindingName(typeName) }))((_m as any).value);
    default: return ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })(_m);
  }
})();
}

export function encodeTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Term) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encodeTypeNamed(ename)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: encodeType(((_x) => _x.function)(appType)),
    argument: encodeType(((_x) => _x.argument)(appType))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => encodeEitherType(et))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "lambda", value: ({
    parameter: encodeBindingName(((_x) => _x.parameter)(ft)),
    domain: null,
    body: encodeTypeNamed(ename)(((_x) => _x.body)(ft))
  }) }))((_m as any).value);
    case "function": return ((_: Core.FunctionType) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => encodeListType(elemType))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => encodeLiteralType(lt))((_m as any).value);
    case "map": return ((mt: Core.MapType) => encodeMapType(mt))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => encodeOptionalType(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => encodePairType(pt))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => encodeRecordTypeNamed(ename)(rt))((_m as any).value);
    case "set": return ((elemType: Core.Type) => encodeSetType(elemType))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => encodeUnionTypeNamed(ename)(rt))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => encodeWrappedTypeNamed(ename)(wt))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) })
  }) }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "lambda", value: ({
    parameter: "_",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) })
  }) }))((_m as any).value);
    case "variable": return ((typeName: Core.Name) => ({ tag: "variable", value: encodeBindingName(typeName) }))((_m as any).value);
    default: return ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "variable", value: "x" })
  }) })(_m);
  }
})());
}

export function encodeUnionType(rt: ReadonlyArray<Core.FieldType>): Core.Term {
  return encodeUnionTypeNamed("unknown")(rt);
}

export function encodeUnionTypeNamed(ename: Core.Name): ((x: ReadonlyArray<Core.FieldType>) => Core.Term) {
  return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "cases", value: ({
    typeName: ename,
    default: null,
    cases: LibLists.map(((ft: Core.FieldType) => ({
    name: ((_x) => _x.name)(ft),
    term: encodeFieldValue(ename)(((_x) => _x.name)(ft))(((_x) => _x.type)(ft))
  })))(rt)
  }) }));
}

export function encodeWrappedType(wt: Core.Type): Core.Term {
  return encodeWrappedTypeNamed("unknown")(wt);
}

export function encodeWrappedTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Term) {
  return ((wt: Core.Type) => ({ tag: "lambda", value: ({
    parameter: "x",
    domain: null,
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "wrap",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.WrappedTerm",
    fields: [({
    name: "typeName",
    term: encodeName(ename)
  }), ({
    name: "body",
    term: ({ tag: "application", value: ({
    function: encodeType(wt),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: ename }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  }) })
  })]
  }) })
  })
  }) })
  }) }));
}

export function encoderCollectForallVariables(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encoderCollectForallVariables(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => LibLists.cons(((_x) => _x.parameter)(ft))(encoderCollectForallVariables(((_x) => _x.body)(ft))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function encoderCollectOrdVars(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encoderCollectOrdVars(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => LibLists.concat2(encoderCollectOrdVars(((_x) => _x.function)(appType)))(encoderCollectOrdVars(((_x) => _x.argument)(appType))))((_m as any).value);
    case "either": return ((et: Core.EitherType) => LibLists.concat2(encoderCollectOrdVars(((_x) => _x.left)(et)))(encoderCollectOrdVars(((_x) => _x.right)(et))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => encoderCollectOrdVars(((_x) => _x.body)(ft)))((_m as any).value);
    case "list": return ((elemType: Core.Type) => encoderCollectOrdVars(elemType))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibLists.concat([encoderCollectTypeVarsFromType(((_x) => _x.keys)(mt)), encoderCollectOrdVars(((_x) => _x.keys)(mt)), encoderCollectOrdVars(((_x) => _x.values)(mt))]))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => encoderCollectOrdVars(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibLists.concat2(encoderCollectOrdVars(((_x) => _x.first)(pt)))(encoderCollectOrdVars(((_x) => _x.second)(pt))))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => encoderCollectOrdVars(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "set": return ((elemType: Core.Type) => LibLists.concat2(encoderCollectTypeVarsFromType(elemType))(encoderCollectOrdVars(elemType)))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => encoderCollectOrdVars(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => encoderCollectOrdVars(wt))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function encoderCollectTypeVarsFromType(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encoderCollectTypeVarsFromType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => LibLists.concat2(encoderCollectTypeVarsFromType(((_x) => _x.function)(appType)))(encoderCollectTypeVarsFromType(((_x) => _x.argument)(appType))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => encoderCollectTypeVarsFromType(((_x) => _x.body)(ft)))((_m as any).value);
    case "list": return ((elemType: Core.Type) => encoderCollectTypeVarsFromType(elemType))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibLists.concat2(encoderCollectTypeVarsFromType(((_x) => _x.keys)(mt)))(encoderCollectTypeVarsFromType(((_x) => _x.values)(mt))))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => encoderCollectTypeVarsFromType(elemType))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibLists.concat2(encoderCollectTypeVarsFromType(((_x) => _x.first)(pt)))(encoderCollectTypeVarsFromType(((_x) => _x.second)(pt))))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => encoderCollectTypeVarsFromType(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "set": return ((elemType: Core.Type) => encoderCollectTypeVarsFromType(elemType))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibLists.concat(LibLists.map(((ft: Core.FieldType) => encoderCollectTypeVarsFromType(((_x) => _x.type)(ft))))(rt)))((_m as any).value);
    case "variable": return ((name: Core.Name) => [name])((_m as any).value);
    case "wrap": return ((wt: Core.Type) => encoderCollectTypeVarsFromType(wt))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function encoderFullResultType(typ: Core.Type): Core.Type {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encoderFullResultType(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: encoderFullResultType(((_x) => _x.function)(appType)),
    argument: ((_x) => _x.argument)(appType)
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: encoderFullResultType(((_x) => _x.left)(et)),
    right: encoderFullResultType(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "application", value: ({
    function: encoderFullResultType(((_x) => _x.body)(ft)),
    argument: ({ tag: "variable", value: ((_x) => _x.parameter)(ft) })
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => ({ tag: "list", value: encoderFullResultType(elemType) }))((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => ({ tag: "variable", value: "hydra.core.Literal" }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: encoderFullResultType(((_x) => _x.keys)(mt)),
    values: encoderFullResultType(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => ({ tag: "maybe", value: encoderFullResultType(elemType) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: encoderFullResultType(((_x) => _x.first)(pt)),
    second: encoderFullResultType(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: "hydra.core.Term" }))((_m as any).value);
    case "set": return ((elemType: Core.Type) => ({ tag: "set", value: encoderFullResultType(elemType) }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: "hydra.core.Term" }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "variable", value: name }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "void" }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "variable", value: "hydra.core.Term" }))((_m as any).value);
    default: return ({ tag: "variable", value: "hydra.core.Term" })(_m);
  }
})();
}

export function encoderFullResultTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encoderFullResultTypeNamed(ename)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((appType: Core.ApplicationType) => ({ tag: "application", value: ({
    function: encoderFullResultType(((_x) => _x.function)(appType)),
    argument: ((_x) => _x.argument)(appType)
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: encoderFullResultType(((_x) => _x.left)(et)),
    right: encoderFullResultType(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "application", value: ({
    function: encoderFullResultTypeNamed(ename)(((_x) => _x.body)(ft)),
    argument: ({ tag: "variable", value: ((_x) => _x.parameter)(ft) })
  }) }))((_m as any).value);
    case "list": return ((elemType: Core.Type) => ({ tag: "list", value: encoderFullResultType(elemType) }))((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => ({ tag: "variable", value: "hydra.core.Literal" }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: encoderFullResultType(((_x) => _x.keys)(mt)),
    values: encoderFullResultType(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "maybe": return ((elemType: Core.Type) => ({ tag: "maybe", value: encoderFullResultType(elemType) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: encoderFullResultType(((_x) => _x.first)(pt)),
    second: encoderFullResultType(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: ename }))((_m as any).value);
    case "set": return ((elemType: Core.Type) => ({ tag: "set", value: encoderFullResultType(elemType) }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "variable", value: ename }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "variable", value: name }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "void" }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "variable", value: ename }))((_m as any).value);
    default: return ({ tag: "variable", value: "hydra.core.Term" })(_m);
  }
})());
}

export function encoderType(typ: Core.Type): Core.Type {
  return (() => {
  const resultType = encoderFullResultType(typ);
  return (() => {
  const baseType = ({ tag: "function", value: ({
    domain: resultType,
    codomain: ({ tag: "variable", value: "hydra.core.Term" })
  }) });
  return prependForallEncoders(baseType)(typ);
})();
})();
}

export function encoderTypeNamed(ename: Core.Name): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const resultType = encoderFullResultTypeNamed(ename)(typ);
  return (() => {
  const baseType = ({ tag: "function", value: ({
    domain: resultType,
    codomain: ({ tag: "variable", value: "hydra.core.Term" })
  }) });
  return prependForallEncoders(baseType)(typ);
})();
})());
}

export function encoderTypeScheme(typ: Core.Type): Core.TypeScheme {
  return (() => {
  const typeVars = encoderCollectForallVariables(typ);
  const encoderFunType = encoderType(typ);
  const allOrdVars = encoderCollectOrdVars(typ);
  const ordVars = LibLists.filter(((v: Core.Name) => LibLists.elem(v)(typeVars)))(allOrdVars);
  const constraints = LibLogic.ifElse(LibLists.null_(ordVars))(null)(LibMaps.fromList(LibLists.map(((v: Core.Name) => [v, ({
    classes: LibSets.singleton("ordering")
  })]))(ordVars)));
  return ({
    variables: typeVars,
    type: encoderFunType,
    constraints: constraints
  });
})();
}

export function encoderTypeSchemeNamed(ename: Core.Name): ((x: Core.Type) => Core.TypeScheme) {
  return ((typ: Core.Type) => (() => {
  const typeVars = encoderCollectForallVariables(typ);
  const encoderFunType = encoderTypeNamed(ename)(typ);
  const allOrdVars = encoderCollectOrdVars(typ);
  const ordVars = LibLists.filter(((v: Core.Name) => LibLists.elem(v)(typeVars)))(allOrdVars);
  const constraints = LibLogic.ifElse(LibLists.null_(ordVars))(null)(LibMaps.fromList(LibLists.map(((v: Core.Name) => [v, ({
    classes: LibSets.singleton("ordering")
  })]))(ordVars)));
  return ({
    variables: typeVars,
    type: encoderFunType,
    constraints: constraints
  });
})());
}

export function filterTypeBindings(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Binding>) => Errors.Error | ReadonlyArray<Core.Binding>)) {
  return ((graph: Graph.Graph) => ((bindings: ReadonlyArray<Core.Binding>) => LibEithers.map(LibMaybes.cat)(LibEithers.mapList(((v1: Core.Binding) => isEncodableBinding(cx)(graph)(v1)))(LibLists.filter(Annotations.isNativeType)(bindings)))));
}

export function isEncodableBinding(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.Error | Core.Binding | null)) {
  return ((graph: Graph.Graph) => ((b: Core.Binding) => LibEithers.bind(Predicates.isSerializableByName(cx)(graph)(((_x) => _x.name)(b)))(((serializable: boolean) => ({ tag: "right", value: LibLogic.ifElse(serializable)(b)(null) })))));
}

export function isUnitType(v1: Core.Type): boolean {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function prependForallEncoders(baseType: Core.Type): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => prependForallEncoders(baseType)(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "function", value: ({
    domain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: ((_x) => _x.parameter)(ft) }),
    codomain: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    codomain: prependForallEncoders(baseType)(((_x) => _x.body)(ft))
  }) }))((_m as any).value);
    default: return baseType(_m);
  }
})());
}
