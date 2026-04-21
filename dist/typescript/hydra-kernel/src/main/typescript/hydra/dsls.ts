// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for generating domain-specific DSL modules from type modules
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
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibStrings from "./lib/strings.js";
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function collectForallVars(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => collectForallVars(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => LibLists.cons(((_x) => _x.parameter)(ft))(collectForallVars(((_x) => _x.body)(ft))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function deduplicateBindings(bindings: ReadonlyArray<Core.Binding>): ReadonlyArray<Core.Binding> {
  return LibLists.foldl(((acc: ReadonlyArray<Core.Binding>) => ((b: Core.Binding) => (() => {
  const n = ((_x) => _x)(((_x) => _x.name)(b));
  return (() => {
  const usedNames = LibLists.map(((a: Core.Binding) => ((_x) => _x)(((_x) => _x.name)(a))))(acc);
  return (() => {
  const uniqueName = findUniqueName(n)(usedNames);
  return LibLists.concat2(acc)([({
    name: uniqueName,
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  })]);
})();
})();
})())))([])(bindings);
}

export function dslBindingName(n: Core.Name): Core.Name {
  return (() => {
  const parts = LibStrings.splitOn(".")(((_x) => _x)(n));
  return LibLogic.ifElse(LibLogic.not(LibLists.null_(LibLists.tail(parts))))(LibLogic.ifElse(LibEquality.equal(LibLists.head(parts))("hydra"))(LibStrings.intercalate(".")(LibLists.concat2(["hydra", "dsl"])(LibLists.concat2(LibLists.tail(LibLists.init(parts)))([Formatting.decapitalize(Names.localNameOf(n))]))))(LibStrings.intercalate(".")(LibLists.concat2(["hydra", "dsl"])(LibLists.concat2(LibLists.init(parts))([Formatting.decapitalize(Names.localNameOf(n))])))))(Formatting.decapitalize(Names.localNameOf(n)));
})();
}

export function dslDefinitionName(typeName: Core.Name): ((x: string) => Core.Name) {
  return ((localName: string) => (() => {
  const parts = LibStrings.splitOn(".")(((_x) => _x)(typeName));
  return (() => {
  const nsParts = LibLists.init(parts);
  return (() => {
  const dslNsParts = LibLogic.ifElse(LibEquality.equal(LibLists.head(nsParts))("hydra"))(LibLists.concat2(["hydra", "dsl"])(LibLists.tail(nsParts)))(LibLists.concat2(["hydra", "dsl"])(nsParts));
  return LibStrings.intercalate(".")(LibLists.concat2(dslNsParts)([localName]));
})();
})();
})());
}

export function dslModule<t0>(cx: t0): ((x: Graph.Graph) => ((x: Packaging.Module) => Errors.Error | Packaging.Module | null)) {
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
})()))(((_x) => _x.definitions)(mod)))))(((typeBindings: ReadonlyArray<Core.Binding>) => LibLogic.ifElse(LibLists.null_(typeBindings))(({ tag: "right", value: null }))(LibEithers.bind(LibEithers.mapList(((b: Core.Binding) => LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((x: ReadonlyArray<Core.Binding>) => x))(generateBindingsForType(cx)(graph)(b))))(typeBindings))(((dslBindings: ReadonlyArray<ReadonlyArray<Core.Binding>>) => ({ tag: "right", value: ({
    namespace: dslNamespace(((_x) => _x.namespace)(mod)),
    definitions: LibLists.map(((b: Core.Binding) => ({ tag: "term", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }) })))(deduplicateBindings(LibLists.concat(dslBindings))),
    termDependencies: LibLists.nub(LibLists.map(dslNamespace)(((_x) => _x.typeDependencies)(mod))),
    typeDependencies: LibLists.nub(LibLists.concat2([((_x) => _x.namespace)(mod), "hydra.phantoms"])(((_x) => _x.typeDependencies)(mod))),
    description: LibStrings.cat(["DSL functions for ", ((_x) => _x)(((_x) => _x.namespace)(mod))])
  }) }))))))));
}

export function dslNamespace(ns: Packaging.Namespace): Packaging.Namespace {
  return (() => {
  const parts = LibStrings.splitOn(".")(((_x) => _x)(ns));
  return LibLogic.ifElse(LibEquality.equal(LibLists.head(parts))("hydra"))(LibStrings.cat(["hydra.dsl.", LibStrings.intercalate(".")(LibLists.tail(parts))]))(LibStrings.cat(["hydra.dsl.", ((_x) => _x)(ns)]));
})();
}

export function dslTypeScheme(origType: Core.Type): ((x: ReadonlyArray<Core.Type>) => ((x: Core.Type) => Core.TypeScheme)) {
  return ((paramTypes: ReadonlyArray<Core.Type>) => ((resultType: Core.Type) => (() => {
  const typeVars = collectForallVars(origType);
  return (() => {
  const wrappedResult = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: resultType
  }) });
  return (() => {
  const funType = LibLists.foldr(((paramType: Core.Type) => ((acc: Core.Type) => ({ tag: "function", value: ({
    domain: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: paramType
  }) }),
    codomain: acc
  }) }))))(wrappedResult)(paramTypes);
  return ({
    variables: typeVars,
    type: funType,
    constraints: null
  });
})();
})();
})()));
}

export function filterTypeBindings<t0, t1, t2>(cx: t0): ((x: t1) => ((x: ReadonlyArray<Core.Binding>) => t2 | ReadonlyArray<Core.Binding>)) {
  return ((graph: t1) => ((bindings: ReadonlyArray<Core.Binding>) => LibEithers.map(LibMaybes.cat)(LibEithers.mapList(((v1: Core.Binding) => isDslEligibleBinding(cx)(graph)(v1)))(LibLists.filter(Annotations.isNativeType)(bindings)))));
}

export function findUniqueName(candidate: string): ((x: ReadonlyArray<string>) => string) {
  return ((usedNames: ReadonlyArray<string>) => LibLogic.ifElse(LibLists.null_(LibLists.filter(((v1) => LibEquality.equal(candidate)(v1)))(usedNames)))(candidate)(findUniqueName(LibStrings.cat([candidate, "_"]))(usedNames)));
}

export function generateBindingsForType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.DecodingError | ReadonlyArray<Core.Binding>)) {
  return ((graph: Graph.Graph) => ((b: Core.Binding) => (() => {
  const typeName = ((_x) => _x.name)(b);
  return LibEithers.bind(DecodeCore.type(graph)(((_x) => _x.term)(b)))(((rawType: Core.Type) => (() => {
  const typ = Strip.deannotateTypeParameters(Strip.deannotateType(rawType));
  return ({ tag: "right", value: (() => {
  const _m = typ;
  switch (_m.tag) {
    case "record": return ((fts: ReadonlyArray<Core.FieldType>) => LibLists.concat([generateRecordConstructor(rawType)(typeName)(fts), LibLists.map(((v1: Core.FieldType) => generateRecordAccessor(rawType)(typeName)(v1)))(fts), LibLists.map(((v1: Core.FieldType) => generateRecordWithUpdater(rawType)(typeName)(fts)(v1)))(fts)]))((_m as any).value);
    case "union": return ((fts: ReadonlyArray<Core.FieldType>) => LibLists.map(((v1: Core.FieldType) => generateUnionInjector(rawType)(typeName)(v1)))(fts))((_m as any).value);
    case "wrap": return ((innerType: Core.Type) => generateWrappedTypeAccessors(rawType)(typeName)(innerType))((_m as any).value);
    default: return [](_m);
  }
})() });
})()));
})()));
}

export function generateRecordAccessor(origType: Core.Type): ((x: Core.Name) => ((x: Core.FieldType) => Core.Binding)) {
  return ((typeName: Core.Name) => ((ft: Core.FieldType) => (() => {
  const fieldName = ((_x) => _x.name)(ft);
  return (() => {
  const accessorLocalName = LibStrings.cat([Formatting.decapitalize(Names.localNameOf(typeName)), LibStrings.intercalate("")(LibLists.map(((s: string) => Formatting.capitalize(s)))(LibStrings.splitOn(".")(((_x) => _x)(fieldName))))]);
  return (() => {
  const accessorName = dslDefinitionName(typeName)(accessorLocalName);
  return (() => {
  const paramDomain = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: nominalResultType(typeName)(origType)
  }) });
  return (() => {
  const body = ({ tag: "lambda", value: ({
    parameter: "x",
    domain: paramDomain,
    body: ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "application",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Application",
    fields: [({
    name: "function",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "project",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Projection",
    fields: [({
    name: "typeName",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  }), ({
    name: "field",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(fieldName) }) })
  }) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "argument",
    term: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) })
  })
  }) })
  }) })
  }) });
  return (() => {
  const ts = dslTypeScheme(origType)([nominalResultType(typeName)(origType)])(((_x) => _x.type)(ft));
  return ({
    name: accessorName,
    term: body,
    type: ts
  });
})();
})();
})();
})();
})();
})()));
}

export function generateRecordConstructor(origType: Core.Type): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ReadonlyArray<Core.Binding>)) {
  return ((typeName: Core.Name) => ((fieldTypes: ReadonlyArray<Core.FieldType>) => (() => {
  const dFields = LibLists.map(((ft: Core.FieldType) => ({ tag: "record", value: ({
    typeName: "hydra.core.Field",
    fields: [({
    name: "name",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(((_x) => _x.name)(ft)) }) })
  }) })
  }), ({
    name: "term",
    term: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: Formatting.decapitalize(Names.localNameOf(((_x) => _x.name)(ft))) })
  }) })
  })]
  }) })))(fieldTypes);
  return (() => {
  const recordTerm = ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "record",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Record",
    fields: [({
    name: "typeName",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  }), ({
    name: "fields",
    term: ({ tag: "list", value: dFields })
  })]
  }) })
  })
  }) })
  }) });
  return (() => {
  const paramPairs = LibLists.map(((ft: Core.FieldType) => [Formatting.decapitalize(Names.localNameOf(((_x) => _x.name)(ft))), ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: ((_x) => _x.type)(ft)
  }) })]))(fieldTypes);
  return (() => {
  const body = LibLists.foldl(((acc: Core.Term) => ((pp: readonly [string, Core.Type]) => ({ tag: "lambda", value: ({
    parameter: LibPairs.first(pp),
    domain: LibPairs.second(pp),
    body: acc
  }) }))))(recordTerm)(LibLists.reverse(paramPairs));
  return (() => {
  const paramTypes = LibLists.map(((ft: Core.FieldType) => ((_x) => _x.type)(ft)))(fieldTypes);
  return (() => {
  const resultType = nominalResultType(typeName)(origType);
  return (() => {
  const ts = dslTypeScheme(origType)(paramTypes)(resultType);
  return [({
    name: dslBindingName(typeName),
    term: body,
    type: ts
  })];
})();
})();
})();
})();
})();
})();
})()));
}

export function generateRecordWithUpdater(origType: Core.Type): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: Core.FieldType) => Core.Binding))) {
  return ((typeName: Core.Name) => ((allFields: ReadonlyArray<Core.FieldType>) => ((targetField: Core.FieldType) => (() => {
  const targetFieldName = ((_x) => _x.name)(targetField);
  return (() => {
  const updaterLocalName = LibStrings.cat([Formatting.decapitalize(Names.localNameOf(typeName)), "With", LibStrings.intercalate("")(LibLists.map(((s: string) => Formatting.capitalize(s)))(LibStrings.splitOn(".")(((_x) => _x)(targetFieldName))))]);
  return (() => {
  const updaterName = dslDefinitionName(typeName)(updaterLocalName);
  return (() => {
  const dFields = LibLists.map(((ft: Core.FieldType) => ({ tag: "record", value: ({
    typeName: "hydra.core.Field",
    fields: [({
    name: "name",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(((_x) => _x.name)(ft)) }) })
  }) })
  }), ({
    name: "term",
    term: LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.name)(ft)))(((_x) => _x)(targetFieldName)))(({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: "newVal" })
  }) }))(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "application",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Application",
    fields: [({
    name: "function",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "project",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Projection",
    fields: [({
    name: "typeName",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  }), ({
    name: "field",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(((_x) => _x.name)(ft)) }) })
  }) })
  })]
  }) })
  })
  }) })
  }), ({
    name: "argument",
    term: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: "original" })
  }) })
  })]
  }) })
  })
  }) }))
  })]
  }) })))(allFields);
  return (() => {
  const recDomain = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: nominalResultType(typeName)(origType)
  }) });
  return (() => {
  const fieldDomain = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: ((_x) => _x.type)(targetField)
  }) });
  return (() => {
  const body = ({ tag: "lambda", value: ({
    parameter: "original",
    domain: recDomain,
    body: ({ tag: "lambda", value: ({
    parameter: "newVal",
    domain: fieldDomain,
    body: ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "record",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Record",
    fields: [({
    name: "typeName",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  }), ({
    name: "fields",
    term: ({ tag: "list", value: dFields })
  })]
  }) })
  })
  }) })
  }) })
  }) })
  }) });
  return (() => {
  const recType = nominalResultType(typeName)(origType);
  return (() => {
  const ts = dslTypeScheme(origType)([recType, ((_x) => _x.type)(targetField)])(recType);
  return ({
    name: updaterName,
    term: body,
    type: ts
  });
})();
})();
})();
})();
})();
})();
})();
})();
})())));
}

export function generateUnionInjector(origType: Core.Type): ((x: Core.Name) => ((x: Core.FieldType) => Core.Binding)) {
  return ((typeName: Core.Name) => ((ft: Core.FieldType) => (() => {
  const fieldName = ((_x) => _x.name)(ft);
  return (() => {
  const fieldType = ((_x) => _x.type)(ft);
  return (() => {
  const injectorLocalName = LibStrings.cat([Formatting.decapitalize(Names.localNameOf(typeName)), LibStrings.intercalate("")(LibLists.map(((s: string) => Formatting.capitalize(s)))(LibStrings.splitOn(".")(((_x) => _x)(fieldName))))]);
  return (() => {
  const injectorName = dslDefinitionName(typeName)(injectorLocalName);
  return (() => {
  const isUnit = (() => {
  const _m = Strip.deannotateType(fieldType);
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
  return (() => {
  const dFieldValue = LibLogic.ifElse(isUnit)(({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unit",
    term: ({ tag: "unit" })
  })
  }) }))(({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: "x" })
  }) }));
  return (() => {
  const injectionTerm = ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "inject",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Injection",
    fields: [({
    name: "typeName",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  }), ({
    name: "field",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Field",
    fields: [({
    name: "name",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(fieldName) }) })
  }) })
  }), ({
    name: "term",
    term: dFieldValue
  })]
  }) })
  })]
  }) })
  })
  }) })
  }) });
  return (() => {
  const variantDomain = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: ((_x) => _x.type)(ft)
  }) });
  return (() => {
  const body = LibLogic.ifElse(isUnit)(injectionTerm)(({ tag: "lambda", value: ({
    parameter: "x",
    domain: variantDomain,
    body: injectionTerm
  }) }));
  return (() => {
  const unionType = nominalResultType(typeName)(origType);
  return (() => {
  const ts = LibLogic.ifElse(isUnit)(dslTypeScheme(origType)([])(unionType))(dslTypeScheme(origType)([((_x) => _x.type)(ft)])(unionType));
  return ({
    name: injectorName,
    term: body,
    type: ts
  });
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
})()));
}

export function generateWrappedTypeAccessors(origType: Core.Type): ((x: Core.Name) => ((x: Core.Type) => ReadonlyArray<Core.Binding>)) {
  return ((typeName: Core.Name) => ((innerType: Core.Type) => (() => {
  const localName = Names.localNameOf(typeName);
  return (() => {
  const wrapName = dslDefinitionName(typeName)(Formatting.decapitalize(localName));
  return (() => {
  const unwrapLocalName = LibStrings.cat(["un", localName]);
  return (() => {
  const unwrapName = dslDefinitionName(typeName)(unwrapLocalName);
  return (() => {
  const wrapperType = nominalResultType(typeName)(origType);
  return (() => {
  const wrapDomain = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: innerType
  }) });
  return (() => {
  const wrapBody = ({ tag: "lambda", value: ({
    parameter: "x",
    domain: wrapDomain,
    body: ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "wrap",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.WrappedTerm",
    fields: [({
    name: "typeName",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  }), ({
    name: "body",
    term: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) })
  })
  }) })
  }) })
  }) });
  return (() => {
  const unwrapDomain = ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.phantoms.TTerm" }),
    argument: wrapperType
  }) });
  return (() => {
  const unwrapBody = ({ tag: "lambda", value: ({
    parameter: "x",
    domain: unwrapDomain,
    body: ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "application",
    term: ({ tag: "record", value: ({
    typeName: "hydra.core.Application",
    fields: [({
    name: "function",
    term: ({ tag: "inject", value: ({
    typeName: "hydra.core.Term",
    field: ({
    name: "unwrap",
    term: ({ tag: "wrap", value: ({
    typeName: "hydra.core.Name",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(typeName) }) })
  }) })
  })
  }) })
  }), ({
    name: "argument",
    term: ({ tag: "application", value: ({
    function: ({ tag: "unwrap", value: "hydra.phantoms.TTerm" }),
    argument: ({ tag: "variable", value: "x" })
  }) })
  })]
  }) })
  })
  }) })
  }) })
  }) });
  return (() => {
  const wrapTs = dslTypeScheme(origType)([innerType])(wrapperType);
  return (() => {
  const unwrapTs = dslTypeScheme(origType)([wrapperType])(innerType);
  return [({
    name: wrapName,
    term: wrapBody,
    type: wrapTs
  }), ({
    name: unwrapName,
    term: unwrapBody,
    type: unwrapTs
  })];
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
})()));
}

export function isDslEligibleBinding<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Binding) => t2 | Core.Binding | null)) {
  return ((graph: t1) => ((b: Core.Binding) => (() => {
  const ns = Names.namespaceOf(((_x) => _x.name)(b));
  return LibLogic.ifElse(LibEquality.equal(LibMaybes.maybe("")(((_x) => _x))(ns))("hydra.phantoms"))(({ tag: "right", value: null }))(({ tag: "right", value: b }));
})()));
}

export function nominalResultType(typeName: Core.Name): ((x: Core.Type) => Core.Type) {
  return ((origType: Core.Type) => (() => {
  const vars = collectForallVars(origType);
  return LibLists.foldl(((acc: Core.Type) => ((v: Core.Name) => ({ tag: "application", value: ({
    function: acc,
    argument: ({ tag: "variable", value: v })
  }) }))))(({ tag: "variable", value: typeName }))(vars);
})());
}
