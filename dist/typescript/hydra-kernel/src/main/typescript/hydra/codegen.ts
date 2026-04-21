// Note: this is an automatically generated file. Do not edit.

/**
 * Pure code generation pipeline for bootstrapping Hydra across languages.
 */



import * as Adapt from "./adapt.js";
import * as Annotations from "./annotations.js";
import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as DecodePackaging from "./decode/packaging.js";
import * as Decoding from "./decoding.js";
import * as EncodeCore from "./encode/core.js";
import * as EncodePackaging from "./encode/packaging.js";
import * as Encoding from "./encoding.js";
import * as Environment from "./environment.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as Inference from "./inference.js";
import * as JsonDecode from "./json/decode.js";
import * as JsonEncode from "./json/encode.js";
import * as JsonModel from "./json/model.js";
import * as JsonWriter from "./json/writer.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
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
import * as ShowCore from "./show/core.js";
import * as ShowErrors from "./show/errors.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function buildSchemaMap(g: Graph.Graph): ReadonlyMap<Core.Name, Core.Type> {
  return LibMaps.map(((ts: Core.TypeScheme) => Strip.deannotateType(((_x) => _x.type)(ts))))(((_x) => _x.schemaTypes)(g));
}

export function decodeModuleFromJson(bsGraph: Graph.Graph): ((x: ReadonlyArray<Packaging.Module>) => ((x: JsonModel.Value) => Errors.Error | Packaging.Module)) {
  return ((universeModules: ReadonlyArray<Packaging.Module>) => ((jsonVal: JsonModel.Value) => (() => {
  const graph = modulesToGraph(bsGraph)(universeModules)(universeModules);
  return (() => {
  const schemaMap = buildSchemaMap(graph);
  return (() => {
  const modType = ({ tag: "variable", value: "hydra.packaging.Module" });
  return LibEithers.either(((err: string) => ({ tag: "left", value: ({ tag: "other", value: err }) })))(((term: Core.Term) => LibEithers.either(((decErr: Errors.DecodingError) => ({ tag: "left", value: ({ tag: "decoding", value: decErr }) })))(((mod: Packaging.Module) => ({ tag: "right", value: mod })))(DecodePackaging.module(graph)(term))))(JsonDecode.fromJson(schemaMap)("hydra.packaging.Module")(modType)(jsonVal));
})();
})();
})()));
}

export function escapeControlCharsInJson(input: ReadonlyArray<number>): ReadonlyArray<number> {
  return (() => {
  const hexDigit = ((n: number) => LibLogic.ifElse(LibEquality.lt(n)(10))(LibMath.add(48)(n))(LibMath.add(97)(LibMath.sub(n)(10))));
  return (() => {
  const escapeToUnicode = ((b: number) => [92, 117, 48, 48, hexDigit(LibMath.div(b)(16)), hexDigit(LibMath.mod(b)(16))]);
  return (() => {
  const go = ((inStr: boolean) => ((esc: boolean) => ((bytes: ReadonlyArray<number>) => LibLogic.ifElse(LibLists.null_(bytes))([])((() => {
  const b = LibLists.head(bytes);
  return (() => {
  const bs = LibLists.tail(bytes);
  return LibLogic.ifElse(esc)(LibLists.cons(b)(go(inStr)(false)(bs)))(LibLogic.ifElse(LibLogic.and(LibEquality.equal(b)(92))(inStr))(LibLists.cons(b)(go(inStr)(true)(bs)))(LibLogic.ifElse(LibEquality.equal(b)(34))(LibLists.cons(b)(go(LibLogic.not(inStr))(false)(bs)))(LibLogic.ifElse(LibLogic.and(inStr)(LibEquality.lt(b)(32)))(LibLists.concat2(escapeToUnicode(b))(go(inStr)(false)(bs)))(LibLists.cons(b)(go(inStr)(false)(bs))))));
})();
})()))));
  return go(false)(false)(input);
})();
})();
})();
}

export function formatPrimitive(prim: Graph.Primitive): string {
  return (() => {
  const name = ((_x) => _x)(((_x) => _x.name)(prim));
  return (() => {
  const typeStr = ShowCore.typeScheme(((_x) => _x.type)(prim));
  return LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("  ")(name))(" : "))(typeStr);
})();
})();
}

export function formatTermBinding(binding: Core.Binding): string {
  return (() => {
  const name = ((_x) => _x)(((_x) => _x.name)(binding));
  return (() => {
  const typeStr = LibMaybes.maybe("?")(((scheme: Core.TypeScheme) => ShowCore.typeScheme(scheme)))(((_x) => _x.type)(binding));
  return LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("  ")(name))(" : "))(typeStr);
})();
})();
}

export function formatTypeBinding(graph: Graph.Graph): ((x: Core.Binding) => Errors.Error | string) {
  return ((binding: Core.Binding) => LibEithers.bind(LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((_a: Core.Type) => _a))(DecodeCore.type(graph)(((_x) => _x.term)(binding))))(((typ: Core.Type) => ({ tag: "right", value: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("  ")(((_x) => _x)(((_x) => _x.name)(binding))))(" = "))(ShowCore.type(typ)) }))));
}

export function generateCoderModules<t0, t1, t2, t3>(codec: ((x: t0) => ((x: Graph.Graph) => ((x: t1) => t2 | t3 | null)))): ((x: Graph.Graph) => ((x: ReadonlyArray<Packaging.Module>) => ((x: ReadonlyArray<t1>) => ((x: t0) => t2 | ReadonlyArray<t3>)))) {
  return ((bsGraph: Graph.Graph) => ((universeModules: ReadonlyArray<Packaging.Module>) => ((typeModules: ReadonlyArray<t1>) => ((cx: t0) => (() => {
  const universe = LibMaps.fromList(LibLists.map(((m: Packaging.Module) => [((_x) => _x.namespace)(m), m]))(LibLists.concat2(universeModules)(universeModules)));
  return (() => {
  const schemaModules = moduleTypeDepsTransitive(universe)(universeModules);
  return (() => {
  const dataModules = moduleTermDepsTransitive(universe)(universeModules);
  return (() => {
  const schemaElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
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
})()))(((_x) => _x.definitions)(m)))))(LibLists.concat2(schemaModules)(universeModules)));
  return (() => {
  const dataElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(m)))))(dataModules));
  return (() => {
  const schemaGraph = Lexical.elementsToGraph(bsGraph)(LibMaps.empty)(schemaElements);
  return (() => {
  const schemaTypes = LibEithers.either(((_: Errors.Error) => LibMaps.empty))(((_r: ReadonlyMap<Core.Name, Core.TypeScheme>) => _r))(Environment.schemaGraphToTypingEnvironment(schemaGraph));
  return (() => {
  const allElements = LibLists.concat2(schemaElements)(dataElements);
  return (() => {
  const graph = Lexical.elementsToGraph(bsGraph)(schemaTypes)(allElements);
  return LibEithers.map(((results: ReadonlyArray<t3 | null>) => LibMaybes.cat(results)))(LibEithers.mapList(((m: t1) => codec(cx)(graph)(m)))(typeModules));
})();
})();
})();
})();
})();
})();
})();
})();
})()))));
}

export function generateLexicon(graph: Graph.Graph): Errors.Error | string {
  return (() => {
  const bindings = Lexical.graphToBindings(graph);
  return (() => {
  const primitives = LibMaps.elems(((_x) => _x.primitives)(graph));
  return (() => {
  const partitioned = LibLists.partition(((b: Core.Binding) => Annotations.isNativeType(b)))(bindings);
  return (() => {
  const typeBindings = LibPairs.first(partitioned);
  return (() => {
  const termBindings = LibPairs.second(partitioned);
  return (() => {
  const sortedPrimitives = LibLists.sortOn(((p: Graph.Primitive) => ((_x) => _x.name)(p)))(primitives);
  return (() => {
  const sortedTypes = LibLists.sortOn(((b: Core.Binding) => ((_x) => _x.name)(b)))(typeBindings);
  return (() => {
  const sortedTerms = LibLists.sortOn(((b: Core.Binding) => ((_x) => _x.name)(b)))(termBindings);
  return LibEithers.bind(LibEithers.mapList(((b: Core.Binding) => formatTypeBinding(graph)(b)))(sortedTypes))(((typeLines: ReadonlyArray<string>) => (() => {
  const termLines = LibLists.map(((b: Core.Binding) => formatTermBinding(b)))(sortedTerms);
  return (() => {
  const primitiveLines = LibLists.map(((p: Graph.Primitive) => formatPrimitive(p)))(sortedPrimitives);
  return ({ tag: "right", value: LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("Primitives:\n")(LibStrings.unlines(primitiveLines)))("\nTypes:\n"))(LibStrings.unlines(typeLines)))("\nTerms:\n"))(LibStrings.unlines(termLines)) });
})();
})()));
})();
})();
})();
})();
})();
})();
})();
})();
}

export function generateSourceFiles<t0, t1>(printDefinitions: ((x: Packaging.Module) => ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyMap<t0, t1>))))): ((x: Coders.Language) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: Graph.Graph) => ((x: ReadonlyArray<Packaging.Module>) => ((x: ReadonlyArray<Packaging.Module>) => ((x: Context.Context) => Errors.Error | ReadonlyArray<readonly [t0, t1]>))))))))) {
  return ((lang: Coders.Language) => ((doInfer: boolean) => ((doExpand: boolean) => ((doHoistCaseStatements: boolean) => ((doHoistPolymorphicLetBindings: boolean) => ((bsGraph: Graph.Graph) => ((universeModules: ReadonlyArray<Packaging.Module>) => ((modsToGenerate: ReadonlyArray<Packaging.Module>) => ((cx: Context.Context) => (() => {
  const namespaceMap = LibMaps.fromList(LibLists.map(((m: Packaging.Module) => [((_x) => _x.namespace)(m), m]))(LibLists.concat2(universeModules)(modsToGenerate)));
  return (() => {
  const constraints = ((_x) => _x.constraints)(lang);
  return (() => {
  const typeModulesToGenerate = LibLists.filter(((mod: Packaging.Module) => LibLogic.not(LibLists.null_(LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
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
})()))(((_x) => _x.definitions)(mod)))))))(modsToGenerate);
  return (() => {
  const termModulesToGenerate = LibLists.filter(((mod: Packaging.Module) => LibLogic.not(LibLists.null_(LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(mod)))))))(modsToGenerate);
  return (() => {
  const schemaMods = moduleTypeDepsTransitive(namespaceMap)(modsToGenerate);
  return (() => {
  const schemaElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
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
})()))(((_x) => _x.definitions)(m)))))(LibLists.concat2(schemaMods)(typeModulesToGenerate)));
  return (() => {
  const dataMods = moduleTermDepsTransitive(namespaceMap)(modsToGenerate);
  return (() => {
  const dataElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(m)))))(dataMods));
  return (() => {
  const schemaGraph = Lexical.elementsToGraph(bsGraph)(LibMaps.empty)(schemaElements);
  return (() => {
  const schemaTypes2 = LibEithers.either(((_: Errors.Error) => LibMaps.empty))(((_r: ReadonlyMap<Core.Name, Core.TypeScheme>) => _r))(Environment.schemaGraphToTypingEnvironment(schemaGraph));
  return (() => {
  const dataGraph = Lexical.elementsToGraph(bsGraph)(schemaTypes2)(dataElements);
  return LibEithers.bind(LibLogic.ifElse(LibLists.null_(typeModulesToGenerate))(({ tag: "right", value: [] }))((() => {
  const nameLists = LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => ((_x) => _x.name)(td))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(m)))))(typeModulesToGenerate);
  return LibEithers.bind(Adapt.schemaGraphToDefinitions(constraints)(schemaGraph)(nameLists)(cx))(((schemaResult: readonly [ReadonlyMap<Core.Name, Core.Type>, ReadonlyArray<ReadonlyArray<Packaging.TypeDefinition>>]) => (() => {
  const defLists = LibPairs.second(schemaResult);
  return (() => {
  const schemaGraphWithTypes = ({
    boundTerms: ((_x) => _x.boundTerms)(schemaGraph),
    boundTypes: ((_x) => _x.boundTypes)(schemaGraph),
    classConstraints: ((_x) => _x.classConstraints)(schemaGraph),
    lambdaVariables: ((_x) => _x.lambdaVariables)(schemaGraph),
    metadata: ((_x) => _x.metadata)(schemaGraph),
    primitives: ((_x) => _x.primitives)(schemaGraph),
    schemaTypes: schemaTypes2,
    typeVariables: ((_x) => _x.typeVariables)(schemaGraph)
  });
  return LibEithers.map(((xs: ReadonlyArray<ReadonlyArray<readonly [t0, t1]>>) => LibLists.concat(xs)))(LibEithers.mapList(((p: readonly [Packaging.Module, ReadonlyArray<Packaging.TypeDefinition>]) => (() => {
  const mod = LibPairs.first(p);
  return (() => {
  const defs = LibPairs.second(p);
  return LibEithers.map(((m: ReadonlyMap<t0, t1>) => LibMaps.toList(m)))(printDefinitions(mod)(LibLists.map(((d: Packaging.TypeDefinition) => ({ tag: "type", value: d })))(defs))(cx)(schemaGraphWithTypes));
})();
})()))(LibLists.zip(typeModulesToGenerate)(defLists)));
})();
})()));
})()))(((schemaFiles: ReadonlyArray<readonly [t0, t1]>) => LibEithers.bind(LibLogic.ifElse(LibLists.null_(termModulesToGenerate))(({ tag: "right", value: [] }))((() => {
  const namespaces = LibLists.map(((m: Packaging.Module) => ((_x) => _x.namespace)(m)))(termModulesToGenerate);
  return LibEithers.bind(Adapt.dataGraphToDefinitions(constraints)(doInfer)(doExpand)(doHoistCaseStatements)(doHoistPolymorphicLetBindings)(dataElements)(dataGraph)(namespaces)(cx))(((dataResult: readonly [Graph.Graph, ReadonlyArray<ReadonlyArray<Packaging.TermDefinition>>]) => (() => {
  const g1 = LibPairs.first(dataResult);
  return (() => {
  const defLists = LibPairs.second(dataResult);
  return (() => {
  const defName = ((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ((_x) => _x.name)(td))((_m as any).value);
    case "type": return ((td: Packaging.TypeDefinition) => ((_x) => _x.name)(td))((_m as any).value);
  }
})());
  return (() => {
  const refreshModule = ((els: ReadonlyArray<Core.Binding>) => ((m: Packaging.Module) => ({
    namespace: ((_x) => _x.namespace)(m),
    definitions: LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => ({ tag: "type", value: td }))((_m as any).value);
    case "term": return ((td: Packaging.TermDefinition) => LibMaybes.map(((b: Core.Binding) => ({ tag: "term", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }) })))(LibLists.find(((b: Core.Binding) => LibEquality.equal(((_x) => _x.name)(b))(((_x) => _x.name)(td))))(els)))((_m as any).value);
  }
})()))(((_x) => _x.definitions)(m))),
    termDependencies: ((_x) => _x.termDependencies)(m),
    typeDependencies: ((_x) => _x.typeDependencies)(m),
    description: ((_x) => _x.description)(m)
  })));
  return (() => {
  const allBindings = Lexical.graphToBindings(g1);
  return (() => {
  const refreshedMods = LibLists.map(((m: Packaging.Module) => refreshModule(allBindings)(m)))(termModulesToGenerate);
  return (() => {
  const dedupDefs = ((defs: ReadonlyArray<Packaging.TermDefinition>) => LibMaps.elems(LibMaps.fromList(LibLists.map(((d: Packaging.TermDefinition) => [((_x) => _x.name)(d), d]))(defs))));
  return (() => {
  const dedupedDefLists = LibLists.map(dedupDefs)(defLists);
  return LibEithers.map(((xs: ReadonlyArray<ReadonlyArray<readonly [t0, t1]>>) => LibLists.concat(xs)))(LibEithers.mapList(((p: readonly [Packaging.Module, ReadonlyArray<Packaging.TermDefinition>]) => (() => {
  const mod = LibPairs.first(p);
  return (() => {
  const defs = LibPairs.second(p);
  return LibEithers.map(((m: ReadonlyMap<t0, t1>) => LibMaps.toList(m)))(printDefinitions(mod)(LibLists.map(((d: Packaging.TermDefinition) => ({ tag: "term", value: d })))(defs))(cx)(g1));
})();
})()))(LibLists.zip(refreshedMods)(dedupedDefLists)));
})();
})();
})();
})();
})();
})();
})();
})()));
})()))(((termFiles: ReadonlyArray<readonly [t0, t1]>) => ({ tag: "right", value: LibLists.concat2(schemaFiles)(termFiles) })))));
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
})())))))))));
}

export function inferAndGenerateLexicon(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Packaging.Module>) => Errors.Error | string)) {
  return ((bsGraph: Graph.Graph) => ((kernelModules: ReadonlyArray<Packaging.Module>) => (() => {
  const g0 = modulesToGraph(bsGraph)(kernelModules)(kernelModules);
  return (() => {
  const dataElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(m)))))(kernelModules));
  return LibEithers.bind(Inference.inferGraphTypes(cx)(dataElements)(g0))(((inferResultWithCx: readonly [readonly [Graph.Graph, ReadonlyArray<Core.Binding>], Context.Context]) => (() => {
  const g1 = LibPairs.first(LibPairs.first(inferResultWithCx));
  return generateLexicon(g1);
})()));
})();
})()));
}

export function inferModules(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Packaging.Module>) => ((x: ReadonlyArray<Packaging.Module>) => Errors.Error | ReadonlyArray<Packaging.Module>))) {
  return ((bsGraph: Graph.Graph) => ((universeMods: ReadonlyArray<Packaging.Module>) => ((targetMods: ReadonlyArray<Packaging.Module>) => (() => {
  const g0 = modulesToGraph(bsGraph)(universeMods)(universeMods);
  return (() => {
  const dataElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(m)))))(universeMods));
  return LibEithers.bind(Inference.inferGraphTypes(cx)(dataElements)(g0))(((inferResultWithCx: readonly [readonly [Graph.Graph, ReadonlyArray<Core.Binding>], Context.Context]) => (() => {
  const inferResult = LibPairs.first(inferResultWithCx);
  return (() => {
  const g1 = LibPairs.first(inferResult);
  return (() => {
  const inferredElements = LibPairs.second(inferResult);
  return (() => {
  const isTypeOnlyModule = ((mod: Packaging.Module) => LibLogic.not(LibLogic.not(LibLists.null_(LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(mod)))))));
  return (() => {
  const defName = ((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ((_x) => _x.name)(td))((_m as any).value);
    case "type": return ((td: Packaging.TypeDefinition) => ((_x) => _x.name)(td))((_m as any).value);
  }
})());
  return (() => {
  const refreshModule = ((m: Packaging.Module) => LibLogic.ifElse(isTypeOnlyModule(m))(m)(({
    namespace: ((_x) => _x.namespace)(m),
    definitions: LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => ({ tag: "type", value: td }))((_m as any).value);
    case "term": return ((td: Packaging.TermDefinition) => LibMaybes.map(((b: Core.Binding) => ({ tag: "term", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }) })))(LibLists.find(((b: Core.Binding) => LibEquality.equal(((_x) => _x.name)(b))(((_x) => _x.name)(td))))(inferredElements)))((_m as any).value);
  }
})()))(((_x) => _x.definitions)(m))),
    termDependencies: ((_x) => _x.termDependencies)(m),
    typeDependencies: ((_x) => _x.typeDependencies)(m),
    description: ((_x) => _x.description)(m)
  })));
  return ({ tag: "right", value: LibLists.map(refreshModule)(targetMods) });
})();
})();
})();
})();
})();
})()));
})();
})())));
}

export function moduleTermDepsTransitive(nsMap: ReadonlyMap<Packaging.Namespace, Packaging.Module>): ((x: ReadonlyArray<Packaging.Module>) => ReadonlyArray<Packaging.Module>) {
  return ((modules: ReadonlyArray<Packaging.Module>) => (() => {
  const closure = LibSets.union(transitiveDeps(((m: Packaging.Module) => ((_x) => _x.termDependencies)(m)))(nsMap)(modules))(LibSets.fromList(LibLists.map(((m: Packaging.Module) => ((_x) => _x.namespace)(m)))(modules)));
  return LibMaybes.cat(LibLists.map(((n: Packaging.Namespace) => LibMaps.lookup(n)(nsMap)))(LibSets.toList(closure)));
})());
}

export function moduleToJson(schemaMap: ReadonlyMap<Core.Name, Core.Type>): ((x: Packaging.Module) => Errors.Error | string) {
  return ((m: Packaging.Module) => (() => {
  const term = EncodePackaging.module(m);
  return (() => {
  const modType = ({ tag: "variable", value: "hydra.packaging.Module" });
  return LibEithers.map(((json: JsonModel.Value) => JsonWriter.printJson(json)))(LibEithers.bimap(((_e: string) => ({ tag: "other", value: _e })))(((_a: JsonModel.Value) => _a))(JsonEncode.toJson(schemaMap)("hydra.packaging.Module")(modType)(term)));
})();
})());
}

export function moduleToSourceModule(m: Packaging.Module): Packaging.Module {
  return (() => {
  const sourceNs = LibStrings.cat2("hydra.sources.")(LibStrings.intercalate(".")(LibLists.drop(1)(LibStrings.splitOn(".")(((_x) => _x)(((_x) => _x.namespace)(m))))));
  return (() => {
  const modTypeNs = "hydra.packaging";
  return (() => {
  const moduleDef = ({ tag: "term", value: ({
    name: LibStrings.cat2(((_x) => _x)(sourceNs))(".module_"),
    term: EncodePackaging.module(m),
    type: null
  }) });
  return ({
    namespace: sourceNs,
    definitions: [moduleDef],
    termDependencies: [modTypeNs],
    typeDependencies: [modTypeNs],
    description: LibStrings.cat2("Source module for ")(((_x) => _x)(((_x) => _x.namespace)(m)))
  });
})();
})();
})();
}

export function moduleTypeDepsTransitive(nsMap: ReadonlyMap<Packaging.Namespace, Packaging.Module>): ((x: ReadonlyArray<Packaging.Module>) => ReadonlyArray<Packaging.Module>) {
  return ((modules: ReadonlyArray<Packaging.Module>) => (() => {
  const termMods = moduleTermDepsTransitive(nsMap)(modules);
  return (() => {
  const typeNamespaces = LibSets.toList(transitiveDeps(((m: Packaging.Module) => ((_x) => _x.typeDependencies)(m)))(nsMap)(termMods));
  return LibMaybes.cat(LibLists.map(((n: Packaging.Namespace) => LibMaps.lookup(n)(nsMap)))(typeNamespaces));
})();
})());
}

export function modulesToGraph(bsGraph: Graph.Graph): ((x: ReadonlyArray<Packaging.Module>) => ((x: ReadonlyArray<Packaging.Module>) => Graph.Graph)) {
  return ((universeModules: ReadonlyArray<Packaging.Module>) => ((modules: ReadonlyArray<Packaging.Module>) => (() => {
  const universe = LibMaps.fromList(LibLists.map(((m: Packaging.Module) => [((_x) => _x.namespace)(m), m]))(LibLists.concat2(universeModules)(modules)));
  return (() => {
  const schemaModules = moduleTypeDepsTransitive(universe)(modules);
  return (() => {
  const dataModules = moduleTermDepsTransitive(universe)(modules);
  return (() => {
  const schemaElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
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
})()))(((_x) => _x.definitions)(m)))))(LibLists.concat2(schemaModules)(modules)));
  return (() => {
  const dataElements = LibLists.concat(LibLists.map(((m: Packaging.Module) => LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    default: return null(_m);
  }
})()))(((_x) => _x.definitions)(m)))))(dataModules));
  return (() => {
  const schemaGraph = Lexical.elementsToGraph(bsGraph)(LibMaps.empty)(schemaElements);
  return (() => {
  const schemaTypes = LibEithers.either(((_: Errors.Error) => LibMaps.empty))(((_r: ReadonlyMap<Core.Name, Core.TypeScheme>) => _r))(Environment.schemaGraphToTypingEnvironment(schemaGraph));
  return Lexical.elementsToGraph(bsGraph)(schemaTypes)(dataElements);
})();
})();
})();
})();
})();
})();
})()));
}

export function namespaceToPath(ns: Packaging.Namespace): string {
  return LibStrings.intercalate("/")(LibStrings.splitOn(".")(((_x) => _x)(ns)));
}

export function transitiveDeps(getDeps: ((x: Packaging.Module) => ReadonlyArray<Packaging.Namespace>)): ((x: ReadonlyMap<Packaging.Namespace, Packaging.Module>) => ((x: ReadonlyArray<Packaging.Module>) => ReadonlySet<Packaging.Namespace>)) {
  return ((nsMap: ReadonlyMap<Packaging.Namespace, Packaging.Module>) => ((startMods: ReadonlyArray<Packaging.Module>) => (() => {
  const initialDeps = LibSets.fromList(LibLists.concat(LibLists.map(((m: Packaging.Module) => LibLists.filter(((dep: Packaging.Namespace) => LibLogic.not(LibEquality.equal(dep)(((_x) => _x.namespace)(m)))))(getDeps(m))))(startMods)));
  return (() => {
  const go = ((pending: ReadonlySet<Packaging.Namespace>) => ((visited: ReadonlySet<Packaging.Namespace>) => LibLogic.ifElse(LibSets.null_(pending))(visited)((() => {
  const newVisited = LibSets.union(visited)(pending);
  return (() => {
  const nextDeps = LibSets.fromList(LibLists.concat(LibLists.map(((nsv: Packaging.Namespace) => LibMaybes.maybe([])(((depMod: Packaging.Module) => getDeps(depMod)))(LibMaps.lookup(nsv)(nsMap))))(LibSets.toList(pending))));
  return (() => {
  const newPending = LibSets.difference(nextDeps)(newVisited);
  return go(newPending)(newVisited);
})();
})();
})())));
  return go(initialDeps)(LibSets.empty);
})();
})()));
}
