// Note: this is an automatically generated file. Do not edit.

/**
 * Simple, one-way adapters for types and terms
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as Dependencies from "./dependencies.js";
import * as Environment from "./environment.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as Hoisting from "./hoisting.js";
import * as Inference from "./inference.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Literals from "./literals.js";
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Reduction from "./reduction.js";
import * as Reflect from "./reflect.js";
import * as Relational from "./relational.js";
import * as Resolution from "./resolution.js";
import * as Rewriting from "./rewriting.js";
import * as Scoping from "./scoping.js";
import * as ShowCore from "./show/core.js";
import * as ShowErrors from "./show/errors.js";
import * as ShowGraph from "./show/graph.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function adaptDataGraph<t0>(constraints: Coders.LanguageConstraints): ((x: boolean) => ((x: ReadonlyArray<Core.Binding>) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | readonly [Graph.Graph, ReadonlyArray<Core.Binding>])))) {
  return ((doExpand: boolean) => ((els0: ReadonlyArray<Core.Binding>) => ((cx: t0) => ((graph0: Graph.Graph) => (() => {
  const transformTerm = ((g: Graph.Graph) => ((term: Core.Term) => (() => {
  const tx = g;
  return (() => {
  const t1 = Variables.unshadowVariables(pushTypeAppsInward(term));
  return (() => {
  const t2 = Variables.unshadowVariables(LibLogic.ifElse(doExpand)(pushTypeAppsInward(Reduction.etaExpandTerm(tx)(t1)))(t1));
  return Dependencies.liftLambdaAboveLet(t2);
})();
})();
})()));
  return (() => {
  const transformBinding = ((g: Graph.Graph) => ((el: Core.Binding) => ({
    name: ((_x) => _x.name)(el),
    term: transformTerm(g)(((_x) => _x.term)(el)),
    type: ((_x) => _x.type)(el)
  })));
  return (() => {
  const litmap = adaptLiteralTypesMap(constraints);
  return (() => {
  const prims0 = ((_x) => _x.primitives)(graph0);
  return (() => {
  const schemaTypes0 = ((_x) => _x.schemaTypes)(graph0);
  return (() => {
  const schemaBindings = Environment.typesToDefinitions(LibMaps.map(((ts: Core.TypeScheme) => Scoping.typeSchemeToFType(ts)))(schemaTypes0));
  return LibEithers.bind(LibLogic.ifElse(LibMaps.null_(schemaTypes0))(({ tag: "right", value: LibMaps.empty }))(LibEithers.bind(LibEithers.bimap(((e: Errors.DecodingError) => ({ tag: "decoding", value: e })))(((x: ReadonlyMap<Core.Name, Core.Type>) => x))(Environment.graphAsTypes(graph0)(schemaBindings)))(((tmap0: ReadonlyMap<Core.Name, Core.Type>) => LibEithers.bind(adaptGraphSchema(constraints)(litmap)(tmap0))(((tmap1: ReadonlyMap<Core.Name, Core.Type>) => ({ tag: "right", value: LibMaps.map(((t: Core.Type) => Resolution.typeToTypeScheme(t)))(tmap1) })))))))(((schemaResult: ReadonlyMap<Core.Name, Core.TypeScheme>) => (() => {
  const adaptedSchemaTypes = schemaResult;
  return (() => {
  const adaptBinding = ((el: Core.Binding) => (() => {
  const transformed = transformBinding(graph0)(el);
  return (() => {
  const wrapped = ({ tag: "let", value: ({
    bindings: LibLists.pure(transformed),
    body: ({ tag: "unit" })
  }) });
  return LibEithers.bind(adaptTerm(constraints)(litmap)(cx)(graph0)(wrapped))(((adapted: Core.Term) => Rewriting.rewriteTermM(((v1: ((x: Core.Term) => Errors.Error | Core.Term)) => ((v2: Core.Term) => adaptLambdaDomains(constraints)(litmap)(v1)(v2))))(adapted)));
})();
})());
  return LibEithers.bind(LibEithers.mapList(adaptBinding)(els0))(((adaptedTerms: ReadonlyArray<Core.Term>) => (() => {
  const els1Raw = LibLists.concat(LibLists.map(Environment.termAsBindings)(adaptedTerms));
  return (() => {
  const processBinding = ((el: Core.Binding) => LibEithers.bind(Rewriting.rewriteTermM(((v1: ((x: Core.Term) => Errors.Error | Core.Term)) => ((v2: Core.Term) => adaptNestedTypes(constraints)(litmap)(v1)(v2))))(((_x) => _x.term)(el)))(((newTerm: Core.Term) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((ts: Core.TypeScheme) => LibEithers.bind(adaptTypeScheme(constraints)(litmap)(ts))(((ts1: Core.TypeScheme) => ({ tag: "right", value: ts1 })))))(((_x) => _x.type)(el)))(((adaptedType: Core.TypeScheme | null) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(el),
    term: newTerm,
    type: adaptedType
  }) }))))));
  return LibEithers.bind(LibEithers.mapList(processBinding)(els1Raw))(((els1: ReadonlyArray<Core.Binding>) => LibEithers.bind(LibEithers.mapList(((kv: readonly [Core.Name, Graph.Primitive]) => LibEithers.bind(adaptPrimitive(constraints)(litmap)(LibPairs.second(kv)))(((prim1: Graph.Primitive) => ({ tag: "right", value: [LibPairs.first(kv), prim1] })))))(LibMaps.toList(prims0)))(((primPairs: ReadonlyArray<readonly [Core.Name, Graph.Primitive]>) => (() => {
  const prims1 = LibMaps.fromList(primPairs);
  return (() => {
  const adaptedGraphRaw = Lexical.buildGraph(els1)(LibMaps.empty)(prims1);
  return (() => {
  const adaptedGraph = ({
    boundTerms: ((_x) => _x.boundTerms)(adaptedGraphRaw),
    boundTypes: ((_x) => _x.boundTypes)(adaptedGraphRaw),
    classConstraints: ((_x) => _x.classConstraints)(adaptedGraphRaw),
    lambdaVariables: ((_x) => _x.lambdaVariables)(adaptedGraphRaw),
    metadata: ((_x) => _x.metadata)(adaptedGraphRaw),
    primitives: ((_x) => _x.primitives)(adaptedGraphRaw),
    schemaTypes: adaptedSchemaTypes,
    typeVariables: ((_x) => _x.typeVariables)(adaptedGraphRaw)
  });
  return ({ tag: "right", value: [adaptedGraph, els1] });
})();
})();
})()))));
})();
})()));
})();
})()));
})();
})();
})();
})();
})();
})()))));
}

export function adaptFloatType(constraints: Coders.LanguageConstraints): ((x: Core.FloatType) => Core.FloatType | null) {
  return ((ft: Core.FloatType) => (() => {
  const supported = LibSets.member(ft)(((_x) => _x.floatTypes)(constraints));
  return (() => {
  const alt = ((v1: Core.FloatType) => adaptFloatType(constraints)(v1));
  return (() => {
  const forUnsupported = ((ft2: Core.FloatType) => (() => {
  const _m = ft2;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => alt(({ tag: "float64" })))((_m as any).value);
    case "float32": return ((_: void) => alt(({ tag: "float64" })))((_m as any).value);
    case "float64": return ((_: void) => alt(({ tag: "bigfloat" })))((_m as any).value);
  }
})());
  return LibLogic.ifElse(supported)(ft)(forUnsupported(ft));
})();
})();
})());
}

export function adaptGraphSchema<t0>(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: ReadonlyMap<t0, Core.Type>) => Errors.Error | ReadonlyMap<t0, Core.Type>)) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((types0: ReadonlyMap<t0, Core.Type>) => (() => {
  const mapPair = ((pair: readonly [t1, Core.Type]) => (() => {
  const name = LibPairs.first(pair);
  return (() => {
  const typ = LibPairs.second(pair);
  return LibEithers.bind(adaptType(constraints)(litmap)(typ))(((typ1: Core.Type) => ({ tag: "right", value: [name, typ1] })));
})();
})());
  return LibEithers.bind(LibEithers.mapList(mapPair)(LibMaps.toList(types0)))(((pairs: ReadonlyArray<readonly [t0, Core.Type]>) => ({ tag: "right", value: LibMaps.fromList(pairs) })));
})()));
}

export function adaptIntegerType(constraints: Coders.LanguageConstraints): ((x: Core.IntegerType) => Core.IntegerType | null) {
  return ((it: Core.IntegerType) => (() => {
  const supported = LibSets.member(it)(((_x) => _x.integerTypes)(constraints));
  return (() => {
  const alt = ((v1: Core.IntegerType) => adaptIntegerType(constraints)(v1));
  return (() => {
  const forUnsupported = ((it2: Core.IntegerType) => (() => {
  const _m = it2;
  switch (_m.tag) {
    case "bigint": return ((_: void) => null)((_m as any).value);
    case "int8": return ((_: void) => alt(({ tag: "uint16" })))((_m as any).value);
    case "int16": return ((_: void) => alt(({ tag: "uint32" })))((_m as any).value);
    case "int32": return ((_: void) => alt(({ tag: "uint64" })))((_m as any).value);
    case "int64": return ((_: void) => alt(({ tag: "bigint" })))((_m as any).value);
    case "uint8": return ((_: void) => alt(({ tag: "int16" })))((_m as any).value);
    case "uint16": return ((_: void) => alt(({ tag: "int32" })))((_m as any).value);
    case "uint32": return ((_: void) => alt(({ tag: "int64" })))((_m as any).value);
    case "uint64": return ((_: void) => alt(({ tag: "bigint" })))((_m as any).value);
  }
})());
  return LibLogic.ifElse(supported)(it)(forUnsupported(it));
})();
})();
})());
}

export function adaptLambdaDomains<t0>(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: ((x: t0) => Errors.Error | Core.Term)) => ((x: t0) => Errors.Error | Core.Term))) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((recurse: ((x: t0) => Errors.Error | Core.Term)) => ((term: t0) => LibEithers.bind(recurse(term))(((rewritten: Core.Term) => (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((dom: Core.Type) => LibEithers.bind(adaptType(constraints)(litmap)(dom))(((dom1: Core.Type) => ({ tag: "right", value: dom1 })))))(((_x) => _x.domain)(l)))(((adaptedDomain: Core.Type | null) => ({ tag: "right", value: ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: adaptedDomain,
    body: ((_x) => _x.body)(l)
  }) }) }))))((_m as any).value);
    default: return ({ tag: "right", value: rewritten })(_m);
  }
})())))));
}

export function adaptLiteral(lt: Core.LiteralType): ((x: Core.Literal) => Core.Literal) {
  return ((l: Core.Literal) => (() => {
  const _m = l;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "string": return ((_: void) => ({ tag: "string", value: LibLiterals.binaryToString(b) }))((_m as any).value);
  }
})())((_m as any).value);
    case "boolean": return ((b: boolean) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "integer": return ((it: Core.IntegerType) => ({ tag: "integer", value: Literals.bigintToIntegerValue(it)(LibLogic.ifElse(b)(1n)(0n)) }))((_m as any).value);
  }
})())((_m as any).value);
    case "float": return ((f: Core.FloatValue) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "float": return ((ft: Core.FloatType) => ({ tag: "float", value: Literals.bigfloatToFloatValue(ft)(Literals.floatValueToBigfloat(f)) }))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((i: Core.IntegerValue) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "integer": return ((it: Core.IntegerType) => ({ tag: "integer", value: Literals.bigintToIntegerValue(it)(Literals.integerValueToBigint(i)) }))((_m as any).value);
  }
})())((_m as any).value);
  }
})());
}

export function adaptLiteralType(constraints: Coders.LanguageConstraints): ((x: Core.LiteralType) => Core.LiteralType | null) {
  return ((lt: Core.LiteralType) => (() => {
  const forUnsupported = ((lt2: Core.LiteralType) => (() => {
  const _m = lt2;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "string" }))((_m as any).value);
    case "boolean": return ((_: void) => LibMaybes.map(((x: Core.IntegerType) => ({ tag: "integer", value: x })))(adaptIntegerType(constraints)(({ tag: "int8" }))))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => LibMaybes.map(((x: Core.FloatType) => ({ tag: "float", value: x })))(adaptFloatType(constraints)(ft)))((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => LibMaybes.map(((x: Core.IntegerType) => ({ tag: "integer", value: x })))(adaptIntegerType(constraints)(it)))((_m as any).value);
    default: return null(_m);
  }
})());
  return LibLogic.ifElse(literalTypeSupported(constraints)(lt))(null)(forUnsupported(lt));
})());
}

export function adaptLiteralTypesMap(constraints: Coders.LanguageConstraints): ReadonlyMap<Core.LiteralType, Core.LiteralType> {
  return (() => {
  const tryType = ((lt: Core.LiteralType) => LibMaybes.maybe(null)(((lt2: Core.LiteralType) => [lt, lt2]))(adaptLiteralType(constraints)(lt)));
  return LibMaps.fromList(LibMaybes.cat(LibLists.map(tryType)(Reflect.literalTypes)));
})();
}

export function adaptLiteralValue<t0>(litmap: ReadonlyMap<t0, Core.LiteralType>): ((x: t0) => ((x: Core.Literal) => Core.Literal)) {
  return ((lt: t0) => ((l: Core.Literal) => LibMaybes.maybe(({ tag: "string", value: ShowCore.literal(l) }))(((lt2: Core.LiteralType) => adaptLiteral(lt2)(l)))(LibMaps.lookup(lt)(litmap))));
}

export function adaptNestedTypes<t0>(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: ((x: t0) => Errors.Error | Core.Term)) => ((x: t0) => Errors.Error | Core.Term))) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((recurse: ((x: t0) => Errors.Error | Core.Term)) => ((term: t0) => LibEithers.bind(recurse(term))(((rewritten: Core.Term) => (() => {
  const _m = rewritten;
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => (() => {
  const adaptB = ((b: Core.Binding) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((ts: Core.TypeScheme) => LibEithers.bind(adaptTypeScheme(constraints)(litmap)(ts))(((ts1: Core.TypeScheme) => ({ tag: "right", value: ts1 })))))(((_x) => _x.type)(b)))(((adaptedBType: Core.TypeScheme | null) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.term)(b),
    type: adaptedBType
  }) }))));
  return LibEithers.bind(LibEithers.mapList(adaptB)(((_x) => _x.bindings)(lt)))(((adaptedBindings: ReadonlyArray<Core.Binding>) => ({ tag: "right", value: ({ tag: "let", value: ({
    bindings: adaptedBindings,
    body: ((_x) => _x.body)(lt)
  }) }) })));
})())((_m as any).value);
    default: return ({ tag: "right", value: rewritten })(_m);
  }
})())))));
}

export function adaptPrimitive(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: Graph.Primitive) => Errors.Error | Graph.Primitive)) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((prim0: Graph.Primitive) => (() => {
  const ts0 = ((_x) => _x.type)(prim0);
  return LibEithers.bind(adaptTypeScheme(constraints)(litmap)(ts0))(((ts1: Core.TypeScheme) => ({ tag: "right", value: ({
    name: ((_x) => _x.name)(prim0),
    type: ts1,
    implementation: ((_x) => _x.implementation)(prim0)
  }) })));
})()));
}

export function adaptTerm<t0>(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: t0) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)))) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((cx: t0) => ((graph: Graph.Graph) => ((term0: Core.Term) => (() => {
  const rewrite = ((recurse: ((x: t1) => Errors.Error | Core.Term)) => ((term02: t1) => (() => {
  const forSupported = ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((l: Core.Literal) => (() => {
  const lt = Reflect.literalType(l);
  return ({ tag: "right", value: LibLogic.ifElse(literalTypeSupported(constraints)(lt))(term)(({ tag: "literal", value: adaptLiteralValue(litmap)(lt)(l) })) });
})())((_m as any).value);
    default: return ({ tag: "right", value: term })(_m);
  }
})());
  const forUnsupported = ((term: Core.Term) => (() => {
  const forNonNull = ((alts: ReadonlyArray<Core.Term>) => LibEithers.bind(tryTerm(LibLists.head(alts)))(((mterm: Core.Term | null) => LibMaybes.maybe(tryAlts(LibLists.tail(alts)))(((t: Core.Term) => ({ tag: "right", value: t })))(mterm))));
  const tryAlts = ((alts: ReadonlyArray<Core.Term>) => LibLogic.ifElse(LibLists.null_(alts))(({ tag: "right", value: null }))(forNonNull(alts)));
  return LibEithers.bind(termAlternatives(cx)(graph)(term))(((alts0: ReadonlyArray<Core.Term>) => tryAlts(alts0)));
})());
  const tryTerm = ((term: Core.Term) => (() => {
  const supportedVariant = LibSets.member(Reflect.termVariant(term))(((_x) => _x.termVariants)(constraints));
  return LibLogic.ifElse(supportedVariant)(forSupported(term))(forUnsupported(term));
})());
  return LibEithers.bind(recurse(term02))(((term1: Core.Term) => (() => {
  const _m = term1;
  switch (_m.tag) {
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => LibEithers.bind(adaptType(constraints)(litmap)(((_x) => _x.type)(ta)))(((atyp: Core.Type) => ({ tag: "right", value: ({ tag: "typeApplication", value: ({
    body: ((_x) => _x.body)(ta),
    type: atyp
  }) }) }))))((_m as any).value);
    case "typeLambda": return ((_: Core.TypeLambda) => ({ tag: "right", value: term1 }))((_m as any).value);
    default: return LibEithers.bind(tryTerm(term1))(((mterm: Core.Term | null) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("no alternatives for term: ")(ShowCore.term(term1)) }) }))(((term2: Core.Term) => ({ tag: "right", value: term2 })))(mterm)))(_m);
  }
})()));
})()));
  return Rewriting.rewriteTermM(rewrite)(term0);
})()))));
}

export function adaptTermForLanguage<t0>(lang: Coders.Language): ((x: t0) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((cx: t0) => ((g: Graph.Graph) => ((term: Core.Term) => (() => {
  const constraints = ((_x) => _x.constraints)(lang);
  return (() => {
  const litmap = adaptLiteralTypesMap(constraints);
  return adaptTerm(constraints)(litmap)(cx)(g)(term);
})();
})())));
}

export function adaptType(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: Core.Type) => Errors.Error | Core.Type)) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((type0: Core.Type) => (() => {
  const forSupported = ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => LibLogic.ifElse(literalTypeSupported(constraints)(lt))(typ)(LibMaybes.maybe(({ tag: "literal", value: ({ tag: "string" }) }))(((lt2: Core.LiteralType) => ({ tag: "literal", value: lt2 })))(LibMaps.lookup(lt)(litmap))))((_m as any).value);
    default: return typ(_m);
  }
})());
  const forUnsupported = ((typ: Core.Type) => (() => {
  const tryAlts = ((alts: ReadonlyArray<Core.Type>) => LibLogic.ifElse(LibLists.null_(alts))(null)(LibMaybes.maybe(tryAlts(LibLists.tail(alts)))(((t: Core.Type) => t))(tryType(LibLists.head(alts)))));
  return (() => {
  const alts0 = typeAlternatives(typ);
  return tryAlts(alts0);
})();
})());
  const tryType = ((typ: Core.Type) => (() => {
  const supportedVariant = LibSets.member(Reflect.typeVariant(typ))(((_x) => _x.typeVariants)(constraints));
  return LibLogic.ifElse(supportedVariant)(forSupported(typ))(forUnsupported(typ));
})());
  return (() => {
  const rewrite = ((recurse: ((x: Core.Type) => Errors.Error | Core.Type)) => ((typ: Core.Type) => LibEithers.bind(recurse(typ))(((type1: Core.Type) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("no alternatives for type: ")(ShowCore.type(typ)) }) }))(((type2: Core.Type) => ({ tag: "right", value: type2 })))(tryType(type1))))));
  return Rewriting.rewriteTypeM(rewrite)(type0);
})();
})()));
}

export function adaptTypeForLanguage(lang: Coders.Language): ((x: Core.Type) => Errors.Error | Core.Type) {
  return ((typ: Core.Type) => (() => {
  const constraints = ((_x) => _x.constraints)(lang);
  return (() => {
  const litmap = adaptLiteralTypesMap(constraints);
  return adaptType(constraints)(litmap)(typ);
})();
})());
}

export function adaptTypeScheme(constraints: Coders.LanguageConstraints): ((x: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((x: Core.TypeScheme) => Errors.Error | Core.TypeScheme)) {
  return ((litmap: ReadonlyMap<Core.LiteralType, Core.LiteralType>) => ((ts0: Core.TypeScheme) => (() => {
  const vars0 = ((_x) => _x.variables)(ts0);
  return (() => {
  const t0 = ((_x) => _x.type)(ts0);
  return LibEithers.bind(adaptType(constraints)(litmap)(t0))(((t1: Core.Type) => ({ tag: "right", value: ({
    variables: vars0,
    type: t1,
    constraints: ((_x) => _x.constraints)(ts0)
  }) })));
})();
})()));
}

export function composeCoders<t0, t1, t2>(c1: Coders.Coder<t0, t1>): ((x: Coders.Coder<t1, t2>) => Coders.Coder<t0, t2>) {
  return ((c2: Coders.Coder<t1, t2>) => ({
    encode: ((cx: Context.Context) => ((a: t0) => LibEithers.bind(((_x) => _x.encode)(c1)(cx)(a))(((b1: t1) => ((_x) => _x.encode)(c2)(cx)(b1))))),
    decode: ((cx: Context.Context) => ((c: t2) => LibEithers.bind(((_x) => _x.decode)(c2)(cx)(c))(((b2: t1) => ((_x) => _x.decode)(c1)(cx)(b2)))))
  }));
}

export function dataGraphToDefinitions(constraints: Coders.LanguageConstraints): ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: boolean) => ((x: ReadonlyArray<Core.Binding>) => ((x: Graph.Graph) => ((x: ReadonlyArray<Packaging.Namespace>) => ((x: Context.Context) => Errors.Error | readonly [Graph.Graph, ReadonlyArray<ReadonlyArray<Packaging.TermDefinition>>])))))))) {
  return ((doInfer: boolean) => ((doExpand: boolean) => ((doHoistCaseStatements: boolean) => ((doHoistPolymorphicLetBindings: boolean) => ((originalBindings: ReadonlyArray<Core.Binding>) => ((graph0: Graph.Graph) => ((namespaces: ReadonlyArray<Packaging.Namespace>) => ((cx: Context.Context) => (() => {
  const namespacesSet = LibSets.fromList(namespaces);
  return (() => {
  const isParentBinding = ((b: Core.Binding) => LibMaybes.maybe(false)(((ns: Packaging.Namespace) => LibSets.member(ns)(namespacesSet)))(Names.namespaceOf(((_x) => _x.name)(b))));
  return (() => {
  const hoistCases = ((bindings: ReadonlyArray<Core.Binding>) => (() => {
  const stripped = LibLists.map(((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: Strip.stripTypeLambdas(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  })))(bindings);
  return (() => {
  const term0 = ({ tag: "let", value: ({
    bindings: stripped,
    body: ({ tag: "unit" })
  }) });
  return (() => {
  const unshadowed0 = Environment.termAsBindings(Variables.unshadowVariables(term0));
  return (() => {
  const hoisted = Hoisting.hoistCaseStatementsInGraph(unshadowed0);
  return (() => {
  const term1 = ({ tag: "let", value: ({
    bindings: hoisted,
    body: ({ tag: "unit" })
  }) });
  return Environment.termAsBindings(Variables.unshadowVariables(term1));
})();
})();
})();
})();
})());
  return (() => {
  const hoistPoly = ((bindings: ReadonlyArray<Core.Binding>) => (() => {
  const letBefore = ({
    bindings: bindings,
    body: ({ tag: "unit" })
  });
  return (() => {
  const letAfter = Hoisting.hoistPolymorphicLetBindings(isParentBinding)(letBefore);
  return ((_x) => _x.bindings)(letAfter);
})();
})());
  return (() => {
  const checkBindingsTyped = ((debugLabel: string) => ((bindings: ReadonlyArray<Core.Binding>) => (() => {
  const untypedBindings = LibLists.map(((b: Core.Binding) => ((_x) => _x)(((_x) => _x.name)(b))))(LibLists.filter(((b: Core.Binding) => LibLogic.not(LibMaybes.isJust(((_x) => _x.type)(b)))))(bindings));
  return LibLogic.ifElse(LibLists.null_(untypedBindings))(({ tag: "right", value: bindings }))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat(["Found untyped bindings (", debugLabel, "): ", LibStrings.intercalate(", ")(untypedBindings)]) }) }));
})()));
  return (() => {
  const normalizeBindings = ((bindings: ReadonlyArray<Core.Binding>) => LibLists.map(((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: pushTypeAppsInward(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  })))(bindings));
  return (() => {
  const rebuildGraph = ((bindings: ReadonlyArray<Core.Binding>) => (() => {
  const g = Lexical.buildGraph(bindings)(LibMaps.empty)(((_x) => _x.primitives)(graph0));
  return ({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: ((_x) => _x.boundTypes)(g),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(graph0),
    typeVariables: ((_x) => _x.typeVariables)(g)
  });
})());
  return (() => {
  const bins0 = originalBindings;
  return (() => {
  const bins1 = LibLogic.ifElse(doHoistCaseStatements)(hoistCases(bins0))(bins0);
  return LibEithers.bind(LibLogic.ifElse(doInfer)(LibEithers.map(((result: readonly [readonly [Graph.Graph, ReadonlyArray<Core.Binding>], Context.Context]) => LibPairs.second(LibPairs.first(result))))(Inference.inferGraphTypes(cx)(bins1)(rebuildGraph(bins1))))(checkBindingsTyped("after case hoisting")(bins1)))(((bins2: ReadonlyArray<Core.Binding>) => LibEithers.bind(LibLogic.ifElse(doHoistPolymorphicLetBindings)(checkBindingsTyped("after let hoisting")(hoistPoly(bins2)))(({ tag: "right", value: bins2 })))(((bins3: ReadonlyArray<Core.Binding>) => LibEithers.bind(adaptDataGraph(constraints)(doExpand)(bins3)(cx)(rebuildGraph(bins3)))(((adaptResult: readonly [Graph.Graph, ReadonlyArray<Core.Binding>]) => (() => {
  const adapted = LibPairs.first(adaptResult);
  return (() => {
  const adaptedBindings = LibPairs.second(adaptResult);
  return LibEithers.bind(checkBindingsTyped("after adaptation")(adaptedBindings))(((bins4: ReadonlyArray<Core.Binding>) => (() => {
  const bins5 = normalizeBindings(bins4);
  return (() => {
  const toDef = ((el: Core.Binding) => LibMaybes.map(((ts: Core.TypeScheme) => ({
    name: ((_x) => _x.name)(el),
    term: ((_x) => _x.term)(el),
    type: ts
  })))(((_x) => _x.type)(el)));
  return (() => {
  const selectedElements = LibLists.filter(((el: Core.Binding) => LibMaybes.maybe(false)(((ns: Packaging.Namespace) => LibSets.member(ns)(namespacesSet)))(Names.namespaceOf(((_x) => _x.name)(el)))))(bins5);
  return (() => {
  const elementsByNamespace = LibLists.foldl(((acc: ReadonlyMap<Packaging.Namespace, ReadonlyArray<Core.Binding>>) => ((el: Core.Binding) => LibMaybes.maybe(acc)(((ns: Packaging.Namespace) => (() => {
  const existing = LibMaybes.maybe([])(LibEquality.identity)(LibMaps.lookup(ns)(acc));
  return LibMaps.insert(ns)(LibLists.concat2(existing)([el]))(acc);
})()))(Names.namespaceOf(((_x) => _x.name)(el))))))(LibMaps.empty)(selectedElements);
  return (() => {
  const defsGrouped = LibLists.map(((ns: Packaging.Namespace) => (() => {
  const elsForNs = LibMaybes.maybe([])(LibEquality.identity)(LibMaps.lookup(ns)(elementsByNamespace));
  return LibMaybes.cat(LibLists.map(toDef)(elsForNs));
})()))(namespaces);
  return (() => {
  const g = Lexical.buildGraph(bins5)(LibMaps.empty)(((_x) => _x.primitives)(adapted));
  return ({ tag: "right", value: [({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: ((_x) => _x.boundTypes)(g),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(adapted),
    typeVariables: ((_x) => _x.typeVariables)(g)
  }), defsGrouped] });
})();
})();
})();
})();
})();
})()));
})();
})()))))));
})();
})();
})();
})();
})();
})();
})();
})();
})()))))))));
}

export function literalTypeSupported(constraints: Coders.LanguageConstraints): ((x: Core.LiteralType) => boolean) {
  return ((lt: Core.LiteralType) => (() => {
  const forType = ((lt2: Core.LiteralType) => (() => {
  const _m = lt2;
  switch (_m.tag) {
    case "float": return ((ft: Core.FloatType) => LibSets.member(ft)(((_x) => _x.floatTypes)(constraints)))((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => LibSets.member(it)(((_x) => _x.integerTypes)(constraints)))((_m as any).value);
    default: return true(_m);
  }
})());
  return LibLogic.ifElse(LibSets.member(Reflect.literalTypeVariant(lt))(((_x) => _x.literalVariants)(constraints)))(forType(lt))(false);
})());
}

export function prepareFloatType(ft: Core.FloatType): readonly [Core.FloatType, readonly [((x: Core.FloatValue) => Core.FloatValue), ReadonlySet<string>]] {
  return (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => [({ tag: "float64" }), [((v: Core.FloatValue) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "bigfloat": return ((d: number) => ({ tag: "float64", value: LibLiterals.bigfloatToFloat64(d) }))((_m as any).value);
    default: return v(_m);
  }
})()), LibSets.fromList(["replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"])]])((_m as any).value);
    default: return prepareSame(ft)(_m);
  }
})();
}

export function prepareIntegerType(it: Core.IntegerType): readonly [Core.IntegerType, readonly [((x: Core.IntegerValue) => Core.IntegerValue), ReadonlySet<string>]] {
  return (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => [({ tag: "int64" }), [((v: Core.IntegerValue) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => ({ tag: "int64", value: LibLiterals.bigintToInt64(i) }))((_m as any).value);
    default: return v(_m);
  }
})()), LibSets.fromList(["replace arbitrary-precision integers with 64-bit integers"])]])((_m as any).value);
    case "uint8": return ((_: void) => [({ tag: "int8" }), [((v: Core.IntegerValue) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint8": return ((i: bigint) => ({ tag: "int8", value: LibLiterals.bigintToInt8(LibLiterals.uint8ToBigint(i)) }))((_m as any).value);
    default: return v(_m);
  }
})()), LibSets.fromList(["replace unsigned 8-bit integers with signed 8-bit integers"])]])((_m as any).value);
    case "uint32": return ((_: void) => [({ tag: "int32" }), [((v: Core.IntegerValue) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint32": return ((i: bigint) => ({ tag: "int32", value: LibLiterals.bigintToInt32(LibLiterals.uint32ToBigint(i)) }))((_m as any).value);
    default: return v(_m);
  }
})()), LibSets.fromList(["replace unsigned 32-bit integers with signed 32-bit integers"])]])((_m as any).value);
    case "uint64": return ((_: void) => [({ tag: "int64" }), [((v: Core.IntegerValue) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "uint64": return ((i: bigint) => ({ tag: "int64", value: LibLiterals.bigintToInt64(LibLiterals.uint64ToBigint(i)) }))((_m as any).value);
    default: return v(_m);
  }
})()), LibSets.fromList(["replace unsigned 64-bit integers with signed 64-bit integers"])]])((_m as any).value);
    default: return prepareSame(it)(_m);
  }
})();
}

export function prepareLiteralType(at: Core.LiteralType): readonly [Core.LiteralType, readonly [((x: Core.Literal) => Core.Literal), ReadonlySet<string>]] {
  return (() => {
  const _m = at;
  switch (_m.tag) {
    case "binary": return ((_: void) => [({ tag: "string" }), [((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => ({ tag: "string", value: LibLiterals.binaryToString(b) }))((_m as any).value);
    default: return v(_m);
  }
})()), LibSets.fromList(["replace binary strings with character strings"])]])((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const result = prepareFloatType(ft);
  const rtyp = LibPairs.first(result);
  const rep = LibPairs.first(LibPairs.second(result));
  const msgs = LibPairs.second(LibPairs.second(result));
  return [({ tag: "float", value: rtyp }), [((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "float": return ((fv: Core.FloatValue) => ({ tag: "float", value: rep(fv) }))((_m as any).value);
    default: return v(_m);
  }
})()), msgs]];
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const result = prepareIntegerType(it);
  const rtyp = LibPairs.first(result);
  const rep = LibPairs.first(LibPairs.second(result));
  const msgs = LibPairs.second(LibPairs.second(result));
  return [({ tag: "integer", value: rtyp }), [((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((iv: Core.IntegerValue) => ({ tag: "integer", value: rep(iv) }))((_m as any).value);
    default: return v(_m);
  }
})()), msgs]];
})())((_m as any).value);
    default: return prepareSame(at)(_m);
  }
})();
}

export function prepareSame<t0, t1, t2>(x: t0): readonly [t0, readonly [((x: t1) => t1), ReadonlySet<t2>]] {
  return [x, [((y: t1) => y), LibSets.empty]];
}

export function prepareType<t0>(cx: t0): ((x: Core.Type) => readonly [Core.Type, readonly [((x: Core.Term) => Core.Term), ReadonlySet<string>]]) {
  return ((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "literal": return ((at: Core.LiteralType) => (() => {
  const result = prepareLiteralType(at);
  const rtyp = LibPairs.first(result);
  const rep = LibPairs.first(LibPairs.second(result));
  const msgs = LibPairs.second(LibPairs.second(result));
  return [({ tag: "literal", value: rtyp }), [((v: Core.Term) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "literal": return ((av: Core.Literal) => ({ tag: "literal", value: rep(av) }))((_m as any).value);
    default: return v(_m);
  }
})()), msgs]];
})())((_m as any).value);
    default: return prepareSame(typ)(_m);
  }
})());
}

export function pushTypeAppsInward(term: Core.Term): Core.Term {
  return (() => {
  const push = ((body: Core.Term) => ((typ: Core.Type) => (() => {
  const _m = body;
  switch (_m.tag) {
    case "application": return ((a: Core.Application) => go(({ tag: "application", value: ({
    function: ({ tag: "typeApplication", value: ({
    body: ((_x) => _x.function)(a),
    type: typ
  }) }),
    argument: ((_x) => _x.argument)(a)
  }) })))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => go(({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: ({ tag: "typeApplication", value: ({
    body: ((_x) => _x.body)(l),
    type: typ
  }) })
  }) })))((_m as any).value);
    case "let": return ((lt: Core.Let) => go(({ tag: "let", value: ({
    bindings: ((_x) => _x.bindings)(lt),
    body: ({ tag: "typeApplication", value: ({
    body: ((_x) => _x.body)(lt),
    type: typ
  }) })
  }) })))((_m as any).value);
    default: return ({ tag: "typeApplication", value: ({
    body: body,
    type: typ
  }) })(_m);
  }
})()));
  const go = ((t: Core.Term) => (() => {
  const forField = ((fld: Core.Field) => ({
    name: ((_x) => _x.name)(fld),
    term: go(((_x) => _x.term)(fld))
  }));
  return (() => {
  const forLet = ((lt: Core.Let) => (() => {
  const mapBinding = ((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: go(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  }));
  return ({
    bindings: LibLists.map(mapBinding)(((_x) => _x.bindings)(lt)),
    body: go(((_x) => _x.body)(lt))
  });
})());
  return (() => {
  const forMap = ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const forPair = ((p: readonly [Core.Term, Core.Term]) => [go(LibPairs.first(p)), go(LibPairs.second(p))]);
  return LibMaps.fromList(LibLists.map(forPair)(LibMaps.toList(m)));
})());
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: go(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "application": return ((a: Core.Application) => ({ tag: "application", value: ({
    function: go(((_x) => _x.function)(a)),
    argument: go(((_x) => _x.argument)(a))
  }) }))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(go)(((_x) => _x.default)(cs)),
    cases: LibLists.map(forField)(((_x) => _x.cases)(cs))
  }) }))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "either", value: LibEithers.either(((l: Core.Term) => ({ tag: "left", value: go(l) })))(((r: Core.Term) => ({ tag: "right", value: go(r) })))(e) }))((_m as any).value);
    case "lambda": return ((l: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(l),
    domain: ((_x) => _x.domain)(l),
    body: go(((_x) => _x.body)(l))
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: forLet(lt) }))((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => ({ tag: "list", value: LibLists.map(go)(els) }))((_m as any).value);
    case "literal": return ((v: Core.Literal) => ({ tag: "literal", value: v }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "map", value: forMap(m) }))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => ({ tag: "maybe", value: LibMaybes.map(go)(m) }))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "pair", value: [go(LibPairs.first(p)), go(LibPairs.second(p))] }))((_m as any).value);
    case "project": return ((p: Core.Projection) => ({ tag: "project", value: p }))((_m as any).value);
    case "record": return ((r: Core.Record) => ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(r),
    fields: LibLists.map(forField)(((_x) => _x.fields)(r))
  }) }))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => ({ tag: "set", value: LibSets.fromList(LibLists.map(go)(LibSets.toList(s))) }))((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => (() => {
  const body1 = go(((_x) => _x.body)(tt));
  return push(body1)(((_x) => _x.type)(tt));
})())((_m as any).value);
    case "typeLambda": return ((ta: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(ta),
    body: go(((_x) => _x.body)(ta))
  }) }))((_m as any).value);
    case "inject": return ((i: Core.Injection) => ({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(i),
    field: forField(((_x) => _x.field)(i))
  }) }))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "unit" }))((_m as any).value);
    case "unwrap": return ((n: Core.Name) => ({ tag: "unwrap", value: n }))((_m as any).value);
    case "variable": return ((v: Core.Name) => ({ tag: "variable", value: v }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => ({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: go(((_x) => _x.body)(wt))
  }) }))((_m as any).value);
  }
})();
})();
})();
})());
  return go(term);
})();
}

export function schemaGraphToDefinitions<t0>(constraints: Coders.LanguageConstraints): ((x: Graph.Graph) => ((x: ReadonlyArray<ReadonlyArray<Core.Name>>) => ((x: t0) => Errors.Error | readonly [ReadonlyMap<Core.Name, Core.Type>, ReadonlyArray<ReadonlyArray<Packaging.TypeDefinition>>]))) {
  return ((graph: Graph.Graph) => ((nameLists: ReadonlyArray<ReadonlyArray<Core.Name>>) => ((cx: t0) => (() => {
  const litmap = adaptLiteralTypesMap(constraints);
  return LibEithers.bind(LibEithers.bimap(((e: Errors.DecodingError) => ({ tag: "decoding", value: e })))(((x: ReadonlyMap<Core.Name, Core.Type>) => x))(Environment.graphAsTypes(graph)(Lexical.graphToBindings(graph))))(((tmap0: ReadonlyMap<Core.Name, Core.Type>) => LibEithers.bind(adaptGraphSchema(constraints)(litmap)(tmap0))(((tmap1: ReadonlyMap<Core.Name, Core.Type>) => (() => {
  const toDef = ((pair: readonly [Core.Name, Core.Type]) => ({
    name: LibPairs.first(pair),
    type: ({
    variables: [],
    type: LibPairs.second(pair),
    constraints: null
  })
  }));
  return ({ tag: "right", value: [tmap1, LibLists.map(((names: ReadonlyArray<Core.Name>) => LibLists.map(toDef)(LibLists.map(((n: Core.Name) => [n, LibMaybes.fromJust(LibMaps.lookup(n)(tmap1))]))(names))))(nameLists)] });
})()))));
})())));
}

export function simpleLanguageAdapter<t0>(lang: Coders.Language): ((x: t0) => ((x: Graph.Graph) => ((x: Core.Type) => Errors.Error | Coders.Adapter<Core.Type, Core.Type, Core.Term, Core.Term>))) {
  return ((cx: t0) => ((g: Graph.Graph) => ((typ: Core.Type) => (() => {
  const constraints = ((_x) => _x.constraints)(lang);
  return (() => {
  const litmap = adaptLiteralTypesMap(constraints);
  return LibEithers.bind(adaptType(constraints)(litmap)(typ))(((adaptedType: Core.Type) => ({ tag: "right", value: ({
    isLossy: false,
    source: typ,
    target: adaptedType,
    coder: ({
    encode: ((cx2: Context.Context) => ((term: Core.Term) => adaptTerm(constraints)(litmap)(cx2)(g)(term))),
    decode: ((cx2: Context.Context) => ((term: Core.Term) => ({ tag: "right", value: term })))
  })
  }) })));
})();
})())));
}

export function termAlternatives<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => (() => {
  const term2 = ((_x) => _x.body)(at);
  return ({ tag: "right", value: [term2] });
})())((_m as any).value);
    case "maybe": return ((ot: Core.Term | null) => ({ tag: "right", value: [({ tag: "list", value: LibMaybes.maybe([])(((term2: Core.Term) => [term2]))(ot) })] }))((_m as any).value);
    case "typeLambda": return ((abs: Core.TypeLambda) => (() => {
  const term2 = ((_x) => _x.body)(abs);
  return ({ tag: "right", value: [term2] });
})())((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const term2 = ((_x) => _x.body)(ta);
  return ({ tag: "right", value: [term2] });
})())((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const tname = ((_x) => _x.typeName)(inj);
  return (() => {
  const field = ((_x) => _x.field)(inj);
  return (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const forFieldType = ((ft: Core.FieldType) => (() => {
  const ftname = ((_x) => _x.name)(ft);
  return ({
    name: fname,
    term: ({ tag: "maybe", value: LibLogic.ifElse(LibEquality.equal(ftname)(fname))(fterm)(null) })
  });
})());
  return LibEithers.bind(Resolution.requireUnionType(cx)(graph)(tname))(((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: [({ tag: "record", value: ({
    typeName: tname,
    fields: LibLists.map(forFieldType)(rt)
  }) })] })));
})();
})();
})();
})();
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: [({ tag: "literal", value: ({ tag: "boolean", value: true }) })] }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const term2 = ((_x) => _x.body)(wt);
  return ({ tag: "right", value: [term2] });
})())((_m as any).value);
    default: return ({ tag: "right", value: [] })(_m);
  }
})()));
}

export function typeAlternatives(type: Core.Type): ReadonlyArray<Core.Type> {
  return (() => {
  const _m = type;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => (() => {
  const type2 = ((_x) => _x.body)(at);
  return [type2];
})())((_m as any).value);
    case "maybe": return ((ot: Core.Type) => [({ tag: "list", value: ot })])((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const toOptField = ((f: Core.FieldType) => ({
    name: ((_x) => _x.name)(f),
    type: ({ tag: "maybe", value: ((_x) => _x.type)(f) })
  }));
  return (() => {
  const optFields = LibLists.map(toOptField)(rt);
  return [({ tag: "record", value: optFields })];
})();
})())((_m as any).value);
    case "unit": return ((_: void) => [({ tag: "literal", value: ({ tag: "boolean" }) })])((_m as any).value);
    case "void": return ((_: void) => [({ tag: "unit" })])((_m as any).value);
    default: return [](_m);
  }
})();
}
