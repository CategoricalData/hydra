// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for reading and writing type and term annotations
 */



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
import * as ExtractCore from "./extract/core.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
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

export function aggregateAnnotations<t0, t1, t2, t3>(getValue: ((x: t0) => t1 | null)): ((x: ((x: t1) => t0)) => ((x: ((x: t1) => ReadonlyMap<t2, t3>)) => ((x: t0) => ReadonlyMap<t2, t3>))) {
  return ((getX: ((x: t1) => t0)) => ((getAnns: ((x: t1) => ReadonlyMap<t2, t3>)) => ((t: t0) => (() => {
  const toPairs = ((rest: ReadonlyArray<ReadonlyArray<readonly [t2, t3]>>) => ((t2: t0) => LibMaybes.maybe(rest)(((yy: t1) => toPairs(LibLists.cons(LibMaps.toList(getAnns(yy)))(rest))(getX(yy))))(getValue(t2))));
  return LibMaps.fromList(LibLists.concat(toPairs([])(t)));
})())));
}

export function commentsFromBinding<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.Error | string | null)) {
  return ((g: Graph.Graph) => ((b: Core.Binding) => getTermDescription(cx)(g)(((_x) => _x.term)(b))));
}

export function commentsFromFieldType<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.FieldType) => Errors.Error | string | null)) {
  return ((g: Graph.Graph) => ((ft: Core.FieldType) => getTypeDescription(cx)(g)(((_x) => _x.type)(ft))));
}

export function debugIf(cx: Context.Context): ((x: string) => ((x: string) => Errors.Error | void)) {
  return ((debugId: string) => ((message: string) => LibEithers.bind(getDebugId(cx))(((mid: string | null) => LibLogic.ifElse(LibEquality.equal(mid)(debugId))(({ tag: "left", value: ({ tag: "other", value: message }) }))(({ tag: "right", value: undefined }))))));
}

export function failOnFlag(cx: Context.Context): ((x: Core.Name) => ((x: string) => Errors.Error | void)) {
  return ((flag: Core.Name) => ((msg: string) => LibEithers.bind(hasFlag(cx)(flag))(((val: boolean) => LibLogic.ifElse(val)(({ tag: "left", value: ({ tag: "other", value: msg }) }))(({ tag: "right", value: undefined }))))));
}

export function getAttr(key: Core.Name): ((x: Context.Context) => Core.Term | null) {
  return ((cx: Context.Context) => LibMaps.lookup(key)(((_x) => _x.other)(cx)));
}

export function getAttrWithDefault(key: Core.Name): ((x: Core.Term) => ((x: Context.Context) => Core.Term)) {
  return ((def: Core.Term) => ((cx: Context.Context) => LibMaybes.fromMaybe(def)(getAttr(key)(cx))));
}

export function getCount(key: Core.Name): ((x: Context.Context) => number) {
  return ((cx: Context.Context) => LibMaybes.maybe(0)(((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "integer": return ((iv: Core.IntegerValue) => (() => {
  const _m = iv;
  switch (_m.tag) {
    case "int32": return ((i: number) => i)((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})())((_m as any).value);
    default: return 0(_m);
  }
})()))(LibMaps.lookup(key)(((_x) => _x.other)(cx))));
}

export function getDebugId(cx: Context.Context): Errors.Error | string | null {
  return LibMaybes.maybe(({ tag: "right", value: null }))(((term: Core.Term) => LibEithers.map(LibMaybes.pure)(ExtractCore.string(({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  }))(term))))(getAttr(Constants.key_debugId)(cx));
}

export function getDescription<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | string | null)) {
  return ((graph: Graph.Graph) => ((anns: ReadonlyMap<Core.Name, Core.Term>) => LibMaybes.maybe(({ tag: "right", value: null }))(((term: Core.Term) => LibEithers.map(LibMaybes.pure)(ExtractCore.string(graph)(term))))(LibMaps.lookup("description")(anns))));
}

export function getTermAnnotation(key: Core.Name): ((x: Core.Term) => Core.Term | null) {
  return ((term: Core.Term) => LibMaps.lookup(key)(termAnnotationInternal(term)));
}

export function getTermDescription<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | string | null)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => (() => {
  const peel = ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "typeLambda": return ((tl: Core.TypeLambda) => peel(((_x) => _x.body)(tl)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => peel(((_x) => _x.body)(ta)))((_m as any).value);
    default: return t(_m);
  }
})());
  return getDescription(cx)(graph)(termAnnotationInternal(peel(term)));
})()));
}

export function getType(graph: Graph.Graph): ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.DecodingError | Core.Type | null) {
  return ((anns: ReadonlyMap<Core.Name, Core.Term>) => LibMaybes.maybe(({ tag: "right", value: null }))(((dat: Core.Term) => LibEithers.map(LibMaybes.pure)(DecodeCore.type(graph)(dat))))(LibMaps.lookup(Constants.key_type)(anns)));
}

export function getTypeAnnotation(key: Core.Name): ((x: Core.Type) => Core.Term | null) {
  return ((typ: Core.Type) => LibMaps.lookup(key)(typeAnnotationInternal(typ)));
}

export function getTypeClasses<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ReadonlyMap<Core.Name, ReadonlySet<Classes.TypeClass>>)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => (() => {
  const decodeClass = ((term2: Core.Term) => (() => {
  const byName = LibMaps.fromList([["equality", ({ tag: "equality" })], ["ordering", ({ tag: "ordering" })]]);
  return LibEithers.bind(ExtractCore.unitVariant("hydra.classes.TypeClass")(graph)(term2))(((fn: Core.Name) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "type class",
    actual: ShowCore.term(term2)
  }) }) }) }))(((x: Classes.TypeClass) => ({ tag: "right", value: x })))(LibMaps.lookup(fn)(byName))));
})());
  return LibMaybes.maybe(({ tag: "right", value: LibMaps.empty }))(((term2: Core.Term) => ExtractCore.map(((t: Core.Term) => LibEithers.bimap(((de: Errors.DecodingError) => ({ tag: "decoding", value: de })))(((x: Core.Name) => x))(DecodeCore.name(graph)(t))))(((v1: Core.Term) => ExtractCore.setOf(decodeClass)(graph)(v1)))(graph)(term2)))(getTermAnnotation(Constants.key_classes)(term));
})()));
}

export function getTypeDescription<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Type) => Errors.Error | string | null)) {
  return ((graph: Graph.Graph) => ((typ: Core.Type) => getDescription(cx)(graph)(typeAnnotationInternal(typ))));
}

export function hasDescription<t0>(anns: ReadonlyMap<Core.Name, t0>): boolean {
  return LibMaybes.isJust(LibMaps.lookup(Constants.key_description)(anns));
}

export function hasFlag(cx: Context.Context): ((x: Core.Name) => Errors.Error | boolean) {
  return ((flag: Core.Name) => (() => {
  const term = getAttrWithDefault(flag)(({ tag: "literal", value: ({ tag: "boolean", value: false }) }))(cx);
  return ExtractCore.boolean_(({
    boundTerms: LibMaps.empty,
    boundTypes: LibMaps.empty,
    classConstraints: LibMaps.empty,
    lambdaVariables: LibSets.empty,
    metadata: LibMaps.empty,
    primitives: LibMaps.empty,
    schemaTypes: LibMaps.empty,
    typeVariables: LibSets.empty
  }))(term);
})());
}

export function hasTypeDescription(typ: Core.Type): boolean {
  return hasDescription(typeAnnotationInternal(typ));
}

export function isNativeType(el: Core.Binding): boolean {
  return (() => {
  const isFlaggedAsFirstClassType = LibMaybes.fromMaybe(false)(LibMaybes.map(((_: Core.Term) => true))(getTermAnnotation(Constants.key_firstClassType)(((_x) => _x.term)(el))));
  return LibMaybes.maybe(false)(((ts: Core.TypeScheme) => LibLogic.and(LibEquality.equal(ts)(({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.Type" }),
    constraints: null
  })))(LibLogic.not(isFlaggedAsFirstClassType))))(((_x) => _x.type)(el));
})();
}

export function nextCount(key: Core.Name): ((x: Context.Context) => readonly [number, Context.Context]) {
  return ((cx: Context.Context) => (() => {
  const count = getCount(key)(cx);
  return [count, putCount(key)(LibMath.add(count)(1))(cx)];
})());
}

export function normalizeTermAnnotations(term: Core.Term): Core.Term {
  return (() => {
  const anns = termAnnotationInternal(term);
  return (() => {
  const stripped = Strip.deannotateTerm(term);
  return LibLogic.ifElse(LibMaps.null_(anns))(stripped)(({ tag: "annotated", value: ({
    body: stripped,
    annotation: anns
  }) }));
})();
})();
}

export function normalizeTypeAnnotations(typ: Core.Type): Core.Type {
  return (() => {
  const anns = typeAnnotationInternal(typ);
  return (() => {
  const stripped = Strip.deannotateType(typ);
  return LibLogic.ifElse(LibMaps.null_(anns))(stripped)(({ tag: "annotated", value: ({
    body: stripped,
    annotation: anns
  }) }));
})();
})();
}

export function putAttr(key: Core.Name): ((x: Core.Term) => ((x: Context.Context) => Context.Context)) {
  return ((val: Core.Term) => ((cx: Context.Context) => ({
    trace: ((_x) => _x.trace)(cx),
    messages: ((_x) => _x.messages)(cx),
    other: LibMaps.insert(key)(val)(((_x) => _x.other)(cx))
  })));
}

export function putCount(key: Core.Name): ((x: number) => ((x: Context.Context) => Context.Context)) {
  return ((count: number) => ((cx: Context.Context) => putAttr(key)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: count }) }) }))(cx)));
}

export function resetCount(key: Core.Name): ((x: Context.Context) => Context.Context) {
  return ((cx: Context.Context) => putAttr(key)(({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: 0 }) }) }))(cx));
}

export function setAnnotation<t0, t1>(key: t0): ((x: t1 | null) => ((x: ReadonlyMap<t0, t1>) => ReadonlyMap<t0, t1>)) {
  return ((val: t1 | null) => ((m: ReadonlyMap<t0, t1>) => LibMaps.alter(((_: t1 | null) => val))(key)(m)));
}

export function setDescription(d: string | null): ((x: ReadonlyMap<Core.Name, Core.Term>) => ReadonlyMap<Core.Name, Core.Term>) {
  return ((v1: ReadonlyMap<Core.Name, Core.Term>) => setAnnotation(Constants.key_description)(LibMaybes.map(((arg_: string) => ({ tag: "literal", value: ({ tag: "string", value: arg_ }) })))(d))(v1));
}

export function setTermAnnotation(key: Core.Name): ((x: Core.Term | null) => ((x: Core.Term) => Core.Term)) {
  return ((val: Core.Term | null) => ((term: Core.Term) => (() => {
  const term_ = Strip.deannotateTerm(term);
  return (() => {
  const anns = setAnnotation(key)(val)(termAnnotationInternal(term));
  return LibLogic.ifElse(LibMaps.null_(anns))(term_)(({ tag: "annotated", value: ({
    body: term_,
    annotation: anns
  }) }));
})();
})()));
}

export function setTermDescription(d: string | null): ((x: Core.Term) => Core.Term) {
  return ((v1: Core.Term) => setTermAnnotation(Constants.key_description)(LibMaybes.map(((s: string) => ({ tag: "literal", value: ({ tag: "string", value: s }) })))(d))(v1));
}

export function setType(mt: Core.Type | null): ((x: ReadonlyMap<Core.Name, Core.Term>) => ReadonlyMap<Core.Name, Core.Term>) {
  return ((v1: ReadonlyMap<Core.Name, Core.Term>) => setAnnotation(Constants.key_type)(LibMaybes.map(EncodeCore.type)(mt))(v1));
}

export function setTypeAnnotation(key: Core.Name): ((x: Core.Term | null) => ((x: Core.Type) => Core.Type)) {
  return ((val: Core.Term | null) => ((typ: Core.Type) => (() => {
  const typ_ = Strip.deannotateType(typ);
  return (() => {
  const anns = setAnnotation(key)(val)(typeAnnotationInternal(typ));
  return LibLogic.ifElse(LibMaps.null_(anns))(typ_)(({ tag: "annotated", value: ({
    body: typ_,
    annotation: anns
  }) }));
})();
})()));
}

export function setTypeClasses(m: ReadonlyMap<Core.Name, ReadonlySet<Classes.TypeClass>>): ((x: Core.Term) => Core.Term) {
  return ((term: Core.Term) => (() => {
  const encodeClass = ((tc: Classes.TypeClass) => (() => {
  const _m = tc;
  switch (_m.tag) {
    case "equality": return ((_: void) => ({ tag: "inject", value: ({
    typeName: "hydra.classes.TypeClass",
    field: ({
    name: "equality",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
    case "ordering": return ((_: void) => ({ tag: "inject", value: ({
    typeName: "hydra.classes.TypeClass",
    field: ({
    name: "ordering",
    term: ({ tag: "unit" })
  })
  }) }))((_m as any).value);
  }
})());
  return (() => {
  const encodePair = ((nameClasses: readonly [Core.Name, ReadonlySet<Classes.TypeClass>]) => (() => {
  const name = LibPairs.first(nameClasses);
  return (() => {
  const classes = LibPairs.second(nameClasses);
  return [EncodeCore.name(name), ({ tag: "set", value: LibSets.fromList(LibLists.map(encodeClass)(LibSets.toList(classes))) })];
})();
})());
  return (() => {
  const encoded = LibLogic.ifElse(LibMaps.null_(m))(null)(({ tag: "map", value: LibMaps.fromList(LibLists.map(encodePair)(LibMaps.toList(m))) }));
  return setTermAnnotation(Constants.key_classes)(encoded)(term);
})();
})();
})());
}

export function setTypeDescription(d: string | null): ((x: Core.Type) => Core.Type) {
  return ((v1: Core.Type) => setTypeAnnotation(Constants.key_description)(LibMaybes.map(((arg_: string) => ({ tag: "literal", value: ({ tag: "string", value: arg_ }) })))(d))(v1));
}

export function termAnnotationInternal(term: Core.Term): ReadonlyMap<Core.Name, Core.Term> {
  return (() => {
  const getAnn = ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((a: Core.AnnotatedTerm) => a)((_m as any).value);
    default: return null(_m);
  }
})());
  return aggregateAnnotations(getAnn)(((at: Core.AnnotatedTerm) => ((_x) => _x.body)(at)))(((at: Core.AnnotatedTerm) => ((_x) => _x.annotation)(at)))(term);
})();
}

export function typeAnnotationInternal(typ: Core.Type): ReadonlyMap<Core.Name, Core.Term> {
  return (() => {
  const getAnn = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((a: Core.AnnotatedType) => a)((_m as any).value);
    default: return null(_m);
  }
})());
  return aggregateAnnotations(getAnn)(((at: Core.AnnotatedType) => ((_x) => _x.body)(at)))(((at: Core.AnnotatedType) => ((_x) => _x.annotation)(at)))(typ);
})();
}

export function whenFlag<t0>(cx: Context.Context): ((x: Core.Name) => ((x: Errors.Error | t0) => ((x: Errors.Error | t0) => Errors.Error | t0))) {
  return ((flag: Core.Name) => ((ethen: Errors.Error | t0) => ((eelse: Errors.Error | t0) => LibEithers.bind(hasFlag(cx)(flag))(((b: boolean) => LibLogic.ifElse(b)(ethen)(eelse))))));
}
