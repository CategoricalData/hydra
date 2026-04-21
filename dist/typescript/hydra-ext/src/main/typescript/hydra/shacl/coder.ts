// Note: this is an automatically generated file. Do not edit.

/**
 * SHACL coder: converts Hydra types and terms to SHACL shapes and RDF descriptions
 */



import * as Annotations from "../annotations.js";
import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Constants from "../constants.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as DecodeCore from "../decode/core.js";
import * as EncodeCore from "../encode/core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMath from "../lib/math.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as RdfSyntax from "../rdf/syntax.js";
import * as RdfUtils from "../rdf/utils.js";
import * as Relational from "../relational.js";
import * as ShaclModel from "./model.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function common(constraints: ReadonlyArray<ShaclModel.CommonConstraint>): ShaclModel.CommonProperties {
  return ({
    constraints: LibSets.fromList(constraints),
    deactivated: null,
    message: LibMaps.empty,
    severity: ({ tag: "info" }),
    targetClass: LibSets.empty,
    targetNode: LibSets.empty,
    targetObjectsOf: LibSets.empty,
    targetSubjectsOf: LibSets.empty
  });
}

export const defaultCommonProperties: ShaclModel.CommonProperties = common([]);

export function elementIri(el: Core.Binding): RdfSyntax.Iri {
  return RdfUtils.nameToIri(((_x) => _x.name)(el));
}

export function encodeField(rname: Core.Name): ((x: RdfSyntax.Resource) => ((x: Core.Field) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | readonly [ReadonlyArray<RdfSyntax.Triple>, Context.Context])))) {
  return ((subject: RdfSyntax.Resource) => ((field: Core.Field) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const pair1 = RdfUtils.nextBlankNode(cx);
  const node = LibPairs.first(pair1);
  const cx1 = LibPairs.second(pair1);
  return LibEithers.bind(encodeTerm(node)(((_x) => _x.term)(field))(cx1)(g))(((__r1: readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]) => (() => {
  const descs = LibPairs.first(__r1);
  const cx2 = LibPairs.second(__r1);
  return ({ tag: "right", value: [LibLists.concat2(RdfUtils.triplesOf(descs))(RdfUtils.forObjects(subject)(RdfUtils.propertyIri(rname)(((_x) => _x.name)(field)))(RdfUtils.subjectsOf(descs))), cx2] });
})()));
})()))));
}

export function encodeFieldType<t0>(rname: Core.Name): ((x: bigint | null) => ((x: Core.FieldType) => ((x: t0) => Errors.Error | ShaclModel.Definition<ShaclModel.PropertyShape>))) {
  return ((order: bigint | null) => ((ft: Core.FieldType) => ((cx: t0) => (() => {
  const fname = ((_x) => _x.name)(ft);
  const ftype = ((_x) => _x.type)(ft);
  const iri = RdfUtils.propertyIri(rname)(fname);
  const forType = ((mn: bigint | null) => ((mx: bigint | null) => ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "maybe": return ((ot: Core.Type) => forType(0n)(mx)(ot))((_m as any).value);
    case "set": return ((st: Core.Type) => forType(mn)(null)(st))((_m as any).value);
    default: return forTypeDefault(mn)(mx)(t)(_m);
  }
})())));
  const forTypeDefault = ((mn: bigint | null) => ((mx: bigint | null) => ((t: Core.Type) => LibEithers.map(((__cp: ShaclModel.CommonProperties) => (() => {
  const baseProp = property(iri);
  const minC = LibMaybes.map(((__n: bigint) => ({ tag: "minCount", value: __n })))(mn);
  const maxC = LibMaybes.map(((__n: bigint) => ({ tag: "maxCount", value: __n })))(mx);
  return ({
    iri: iri,
    target: ({
    common: __cp,
    constraints: LibSets.fromList(LibMaybes.cat([minC, maxC])),
    defaultValue: null,
    description: LibMaps.empty,
    name: LibMaps.empty,
    order: order,
    path: iri
  })
  });
})()))(encodeType(rname)(t)(cx)))));
  return forType(1n)(1n)(ftype);
})())));
}

export function encodeList(subj: RdfSyntax.Resource): ((x: ReadonlyArray<Core.Term>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]))) {
  return ((terms: ReadonlyArray<Core.Term>) => ((cx0: Context.Context) => ((g: Graph.Graph) => LibLogic.ifElse(LibLists.null_(terms))(({ tag: "right", value: [[({
    subject: ({ tag: "iri", value: "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil" }),
    graph: LibSets.empty
  })], cx0] }))((() => {
  const pair1 = RdfUtils.nextBlankNode(cx0);
  const node1 = LibPairs.first(pair1);
  const cx1 = LibPairs.second(pair1);
  return LibEithers.bind(encodeTerm(node1)(LibLists.head(terms))(cx1)(g))(((__r1: readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]) => (() => {
  const fdescs = LibPairs.first(__r1);
  const cx2 = LibPairs.second(__r1);
  const firstTriples = LibLists.concat2(RdfUtils.triplesOf(fdescs))(RdfUtils.forObjects(subj)(RdfUtils.rdfIri("first"))(RdfUtils.subjectsOf(fdescs)));
  const pair2 = RdfUtils.nextBlankNode(cx2);
  const next = LibPairs.first(pair2);
  const cx3 = LibPairs.second(pair2);
  return LibEithers.map(((__r2: readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]) => (() => {
  const rdescs = LibPairs.first(__r2);
  const cx4 = LibPairs.second(__r2);
  const restTriples = LibLists.concat2(RdfUtils.triplesOf(rdescs))(RdfUtils.forObjects(subj)(RdfUtils.rdfIri("rest"))(RdfUtils.subjectsOf(rdescs)));
  return [[({
    subject: RdfUtils.resourceToNode(subj),
    graph: LibSets.fromList(LibLists.concat2(firstTriples)(restTriples))
  })], cx4];
})()))(encodeList(next)(LibLists.tail(terms))(cx3)(g));
})()));
})()))));
}

export function encodeLiteralType(lt: Core.LiteralType): ShaclModel.CommonProperties {
  return (() => {
  const xsd = ((local: string) => common([({ tag: "datatype", value: RdfUtils.xmlSchemaDatatypeIri(local) })]));
  return (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => xsd("base64Binary"))((_m as any).value);
    case "boolean": return ((_: void) => xsd("boolean"))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => xsd("decimal"))((_m as any).value);
    case "float32": return ((_: void) => xsd("float"))((_m as any).value);
    case "float64": return ((_: void) => xsd("double"))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => xsd("integer"))((_m as any).value);
    case "int8": return ((_: void) => xsd("byte"))((_m as any).value);
    case "int16": return ((_: void) => xsd("short"))((_m as any).value);
    case "int32": return ((_: void) => xsd("int"))((_m as any).value);
    case "int64": return ((_: void) => xsd("long"))((_m as any).value);
    case "uint8": return ((_: void) => xsd("unsignedByte"))((_m as any).value);
    case "uint16": return ((_: void) => xsd("unsignedShort"))((_m as any).value);
    case "uint32": return ((_: void) => xsd("unsignedInt"))((_m as any).value);
    case "uint64": return ((_: void) => xsd("unsignedLong"))((_m as any).value);
  }
})())((_m as any).value);
    case "string": return ((_: void) => xsd("string"))((_m as any).value);
  }
})();
})();
}

export function encodeTerm(subject: RdfSyntax.Resource): ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]))) {
  return ((term: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => encodeTerm(subject)(((_x) => _x.body)(at))(cx)(g))((_m as any).value);
    case "list": return ((terms: ReadonlyArray<Core.Term>) => encodeList(subject)(terms)(cx)(g))((_m as any).value);
    case "literal": return ((lit: Core.Literal) => ({ tag: "right", value: [[({
    subject: ({ tag: "literal", value: RdfUtils.encodeLiteral(lit) }),
    graph: LibSets.empty
  })], cx] }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.map(((__r: readonly [ReadonlyArray<ReadonlyArray<RdfSyntax.Triple>>, Context.Context]) => [[({
    subject: RdfUtils.resourceToNode(subject),
    graph: LibSets.fromList(LibLists.concat(LibPairs.first(__r)))
  })], LibPairs.second(__r)]))(foldAccumResult(((__cx0: Context.Context) => ((kv: readonly [Core.Term, Core.Term]) => LibEithers.bind(ExtractCore.string(g)(Strip.deannotateTerm(LibPairs.first(kv))))(((__ks: string) => (() => {
  const pair2 = RdfUtils.nextBlankNode(__cx0);
  const node2 = LibPairs.first(pair2);
  const cx2 = LibPairs.second(pair2);
  return LibEithers.map(((__dr: readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]) => [LibLists.concat2(RdfUtils.forObjects(subject)(RdfUtils.keyIri(__ks))(RdfUtils.subjectsOf(LibPairs.first(__dr))))(RdfUtils.triplesOf(LibPairs.first(__dr))), LibPairs.second(__dr)]))(encodeTerm(node2)(LibPairs.second(kv))(cx2)(g));
})())))))(cx)(LibMaps.toList(m))))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => LibEithers.map(((__dr: readonly [ReadonlyArray<RdfSyntax.Description>, Context.Context]) => (() => {
  const descs = LibPairs.first(__dr);
  const cx1 = LibPairs.second(__dr);
  return [LibLists.cons(withType(((_x) => _x.typeName)(wt))(LibLists.head(descs)))(LibLists.tail(descs)), cx1];
})()))(encodeTerm(subject)(((_x) => _x.body)(wt))(cx)(g)))((_m as any).value);
    case "maybe": return ((mterm: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: [[], cx] }))(((__inner: Core.Term) => encodeTerm(subject)(__inner)(cx)(g)))(mterm))((_m as any).value);
    case "record": return ((rec: Core.Record) => (() => {
  const rname = ((_x) => _x.typeName)(rec);
  const fields = ((_x) => _x.fields)(rec);
  return LibEithers.map(((__r: readonly [ReadonlyArray<ReadonlyArray<RdfSyntax.Triple>>, Context.Context]) => [[withType(rname)(({
    subject: RdfUtils.resourceToNode(subject),
    graph: LibSets.fromList(LibLists.concat(LibPairs.first(__r)))
  }))], LibPairs.second(__r)]))(foldAccumResult(((__cx0: Context.Context) => ((field: Core.Field) => encodeField(rname)(subject)(field)(__cx0)(g))))(cx)(fields));
})())((_m as any).value);
    case "set": return ((terms: ReadonlySet<Core.Term>) => LibEithers.map(((__r: readonly [ReadonlyArray<ReadonlyArray<RdfSyntax.Description>>, Context.Context]) => [LibLists.concat(LibPairs.first(__r)), LibPairs.second(__r)]))(foldAccumResult(((__cx0: Context.Context) => ((t: Core.Term) => (() => {
  const pair3 = RdfUtils.nextBlankNode(__cx0);
  const node3 = LibPairs.first(pair3);
  const cx3 = LibPairs.second(pair3);
  return encodeTerm(node3)(t)(cx3)(g);
})())))(cx)(LibSets.toList(terms))))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const rname = ((_x) => _x.typeName)(inj);
  const field = ((_x) => _x.field)(inj);
  return LibEithers.map(((__r: readonly [ReadonlyArray<RdfSyntax.Triple>, Context.Context]) => [[withType(rname)(({
    subject: RdfUtils.resourceToNode(subject),
    graph: LibSets.fromList(LibPairs.first(__r))
  }))], LibPairs.second(__r)]))(encodeField(rname)(subject)(field)(cx)(g));
})())((_m as any).value);
    default: return unexpectedE(cx)("RDF-compatible term")("unsupported term variant")(_m);
  }
})())));
}

export function encodeType<t0>(tname: Core.Name): ((x: Core.Type) => ((x: t0) => Errors.Error | ShaclModel.CommonProperties)) {
  return ((typ: Core.Type) => ((cx: t0) => (() => {
  const any = ({ tag: "right", value: common([]) });
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "either": return ((_: Core.EitherType) => any)((_m as any).value);
    case "list": return ((_: Core.Type) => any)((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => ({ tag: "right", value: encodeLiteralType(lt) }))((_m as any).value);
    case "map": return ((_: Core.MapType) => any)((_m as any).value);
    case "pair": return ((_: Core.PairType) => any)((_m as any).value);
    case "wrap": return ((_: Core.Type) => any)((_m as any).value);
    case "record": return ((fts: ReadonlyArray<Core.FieldType>) => LibEithers.map(((__props: ReadonlyArray<ShaclModel.Definition<ShaclModel.PropertyShape>>) => common([({ tag: "property", value: LibSets.fromList(LibLists.map(((__p: ShaclModel.Definition<ShaclModel.PropertyShape>) => ({ tag: "definition", value: __p })))(__props)) })])))(LibEithers.mapList(((__pair: readonly [bigint, Core.FieldType]) => encodeFieldType(tname)(LibPairs.first(__pair))(LibPairs.second(__pair))(cx)))(LibLists.zip(LibLists.map(((__i: number) => LibLiterals.int32ToBigint(__i)))(LibMath.range(0)(LibLists.length(fts))))(fts))))((_m as any).value);
    case "set": return ((_: Core.Type) => any)((_m as any).value);
    case "union": return ((fts: ReadonlyArray<Core.FieldType>) => LibEithers.map(((__props: ReadonlyArray<ShaclModel.Definition<ShaclModel.PropertyShape>>) => common([({ tag: "xone", value: LibSets.fromList(LibLists.map(((__p: ShaclModel.Definition<ShaclModel.PropertyShape>) => ({ tag: "anonymous", value: node([({ tag: "property", value: LibSets.fromList([({ tag: "definition", value: __p })]) })]) })))(__props)) })])))(LibEithers.mapList(((__ft: Core.FieldType) => encodeFieldType(tname)(null)(__ft)(cx)))(fts)))((_m as any).value);
    case "unit": return ((_: void) => any)((_m as any).value);
    case "variable": return ((vname: Core.Name) => ({ tag: "right", value: common([({ tag: "node", value: LibSets.fromList([({ tag: "named", value: RdfUtils.nameToIri(vname) })]) })]) }))((_m as any).value);
    default: return unexpectedE(cx)("type")("unsupported type variant")(_m);
  }
})();
})()));
}

export function err<t0, t1>(cx: t0): ((x: string) => Errors.Error | t1) {
  return ((msg: string) => ({ tag: "left", value: ({ tag: "other", value: msg }) }));
}

export function foldAccumResult<t0, t1, t2, t3>(f: ((x: t0) => ((x: t1) => t2 | readonly [t3, t0]))): ((x: t0) => ((x: ReadonlyArray<t1>) => t2 | readonly [ReadonlyArray<t3>, t0])) {
  return ((cx: t0) => ((xs: ReadonlyArray<t1>) => LibLogic.ifElse(LibLists.null_(xs))(({ tag: "right", value: [[], cx] }))(LibEithers.bind(f(cx)(LibLists.head(xs)))(((__r: readonly [t3, t0]) => LibEithers.map(((__rest: readonly [ReadonlyArray<t3>, t0]) => [LibLists.cons(LibPairs.first(__r))(LibPairs.first(__rest)), LibPairs.second(__rest)]))(foldAccumResult(f)(LibPairs.second(__r))(LibLists.tail(xs))))))));
}

export function node(constraints: ReadonlyArray<ShaclModel.CommonConstraint>): ShaclModel.Shape {
  return ({ tag: "node", value: ({
    common: common(constraints)
  }) });
}

export function property(iri: RdfSyntax.Iri): ShaclModel.PropertyShape {
  return ({
    common: defaultCommonProperties,
    constraints: LibSets.empty,
    defaultValue: null,
    description: LibMaps.empty,
    name: LibMaps.empty,
    order: null,
    path: iri
  });
}

export function shaclCoder<t0>(mod: Packaging.Module): ((x: t0) => ((x: Graph.Graph) => Errors.Error | readonly [ShaclModel.ShapesGraph, t0])) {
  return ((cx: t0) => ((g: Graph.Graph) => (() => {
  const typeEls = LibMaybes.cat(LibLists.map(((d: Packaging.Definition) => (() => {
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
})()))(((_x) => _x.definitions)(mod)));
  const toShape = ((el: Core.Binding) => LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__t: Core.Type) => __t))(DecodeCore.type(g)(((_x) => _x.term)(el))))(((__typ: Core.Type) => LibEithers.map(((__cp: ShaclModel.CommonProperties) => ({
    iri: elementIri(el),
    target: ({ tag: "node", value: ({
    common: __cp
  }) })
  })))(encodeType(((_x) => _x.name)(el))(__typ)(cx)))));
  return LibEithers.map(((__shapes: ReadonlyArray<ShaclModel.Definition<ShaclModel.Shape>>) => [LibSets.fromList(__shapes), cx]))(LibEithers.mapList(toShape)(typeEls));
})()));
}

export function unexpectedE<t0, t1>(cx: t0): ((x: string) => ((x: string) => Errors.Error | t1)) {
  return ((expected: string) => ((found: string) => err(cx)(LibStrings.cat(["Expected ", expected, ", found: ", found]))));
}

export function withType(name: Core.Name): ((x: RdfSyntax.Description) => RdfSyntax.Description) {
  return ((desc: RdfSyntax.Description) => (() => {
  const subj = ((_x) => _x.subject)(desc);
  const triples = ((_x) => _x)(((_x) => _x.graph)(desc));
  const subjRes = (() => {
  const _m = subj;
  switch (_m.tag) {
    case "iri": return ((iri: RdfSyntax.Iri) => ({ tag: "iri", value: iri }))((_m as any).value);
    case "bnode": return ((bnode: RdfSyntax.BlankNode) => ({ tag: "bnode", value: bnode }))((_m as any).value);
  }
})();
  const triple = ({
    subject: subjRes,
    predicate: RdfUtils.rdfIri("type"),
    object: ({ tag: "iri", value: RdfUtils.nameToIri(name) })
  });
  return ({
    subject: subj,
    graph: LibSets.insert(triple)(triples)
  });
})());
}
