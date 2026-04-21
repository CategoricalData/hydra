// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for mapping Hydra terms to property graph elements using mapping specifications
 */



import * as Annotations from "../annotations.js";
import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as PgMapping from "./mapping.js";
import * as PgModel from "./model.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Resolution from "../resolution.js";
import * as ShowCore from "../show/core.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function applyPattern<t0>(cx: t0): ((x: string) => ((x: ReadonlyArray<readonly [ReadonlyArray<string>, string]>) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>))) {
  return ((firstLit: string) => ((pairs: ReadonlyArray<readonly [ReadonlyArray<string>, string]>) => ((term: Core.Term) => LibLogic.ifElse(LibLists.null_(pairs))(({ tag: "right", value: [({ tag: "literal", value: ({ tag: "string", value: firstLit }) })] }))(LibEithers.bind(LibEithers.mapList(((pp: readonly [ReadonlyArray<string>, string]) => LibEithers.map(((terms: ReadonlyArray<Core.Term>) => [LibLists.map(((t: Core.Term) => termToString(t)))(terms), LibPairs.second(pp)]))(evalPath(cx)(LibPairs.first(pp))(term))))(pairs))(((evaluated: ReadonlyArray<readonly [ReadonlyArray<string>, string]>) => ({ tag: "right", value: LibLists.map(((s: string) => ({ tag: "literal", value: ({ tag: "string", value: s }) })))(LibLists.foldl(((accum: ReadonlyArray<string>) => ((ep: readonly [ReadonlyArray<string>, string]) => (() => {
  const pStrs = LibPairs.first(ep);
  const litP = LibPairs.second(ep);
  return LibLists.concat(LibLists.map(((pStr: string) => LibLists.map(((a: string) => LibStrings.cat2(LibStrings.cat2(a)(pStr))(litP)))(accum)))(pStrs));
})())))([firstLit])(evaluated)) })))))));
}

export function decodeEdgeLabel<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgModel.EdgeLabel)) {
  return ((g: Graph.Graph) => ((t: Core.Term) => LibEithers.map(((_x: string) => _x))(ExtractCore.string(g)(t))));
}

export function decodeEdgeSpec<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgMapping.EdgeSpec)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => readRecord(cx)(g)(((fields: ReadonlyMap<Core.Name, Core.Term>) => LibEithers.bind(readField(cx)(fields)("label")(((v1: Core.Term) => decodeEdgeLabel(cx)(g)(v1))))(((_a: PgModel.EdgeLabel) => LibEithers.bind(readField(cx)(fields)("id")(((v1: Core.Term) => decodeValueSpec(cx)(g)(v1))))(((_b: PgMapping.ValueSpec) => LibEithers.bind(readField(cx)(fields)("out")(((v1: Core.Term) => decodeValueSpec(cx)(g)(v1))))(((_c: PgMapping.ValueSpec) => LibEithers.bind(readField(cx)(fields)("in")(((v1: Core.Term) => decodeValueSpec(cx)(g)(v1))))(((_d: PgMapping.ValueSpec) => LibEithers.map(((_e: ReadonlyArray<PgMapping.PropertySpec>) => ({
    label: _a,
    id: _b,
    out: _c,
    in: _d,
    properties: _e
  })))(readField(cx)(fields)("properties")(((v1: Core.Term) => expectList(cx)(g)(decodePropertySpec)(v1))))))))))))))(term)));
}

export function decodeElementSpec<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgMapping.ElementSpec)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => readInjection(cx)(g)([["vertex", ((t: Core.Term) => LibEithers.map(((_x: PgMapping.VertexSpec) => ({ tag: "vertex", value: _x })))(decodeVertexSpec(cx)(g)(t)))], ["edge", ((t: Core.Term) => LibEithers.map(((_x: PgMapping.EdgeSpec) => ({ tag: "edge", value: _x })))(decodeEdgeSpec(cx)(g)(t)))]])(term)));
}

export function decodePropertyKey<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgModel.PropertyKey)) {
  return ((g: Graph.Graph) => ((t: Core.Term) => LibEithers.map(((_x: string) => _x))(ExtractCore.string(g)(t))));
}

export function decodePropertySpec<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgMapping.PropertySpec)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => readRecord(cx)(g)(((fields: ReadonlyMap<Core.Name, Core.Term>) => LibEithers.bind(readField(cx)(fields)("key")(((v1: Core.Term) => decodePropertyKey(cx)(g)(v1))))(((_a: PgModel.PropertyKey) => LibEithers.map(((_b: PgMapping.ValueSpec) => ({
    key: _a,
    value: _b
  })))(readField(cx)(fields)("value")(((v1: Core.Term) => decodeValueSpec(cx)(g)(v1))))))))(term)));
}

export function decodeValueSpec<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgMapping.ValueSpec)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: ({ tag: "pattern", value: s }) }))((_m as any).value);
    default: return readInjection(cx)(g)([["value", ((_: Core.Term) => ({ tag: "right", value: ({ tag: "value" }) }))], ["pattern", ((t: Core.Term) => LibEithers.map(((_x: string) => ({ tag: "pattern", value: _x })))(ExtractCore.string(g)(t)))]])(term)(_m);
  }
})())((_m as any).value);
    default: return readInjection(cx)(g)([["value", ((_: Core.Term) => ({ tag: "right", value: ({ tag: "value" }) }))], ["pattern", ((t: Core.Term) => LibEithers.map(((_x: string) => ({ tag: "pattern", value: _x })))(ExtractCore.string(g)(t)))]])(term)(_m);
  }
})()));
}

export function decodeVertexLabel<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgModel.VertexLabel)) {
  return ((g: Graph.Graph) => ((t: Core.Term) => LibEithers.map(((_x: string) => _x))(ExtractCore.string(g)(t))));
}

export function decodeVertexSpec<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | PgMapping.VertexSpec)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => readRecord(cx)(g)(((fields: ReadonlyMap<Core.Name, Core.Term>) => LibEithers.bind(readField(cx)(fields)("label")(((v1: Core.Term) => decodeVertexLabel(cx)(g)(v1))))(((_a: PgModel.VertexLabel) => LibEithers.bind(readField(cx)(fields)("id")(((v1: Core.Term) => decodeValueSpec(cx)(g)(v1))))(((_b: PgMapping.ValueSpec) => LibEithers.map(((_c: ReadonlyArray<PgMapping.PropertySpec>) => ({
    label: _a,
    id: _b,
    properties: _c
  })))(readField(cx)(fields)("properties")(((v1: Core.Term) => expectList(cx)(g)(decodePropertySpec)(v1))))))))))(term)));
}

export function evalPath<t0>(cx: t0): ((x: ReadonlyArray<string>) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>)) {
  return ((path: ReadonlyArray<string>) => ((term: Core.Term) => LibLogic.ifElse(LibLists.null_(path))(({ tag: "right", value: [term] }))(LibEithers.bind(evalStep(cx)(LibLists.head(path))(term))(((results: ReadonlyArray<Core.Term>) => LibEithers.map(((xs: ReadonlyArray<ReadonlyArray<Core.Term>>) => LibLists.concat(xs)))(LibEithers.mapList(((v1: Core.Term) => evalPath(cx)(LibLists.tail(path))(v1)))(results)))))));
}

export function evalStep<t0>(cx: t0): ((x: string) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>)) {
  return ((step: string) => ((term: Core.Term) => LibLogic.ifElse(LibStrings.null_(step))(({ tag: "right", value: [term] }))((() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "list": return ((terms: ReadonlyArray<Core.Term>) => LibEithers.map(((xs: ReadonlyArray<ReadonlyArray<Core.Term>>) => LibLists.concat(xs)))(LibEithers.mapList(((v1: Core.Term) => evalStep(cx)(step)(v1)))(terms)))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: [] }))(((t: Core.Term) => evalStep(cx)(step)(t)))(mt))((_m as any).value);
    case "record": return ((rec: Core.Record) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2(LibStrings.cat2("No such field ")(step))(" in record") }) }))(((t: Core.Term) => ({ tag: "right", value: [t] })))(LibMaps.lookup(step)(Resolution.fieldMap(((_x) => _x.fields)(rec)))))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.name)(((_x) => _x.field)(inj))))(step))(evalStep(cx)(step)(((_x) => _x.term)(((_x) => _x.field)(inj))))(({ tag: "right", value: [] })))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => evalStep(cx)(step)(((_x) => _x.body)(wt)))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("Can't traverse through term for step ")(step) }) })(_m);
  }
})())));
}

export function expectList<t0, t1>(cx: t0): ((x: Graph.Graph) => ((x: ((x: t0) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | t1)))) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t1>))) {
  return ((g: Graph.Graph) => ((f: ((x: t0) => ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | t1)))) => ((term: Core.Term) => LibEithers.bind(ExtractCore.list(g)(term))(((elems: ReadonlyArray<Core.Term>) => LibEithers.mapList(((v1: Core.Term) => f(cx)(g)(v1)))(elems))))));
}

export function parseEdgeIdPattern<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: PgMapping.ValueSpec) => t5 | ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t4>))))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((spec: PgMapping.ValueSpec) => LibEithers.bind(parseValueSpec(cx)(g)(spec))(((fun: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>))) => ({ tag: "right", value: ((cx_: Context.Context) => ((term: Core.Term) => LibEithers.bind(fun(cx_)(term))(((terms: ReadonlyArray<Core.Term>) => LibEithers.mapList(((_x) => _x.encode)(((_x) => _x.edgeIds)(schema))(cx_))(terms))))) }))))));
}

export function parseEdgeSpec<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: PgMapping.EdgeSpec) => t5 | readonly [PgModel.Label, ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t4>>))]))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((spec: PgMapping.EdgeSpec) => (() => {
  const label = ((_x) => _x.label)(spec);
  const id = ((_x) => _x.id)(spec);
  const outV = ((_x) => _x.out)(spec);
  const inV = ((_x) => _x.in)(spec);
  const props = ((_x) => _x.properties)(spec);
  return LibEithers.bind(parseEdgeIdPattern(cx)(g)(schema)(id))(((getId: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t4>))) => LibEithers.bind(parseVertexIdPattern(cx)(g)(schema)(outV))(((getOut: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t4>))) => LibEithers.bind(parseVertexIdPattern(cx)(g)(schema)(inV))(((getIn: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t4>))) => LibEithers.bind(LibEithers.mapList(((v1: PgMapping.PropertySpec) => parsePropertySpec(cx)(g)(schema)(v1)))(props))(((getProps: ReadonlyArray<((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<readonly [PgModel.PropertyKey, t4]>))>) => ({ tag: "right", value: [({ tag: "edge", value: label }), ((cx_: Context.Context) => ((term: Core.Term) => LibEithers.bind(requireUnique(cx_)("edge id")(((v1: Core.Term) => getId(cx_)(v1)))(term))(((tid: t4) => LibEithers.bind(requireUnique(cx_)("vertex id")(((v1: Core.Term) => getOut(cx_)(v1)))(term))(((tout: t4) => LibEithers.bind(requireUnique(cx_)("edge id")(((v1: Core.Term) => getIn(cx_)(v1)))(term))(((tin: t4) => LibEithers.bind(LibEithers.map(((_xs: ReadonlyArray<readonly [PgModel.PropertyKey, t4]>) => LibMaps.fromList(_xs)))(LibEithers.mapList(((gf: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<readonly [PgModel.PropertyKey, t4]>))) => requireUnique(cx_)("property key")(((v1: Core.Term) => gf(cx_)(v1)))(term)))(getProps)))(((tprops: ReadonlyMap<PgModel.PropertyKey, t4>) => ({ tag: "right", value: [({ tag: "edge", value: ({
    label: label,
    id: tid,
    out: tout,
    in: tin,
    properties: tprops
  }) })] })))))))))))] })))))))));
})())));
}

export function parseElementSpec<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: PgMapping.ElementSpec) => t5 | readonly [PgModel.Label, ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t4>>))]))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((spec: PgMapping.ElementSpec) => (() => {
  const _m = spec;
  switch (_m.tag) {
    case "vertex": return ((vspec: PgMapping.VertexSpec) => parseVertexSpec(cx)(g)(schema)(vspec))((_m as any).value);
    case "edge": return ((espec: PgMapping.EdgeSpec) => parseEdgeSpec(cx)(g)(schema)(espec))((_m as any).value);
  }
})())));
}

export function parsePattern<t0, t1, t2, t3>(cx: t0): ((x: t1) => ((x: string) => t2 | ((x: t3) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>)))) {
  return ((_g: t1) => ((pat: string) => (() => {
  const segments = LibStrings.splitOn("${")(pat);
  const firstLit = LibLists.head(segments);
  const rest = LibLists.tail(segments);
  const parsed = LibLists.map(((seg: string) => (() => {
  const parts = LibStrings.splitOn("}")(seg);
  const pathStr = LibLists.head(parts);
  const litPart = LibStrings.intercalate("}")(LibLists.tail(parts));
  const pathSteps = LibStrings.splitOn("/")(pathStr);
  return [pathSteps, litPart];
})()))(rest);
  return ({ tag: "right", value: ((cx_: t3) => ((term: Core.Term) => applyPattern(cx_)(firstLit)(parsed)(term))) });
})()));
}

export function parsePropertySpec<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: PgMapping.PropertySpec) => t5 | ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<readonly [PgModel.PropertyKey, t4]>))))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((spec: PgMapping.PropertySpec) => (() => {
  const key = ((_x) => _x.key)(spec);
  const value = ((_x) => _x.value)(spec);
  return LibEithers.bind(parseValueSpec(cx)(g)(value))(((fun: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>))) => ({ tag: "right", value: ((cx_: Context.Context) => ((term: Core.Term) => LibEithers.bind(fun(cx_)(term))(((results: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(((_x) => _x.encode)(((_x) => _x.propertyValues)(schema))(cx_))(results))(((values: ReadonlyArray<t4>) => ({ tag: "right", value: LibLists.map(((v: t4) => [key, v]))(values) }))))))) })));
})())));
}

export function parseValueSpec<t0, t1, t2, t3>(cx: t0): ((x: t1) => ((x: PgMapping.ValueSpec) => t2 | ((x: t3) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>)))) {
  return ((g: t1) => ((spec: PgMapping.ValueSpec) => (() => {
  const _m = spec;
  switch (_m.tag) {
    case "value": return ((_: void) => ({ tag: "right", value: ((_cx: t3) => ((term: Core.Term) => ({ tag: "right", value: [term] }))) }))((_m as any).value);
    case "pattern": return ((pat: string) => parsePattern(cx)(g)(pat))((_m as any).value);
  }
})()));
}

export function parseVertexIdPattern<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: PgMapping.ValueSpec) => t5 | ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t4>))))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((spec: PgMapping.ValueSpec) => LibEithers.bind(parseValueSpec(cx)(g)(spec))(((fun: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<Core.Term>))) => ({ tag: "right", value: ((cx_: Context.Context) => ((term: Core.Term) => LibEithers.bind(fun(cx_)(term))(((terms: ReadonlyArray<Core.Term>) => LibEithers.mapList(((_x) => _x.encode)(((_x) => _x.vertexIds)(schema))(cx_))(terms))))) }))))));
}

export function parseVertexSpec<t0, t1, t2, t3, t4, t5>(cx: t0): ((x: t1) => ((x: PgMapping.Schema<t2, t3, t4>) => ((x: PgMapping.VertexSpec) => t5 | readonly [PgModel.Label, ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t4>>))]))) {
  return ((g: t1) => ((schema: PgMapping.Schema<t2, t3, t4>) => ((spec: PgMapping.VertexSpec) => (() => {
  const label = ((_x) => _x.label)(spec);
  const id = ((_x) => _x.id)(spec);
  const props = ((_x) => _x.properties)(spec);
  return LibEithers.bind(parseVertexIdPattern(cx)(g)(schema)(id))(((getId: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<t4>))) => LibEithers.bind(LibEithers.mapList(((v1: PgMapping.PropertySpec) => parsePropertySpec(cx)(g)(schema)(v1)))(props))(((getProps: ReadonlyArray<((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<readonly [PgModel.PropertyKey, t4]>))>) => ({ tag: "right", value: [({ tag: "vertex", value: label }), ((cx_: Context.Context) => ((term: Core.Term) => LibEithers.bind(requireUnique(cx_)("vertex id")(((v1: Core.Term) => getId(cx_)(v1)))(term))(((tid: t4) => LibEithers.bind(LibEithers.map(((_xs: ReadonlyArray<readonly [PgModel.PropertyKey, t4]>) => LibMaps.fromList(_xs)))(LibEithers.mapList(((gf: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<readonly [PgModel.PropertyKey, t4]>))) => requireUnique(cx_)("property key")(((v1: Core.Term) => gf(cx_)(v1)))(term)))(getProps)))(((tprops: ReadonlyMap<PgModel.PropertyKey, t4>) => ({ tag: "right", value: [({ tag: "vertex", value: ({
    label: label,
    id: tid,
    properties: tprops
  }) })] })))))))] })))));
})())));
}

export function readField<t0, t1, t2>(cx: t0): ((x: ReadonlyMap<Core.Name, t1>) => ((x: Core.Name) => ((x: ((x: t1) => Errors.Error | t2)) => Errors.Error | t2))) {
  return ((fields: ReadonlyMap<Core.Name, t1>) => ((fname: Core.Name) => ((fun: ((x: t1) => Errors.Error | t2)) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("no such field: ")(((_x) => _x)(fname)) }) }))(fun)(LibMaps.lookup(fname)(fields)))));
}

export function readInjection<t0, t1>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<readonly [Core.Name, ((x: Core.Term) => Errors.Error | t1)]>) => ((x: Core.Term) => Errors.Error | t1))) {
  return ((g: Graph.Graph) => ((cases: ReadonlyArray<readonly [Core.Name, ((x: Core.Term) => Errors.Error | t1)]>) => ((encoded: Core.Term) => LibEithers.bind(ExtractCore.map(((k: Core.Term) => LibEithers.map(((_n: string) => _n))(ExtractCore.string(g)(k))))(((_v: Core.Term) => ({ tag: "right", value: _v })))(g)(encoded))(((mp: ReadonlyMap<Core.Name, Core.Term>) => (() => {
  const entries = LibMaps.toList(mp);
  return LibLogic.ifElse(LibLists.null_(entries))(({ tag: "left", value: ({ tag: "other", value: "empty injection" }) }))((() => {
  const f = LibLists.head(entries);
  const key = LibPairs.first(f);
  const val = LibPairs.second(f);
  const matching = LibLists.filter(((c: readonly [Core.Name, ((x: Core.Term) => Errors.Error | t1)]) => LibEquality.equal(LibPairs.first(c))(key)))(cases);
  return LibLogic.ifElse(LibLists.null_(matching))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("unexpected field: ")(((_x) => _x)(key)) }) }))(LibPairs.second(LibLists.head(matching))(val));
})());
})())))));
}

export function readRecord<t0, t1>(cx: t0): ((x: Graph.Graph) => ((x: ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | t1)) => ((x: Core.Term) => Errors.Error | t1))) {
  return ((g: Graph.Graph) => ((cons: ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | t1)) => ((term: Core.Term) => LibEithers.bind(ExtractCore.map(((k: Core.Term) => LibEithers.map(((_n: string) => _n))(ExtractCore.string(g)(k))))(((_v: Core.Term) => ({ tag: "right", value: _v })))(g)(term))(cons))));
}

export function requireUnique<t0, t1, t2>(cx: t0): ((x: string) => ((x: ((x: t1) => Errors.Error | ReadonlyArray<t2>)) => ((x: t1) => Errors.Error | t2))) {
  return ((context: string) => ((fun: ((x: t1) => Errors.Error | ReadonlyArray<t2>)) => ((term: t1) => LibEithers.bind(fun(term))(((results: ReadonlyArray<t2>) => LibLogic.ifElse(LibLists.null_(results))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("No value found: ")(context) }) }))(LibLogic.ifElse(LibEquality.equal(LibLists.length(results))(1))(({ tag: "right", value: LibLists.head(results) }))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("Multiple values found: ")(context) }) }))))))));
}

export function termToElementsAdapter<t0, t1, t2, t3>(cx: t0): ((x: Graph.Graph) => ((x: PgMapping.Schema<t1, t2, t3>) => ((x: Core.Type) => Errors.Error | Coders.Adapter<Core.Type, ReadonlyArray<PgModel.Label>, Core.Term, ReadonlyArray<PgModel.Element<t3>>>))) {
  return ((g: Graph.Graph) => ((schema: PgMapping.Schema<t1, t2, t3>) => ((typ: Core.Type) => (() => {
  const key_elements = "elements";
  return LibMaybes.maybe(({ tag: "right", value: ({
    isLossy: false,
    source: typ,
    target: [],
    coder: ({
    encode: ((_cx: Context.Context) => ((_t: Core.Term) => ({ tag: "right", value: [] }))),
    decode: ((cx_: Context.Context) => ((_els: ReadonlyArray<PgModel.Element<t3>>) => ({ tag: "left", value: ({ tag: "other", value: "no corresponding element type" }) })))
  })
  }) }))(((term: Core.Term) => LibEithers.bind(expectList(cx)(g)(decodeElementSpec)(term))(((specTerms: ReadonlyArray<PgMapping.ElementSpec>) => LibEithers.bind(LibEithers.mapList(((v1: PgMapping.ElementSpec) => parseElementSpec(cx)(g)(schema)(v1)))(specTerms))(((specs: ReadonlyArray<readonly [PgModel.Label, ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t3>>))]>) => (() => {
  const labels = LibLists.nub(LibLists.map(((_p: readonly [PgModel.Label, ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t3>>))]) => LibPairs.first(_p)))(specs));
  const encoders = LibLists.map(((_p: readonly [PgModel.Label, ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t3>>))]) => LibPairs.second(_p)))(specs);
  return ({ tag: "right", value: ({
    isLossy: false,
    source: typ,
    target: labels,
    coder: ({
    encode: ((cx_: Context.Context) => ((t: Core.Term) => LibEithers.map(((_xs: ReadonlyArray<ReadonlyArray<PgModel.Element<t3>>>) => LibLists.concat(_xs)))(LibEithers.mapList(((e: ((x: Context.Context) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PgModel.Element<t3>>))) => e(cx_)(t)))(encoders)))),
    decode: ((cx_: Context.Context) => ((_els: ReadonlyArray<PgModel.Element<t3>>) => ({ tag: "left", value: ({ tag: "other", value: "element decoding is not yet supported" }) })))
  })
  }) });
})()))))))(Annotations.getTypeAnnotation(key_elements)(typ));
})())));
}

export function termToString(term: Core.Term): string {
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "string": return ((s: string) => s)((_m as any).value);
    case "boolean": return ((b: boolean) => LibLogic.ifElse(b)("true")("false"))((_m as any).value);
    case "integer": return ((i: Core.IntegerValue) => (() => {
  const _m = i;
  switch (_m.tag) {
    case "int32": return ((n: number) => LibLiterals.showInt32(n))((_m as any).value);
    default: return ShowCore.term(term)(_m);
  }
})())((_m as any).value);
    case "float": return ((f: Core.FloatValue) => (() => {
  const _m = f;
  switch (_m.tag) {
    case "float64": return ((n: number) => LibLiterals.showFloat64(n))((_m as any).value);
    default: return ShowCore.term(term)(_m);
  }
})())((_m as any).value);
    default: return ShowCore.term(term)(_m);
  }
})())((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe("nothing")(((t: Core.Term) => termToString(t)))(mt))((_m as any).value);
    default: return ShowCore.term(term)(_m);
  }
})();
}
