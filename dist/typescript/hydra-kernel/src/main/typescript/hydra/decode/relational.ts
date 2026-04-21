// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.relational
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Relational from "../relational.js";
import * as Rewriting from "../rewriting.js";
import * as Util from "../util.js";

export function columnName(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Relational.ColumnName) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: string) => b))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(((_x) => _x.body)(wrappedTerm)))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function columnSchema<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Relational.ColumnSchema<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(columnName)(fieldMap)(cx))(((field_name: Relational.ColumnName) => LibEithers.bind(ExtractCore.requireField("domain")(t)(fieldMap)(cx))(((field_domain: t0) => ({ tag: "right", value: ({
    name: field_name,
    domain: field_domain
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function foreignKey(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Relational.ForeignKey) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("foreignRelation")(relationName)(fieldMap)(cx))(((field_foreignRelation: Relational.RelationName) => LibEithers.bind(ExtractCore.requireField("keys")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(columnName)(columnName)(v1)(v2))))(fieldMap)(cx))(((field_keys: ReadonlyMap<Relational.ColumnName, Relational.ColumnName>) => ({ tag: "right", value: ({
    foreignRelation: field_foreignRelation,
    keys: field_keys
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function primaryKey(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Relational.PrimaryKey) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<Relational.ColumnName>) => b))(ExtractCore.decodeList(columnName)(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function relation<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Relational.Relation<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<Relational.Row<t0>>) => b))(ExtractCore.decodeList(((v1: Graph.Graph) => ((v2: Core.Term) => row(v)(v1)(v2))))(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function relationName(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Relational.RelationName) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: string) => b))(LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
  const _m = stripped2;
  switch (_m.tag) {
    case "literal": return ((v: Core.Literal) => (() => {
  const _m = v;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "right", value: s }))((_m as any).value);
    default: return ({ tag: "left", value: "expected string literal" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected literal" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(((_x) => _x.body)(wrappedTerm)))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function relationSchema<t0>(t: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Relational.RelationSchema<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(relationName)(fieldMap)(cx))(((field_name: Relational.RelationName) => LibEithers.bind(ExtractCore.requireField("columns")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => columnSchema(t)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_columns: ReadonlyArray<Relational.ColumnSchema<t0>>) => LibEithers.bind(ExtractCore.requireField("primaryKeys")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(primaryKey)(v1)(v2))))(fieldMap)(cx))(((field_primaryKeys: ReadonlyArray<Relational.PrimaryKey>) => LibEithers.bind(ExtractCore.requireField("foreignKeys")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(foreignKey)(v1)(v2))))(fieldMap)(cx))(((field_foreignKeys: ReadonlyArray<Relational.ForeignKey>) => ({ tag: "right", value: ({
    name: field_name,
    columns: field_columns,
    primaryKeys: field_primaryKeys,
    foreignKeys: field_foreignKeys
  }) })))))))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function relationship<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Relational.Relationship<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlySet<ReadonlyMap<Relational.ColumnName, t0>>) => b))(ExtractCore.decodeSet(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMap(columnName)(v)(v1)(v2))))(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function row<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Relational.Row<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<t0>) => b))(ExtractCore.decodeList(v)(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}
