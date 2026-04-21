// Note: this is an automatically generated file. Do not edit.

/**
 * Term decoders for hydra.tabular
 */



import * as Core from "../core.js";
import * as DecodeCore from "./core.js";
import * as DecodeRelational from "./relational.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "../extract/core.js";
import * as Graph from "../graph.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Rewriting from "../rewriting.js";
import * as Tabular from "../tabular.js";
import * as Util from "../util.js";

export function columnType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Tabular.ColumnType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeRelational.columnName)(fieldMap)(cx))(((field_name: hydra.relational.ColumnName) => LibEithers.bind(ExtractCore.requireField("type")(DecodeCore.type)(fieldMap)(cx))(((field_type: Core.Type) => ({ tag: "right", value: ({
    name: field_name,
    type: field_type
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function dataRow<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Tabular.DataRow<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<t0 | null>) => b))(ExtractCore.decodeList(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(v)(v1)(v2))))(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function headerRow(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Tabular.HeaderRow) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "wrap": return ((wrappedTerm: Core.WrappedTerm) => LibEithers.map(((b: ReadonlyArray<string>) => b))(ExtractCore.decodeList(((cx2: Graph.Graph) => ((raw2: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped2: Core.Term) => (() => {
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
})()))(ExtractCore.stripWithDecodingError(cx2)(raw2)))))(cx)(((_x) => _x.body)(wrappedTerm))))((_m as any).value);
    default: return ({ tag: "left", value: "expected wrapped type" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}

export function table<t0>(v: ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | t0))): ((x: Graph.Graph) => ((x: Core.Term) => Errors.DecodingError | Tabular.Table<t0>)) {
  return ((cx: Graph.Graph) => ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("header")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeMaybe(headerRow)(v1)(v2))))(fieldMap)(cx))(((field_header: Tabular.HeaderRow | null) => LibEithers.bind(ExtractCore.requireField("data")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(((v12: Graph.Graph) => ((v22: Core.Term) => dataRow(v)(v12)(v22))))(v1)(v2))))(fieldMap)(cx))(((field_data: ReadonlyArray<Tabular.DataRow<t0>>) => ({ tag: "right", value: ({
    header: field_header,
    data: field_data
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw))));
}

export function tableType(cx: Graph.Graph): ((x: Core.Term) => Errors.DecodingError | Tabular.TableType) {
  return ((raw: Core.Term) => LibEithers.either(((err: Errors.DecodingError) => ({ tag: "left", value: err })))(((stripped: Core.Term) => (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "record": return ((record: Core.Record) => (() => {
  const fieldMap = ExtractCore.toFieldMap(record);
  return LibEithers.bind(ExtractCore.requireField("name")(DecodeRelational.relationName)(fieldMap)(cx))(((field_name: hydra.relational.RelationName) => LibEithers.bind(ExtractCore.requireField("columns")(((v1: Graph.Graph) => ((v2: Core.Term) => ExtractCore.decodeList(columnType)(v1)(v2))))(fieldMap)(cx))(((field_columns: ReadonlyArray<Tabular.ColumnType>) => ({ tag: "right", value: ({
    name: field_name,
    columns: field_columns
  }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: "expected record" })(_m);
  }
})()))(ExtractCore.stripWithDecodingError(cx)(raw)));
}
