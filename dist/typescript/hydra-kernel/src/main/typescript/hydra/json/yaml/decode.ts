// Note: this is an automatically generated file. Do not edit.

/**
 * YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as JsonDecode from "../decode.js";
import * as JsonModel from "../model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibLiterals from "../../lib/literals.js";
import * as LibMaps from "../../lib/maps.js";
import * as LibPairs from "../../lib/pairs.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";
import * as YamlModel from "../../yaml/model.js";

export function fromYaml(types: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Name) => ((x: Core.Type) => ((x: YamlModel.Node) => string | Core.Term))) {
  return ((tname: Core.Name) => ((typ: Core.Type) => ((node: YamlModel.Node) => (() => {
  const jsonResult = yamlToJson(node);
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((json: JsonModel.Value) => JsonDecode.fromJson(types)(tname)(typ)(json)))(jsonResult);
})())));
}

export function yamlToJson(node: YamlModel.Node): string | JsonModel.Value {
  return (() => {
  const _m = node;
  switch (_m.tag) {
    case "mapping": return ((m: ReadonlyMap<YamlModel.Node, YamlModel.Node>) => (() => {
  const convertEntry = ((kv: readonly [YamlModel.Node, YamlModel.Node]) => (() => {
  const keyNode = LibPairs.first(kv);
  return (() => {
  const valNode = LibPairs.second(kv);
  return (() => {
  const keyResult = (() => {
  const _m = keyNode;
  switch (_m.tag) {
    case "scalar": return ((s: YamlModel.Scalar) => (() => {
  const _m = s;
  switch (_m.tag) {
    case "str": return ((str: string) => ({ tag: "right", value: str }))((_m as any).value);
    default: return ({ tag: "left", value: "non-string YAML mapping key" })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "left", value: "non-scalar YAML mapping key" })(_m);
  }
})();
  return LibEithers.either(((err: string) => ({ tag: "left", value: err })))(((key: string) => (() => {
  const valResult = yamlToJson(valNode);
  return LibEithers.map(((v: JsonModel.Value) => [key, v]))(valResult);
})()))(keyResult);
})();
})();
})());
  return (() => {
  const entries = LibEithers.mapList(convertEntry)(LibMaps.toList(m));
  return LibEithers.map(((es: ReadonlyArray<readonly [string, JsonModel.Value]>) => ({ tag: "object", value: LibMaps.fromList(es) })))(entries);
})();
})())((_m as any).value);
    case "scalar": return ((s: YamlModel.Scalar) => (() => {
  const _m = s;
  switch (_m.tag) {
    case "bool": return ((b: boolean) => ({ tag: "right", value: ({ tag: "boolean", value: b }) }))((_m as any).value);
    case "float": return ((f: number) => ({ tag: "right", value: ({ tag: "number", value: f }) }))((_m as any).value);
    case "int": return ((i: bigint) => ({ tag: "right", value: ({ tag: "number", value: LibLiterals.bigintToBigfloat(i) }) }))((_m as any).value);
    case "null": return ((_: void) => ({ tag: "right", value: ({ tag: "null" }) }))((_m as any).value);
    case "str": return ((str: string) => ({ tag: "right", value: ({ tag: "string", value: str }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "sequence": return ((nodes: ReadonlyArray<YamlModel.Node>) => (() => {
  const results = LibEithers.mapList(((n: YamlModel.Node) => yamlToJson(n)))(nodes);
  return LibEithers.map(((vs: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: vs })))(results);
})())((_m as any).value);
  }
})();
}
