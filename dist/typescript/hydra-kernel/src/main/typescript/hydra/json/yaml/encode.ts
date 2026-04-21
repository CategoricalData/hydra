// Note: this is an automatically generated file. Do not edit.

/**
 * JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.
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
import * as JsonEncode from "../encode.js";
import * as JsonModel from "../model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibLists from "../../lib/lists.js";
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

export function jsonToYaml(value: JsonModel.Value): YamlModel.Node {
  return (() => {
  const _m = value;
  switch (_m.tag) {
    case "array": return ((arr: ReadonlyArray<JsonModel.Value>) => ({ tag: "sequence", value: LibLists.map(((v: JsonModel.Value) => jsonToYaml(v)))(arr) }))((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "scalar", value: ({ tag: "bool", value: b }) }))((_m as any).value);
    case "null": return ((_: void) => ({ tag: "scalar", value: ({ tag: "null" }) }))((_m as any).value);
    case "number": return ((n: number) => ({ tag: "scalar", value: ({ tag: "float", value: n }) }))((_m as any).value);
    case "object": return ((obj: ReadonlyMap<string, JsonModel.Value>) => ({ tag: "mapping", value: LibMaps.fromList(LibLists.map(((kv: readonly [string, JsonModel.Value]) => [({ tag: "scalar", value: ({ tag: "str", value: LibPairs.first(kv) }) }), jsonToYaml(LibPairs.second(kv))]))(LibMaps.toList(obj))) }))((_m as any).value);
    case "string": return ((s: string) => ({ tag: "scalar", value: ({ tag: "str", value: s }) }))((_m as any).value);
  }
})();
}

export function toYaml(types: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Name) => ((x: Core.Type) => ((x: Core.Term) => string | YamlModel.Node))) {
  return ((tname: Core.Name) => ((typ: Core.Type) => ((term: Core.Term) => LibEithers.map(((v: JsonModel.Value) => jsonToYaml(v)))(JsonEncode.toJson(types)(tname)(typ)(term)))));
}
