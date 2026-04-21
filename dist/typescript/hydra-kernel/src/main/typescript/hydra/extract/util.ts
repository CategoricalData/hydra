// Note: this is an automatically generated file. Do not edit.

/**
 * Extraction and validation for hydra.util types
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as ExtractCore from "./core.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLogic from "../lib/logic.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowErrors from "../show/errors.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function comparison<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Util.Comparison)) {
  return ((graph: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(ExtractCore.unitVariant("hydra.util.Comparison")(graph)(term))(((fname: Core.Name) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(fname))("equalTo"))(({ tag: "right", value: ({ tag: "equalTo" }) }))(LibLogic.ifElse(LibEquality.equal(((_x) => _x)(fname))("lessThan"))(({ tag: "right", value: ({ tag: "lessThan" }) }))(LibLogic.ifElse(LibEquality.equal(((_x) => _x)(fname))("greaterThan"))(({ tag: "right", value: ({ tag: "greaterThan" }) }))(({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "comparison",
    actual: ((_x) => _x)(fname)
  }) }) }) }))))))));
}
