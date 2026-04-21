// Note: this is an automatically generated file. Do not edit.

/**
 * String representations of hydra.util types
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
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function caseConvention(c: Util.CaseConvention): string {
  return (() => {
  const _m = c;
  switch (_m.tag) {
    case "lowerSnake": return ((_: void) => "lower_snake_case")((_m as any).value);
    case "upperSnake": return ((_: void) => "UPPER_SNAKE_CASE")((_m as any).value);
    case "camel": return ((_: void) => "camelCase")((_m as any).value);
    case "pascal": return ((_: void) => "PascalCase")((_m as any).value);
  }
})();
}

export function comparison(c: Util.Comparison): string {
  return (() => {
  const _m = c;
  switch (_m.tag) {
    case "lessThan": return ((_: void) => "lessThan")((_m as any).value);
    case "equalTo": return ((_: void) => "equalTo")((_m as any).value);
    case "greaterThan": return ((_: void) => "greaterThan")((_m as any).value);
  }
})();
}
