// Note: this is an automatically generated file. Do not edit.

/**
 * String representations of hydra.variants types
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

export function termVariant(v1: Variants.TermVariant): string {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((_: void) => "annotated")((_m as any).value);
    case "application": return ((_: void) => "application")((_m as any).value);
    case "cases": return ((_: void) => "cases")((_m as any).value);
    case "either": return ((_: void) => "either")((_m as any).value);
    case "lambda": return ((_: void) => "lambda")((_m as any).value);
    case "let": return ((_: void) => "let")((_m as any).value);
    case "list": return ((_: void) => "list")((_m as any).value);
    case "literal": return ((_: void) => "literal")((_m as any).value);
    case "map": return ((_: void) => "map")((_m as any).value);
    case "maybe": return ((_: void) => "maybe")((_m as any).value);
    case "pair": return ((_: void) => "pair")((_m as any).value);
    case "project": return ((_: void) => "project")((_m as any).value);
    case "record": return ((_: void) => "record")((_m as any).value);
    case "set": return ((_: void) => "set")((_m as any).value);
    case "typeLambda": return ((_: void) => "typeLambda")((_m as any).value);
    case "typeApplication": return ((_: void) => "typeApplication")((_m as any).value);
    case "inject": return ((_: void) => "inject")((_m as any).value);
    case "unit": return ((_: void) => "unit")((_m as any).value);
    case "unwrap": return ((_: void) => "unwrap")((_m as any).value);
    case "variable": return ((_: void) => "variable")((_m as any).value);
    case "wrap": return ((_: void) => "wrap")((_m as any).value);
  }
})();
}

export function typeVariant(v1: Variants.TypeVariant): string {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "annotated": return ((_: void) => "annotated")((_m as any).value);
    case "application": return ((_: void) => "application")((_m as any).value);
    case "either": return ((_: void) => "either")((_m as any).value);
    case "forall": return ((_: void) => "forall")((_m as any).value);
    case "function": return ((_: void) => "function")((_m as any).value);
    case "list": return ((_: void) => "list")((_m as any).value);
    case "literal": return ((_: void) => "literal")((_m as any).value);
    case "map": return ((_: void) => "map")((_m as any).value);
    case "maybe": return ((_: void) => "maybe")((_m as any).value);
    case "pair": return ((_: void) => "pair")((_m as any).value);
    case "record": return ((_: void) => "record")((_m as any).value);
    case "set": return ((_: void) => "set")((_m as any).value);
    case "union": return ((_: void) => "union")((_m as any).value);
    case "unit": return ((_: void) => "unit")((_m as any).value);
    case "variable": return ((_: void) => "variable")((_m as any).value);
    case "void": return ((_: void) => "void")((_m as any).value);
    case "wrap": return ((_: void) => "wrap")((_m as any).value);
  }
})();
}
