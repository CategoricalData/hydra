// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints for W3C SHACL
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
import * as Lexical from "../lexical.js";
import * as LibSets from "../lib/sets.js";
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

export const shaclLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.empty;
  const literalVariants = LibSets.fromList([({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })]);
  const floatTypes = LibSets.fromList([({ tag: "float32" }), ({ tag: "float64" })]);
  const functionVariants = LibSets.empty;
  const integerTypes = LibSets.fromList([({ tag: "int32" }), ({ tag: "int64" })]);
  const termVariants = LibSets.fromList([({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "wrap" }), ({ tag: "maybe" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "inject" })]);
  const typeVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "wrap" }), ({ tag: "maybe" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" })]);
  const typePredicate = ((_: t0) => true);
  return ({
    name: "hydra.shacl",
    constraints: ({
    eliminationVariants: eliminationVariants,
    literalVariants: literalVariants,
    floatTypes: floatTypes,
    functionVariants: functionVariants,
    integerTypes: integerTypes,
    termVariants: termVariants,
    typeVariants: typeVariants,
    types: typePredicate
  })
  });
})();
