// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints and reserved words for Python 3
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
import * as LibLists from "../lib/lists.js";
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

export const pythonLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.fromList([({ tag: "record" }), ({ tag: "union" }), ({ tag: "wrap" })]);
  const literalVariants = LibSets.fromList([({ tag: "binary" }), ({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })]);
  const floatTypes = LibSets.fromList([({ tag: "bigfloat" }), ({ tag: "float64" })]);
  const functionVariants = LibSets.fromList([({ tag: "elimination" }), ({ tag: "lambda" })]);
  const integerTypes = LibSets.fromList([({ tag: "bigint" })]);
  const termVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "cases" }), ({ tag: "lambda" }), ({ tag: "project" }), ({ tag: "unwrap" }), ({ tag: "let" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "typeApplication" }), ({ tag: "typeLambda" }), ({ tag: "inject" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "wrap" })]);
  const typeVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "function" }), ({ tag: "forall" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "void" }), ({ tag: "wrap" })]);
  const typePredicate = ((_: t0) => true);
  return ({
    name: "hydra.python",
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

export const pythonReservedWords: ReadonlySet<string> = (() => {
  const pythonKeywords = ["False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield"];
  const pythonBuiltInFunctions = ["range"];
  const hydraPythonKeywords = ["Node", "FrozenDict"];
  return LibSets.fromList(LibLists.concat([pythonKeywords, pythonBuiltInFunctions, hydraPythonKeywords]));
})();
