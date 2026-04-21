// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints and reserved words for Haskell
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

export const haskellLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.fromList([({ tag: "record" }), ({ tag: "union" }), ({ tag: "wrap" })]);
  const literalVariants = LibSets.fromList([({ tag: "binary" }), ({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })]);
  const floatTypes = LibSets.fromList([({ tag: "float32" }), ({ tag: "float64" })]);
  const functionVariants = LibSets.fromList([({ tag: "elimination" }), ({ tag: "lambda" })]);
  const integerTypes = LibSets.fromList([({ tag: "bigint" }), ({ tag: "int8" }), ({ tag: "int16" }), ({ tag: "int32" }), ({ tag: "int64" })]);
  const termVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "cases" }), ({ tag: "either" }), ({ tag: "lambda" }), ({ tag: "let" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "project" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "typeApplication" }), ({ tag: "typeLambda" }), ({ tag: "inject" }), ({ tag: "unit" }), ({ tag: "unwrap" }), ({ tag: "variable" }), ({ tag: "wrap" })]);
  const typeVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "function" }), ({ tag: "forall" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "void" }), ({ tag: "wrap" })]);
  const typePredicate = ((_: t0) => true);
  return ({
    name: "hydra.haskell",
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

export const reservedWords: ReadonlySet<string> = (() => {
  const keywordSymbols = ["case", "class", "data", "default", "deriving", "do", "else", "forall", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where"];
  const reservedSymbols = ["Bool", "Double", "False", "Float", "Int", "Integer", "Just", "Maybe", "Nothing", "Ord", "Show", "String", "True"];
  return LibSets.fromList(LibLists.concat2(keywordSymbols)(reservedSymbols));
})();
