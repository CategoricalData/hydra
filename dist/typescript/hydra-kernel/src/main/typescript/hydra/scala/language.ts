// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints and reserved words for Scala
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

export const scalaLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.fromList([({ tag: "record" }), ({ tag: "union" }), ({ tag: "wrap" })]);
  const literalVariants = LibSets.fromList([({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })]);
  const floatTypes = LibSets.fromList([({ tag: "bigfloat" }), ({ tag: "float32" }), ({ tag: "float64" })]);
  const functionVariants = LibSets.fromList([({ tag: "elimination" }), ({ tag: "lambda" })]);
  const integerTypes = LibSets.fromList([({ tag: "bigint" }), ({ tag: "int8" }), ({ tag: "int16" }), ({ tag: "int32" }), ({ tag: "int64" }), ({ tag: "uint8" }), ({ tag: "uint16" }), ({ tag: "uint32" }), ({ tag: "uint64" })]);
  const termVariants = LibSets.fromList([({ tag: "application" }), ({ tag: "either" }), ({ tag: "cases" }), ({ tag: "lambda" }), ({ tag: "project" }), ({ tag: "unwrap" }), ({ tag: "typeApplication" }), ({ tag: "typeLambda" }), ({ tag: "let" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "inject" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "wrap" })]);
  const typeVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "function" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" }), ({ tag: "unit" }), ({ tag: "forall" }), ({ tag: "variable" }), ({ tag: "void" }), ({ tag: "wrap" })]);
  const typePredicate = ((_: t0) => true);
  return ({
    name: "hydra.scala",
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

export const scalaReservedWords: ReadonlySet<string> = (() => {
  const keywords = ["abstract", "case", "catch", "class", "def", "do", "else", "end", "enum", "export", "extends", "false", "final", "finally", "for", "forSome", "given", "if", "implicit", "import", "lazy", "macro", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "then", "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield"];
  const classNames = ["Any", "AnyVal", "App", "Array", "Boolean", "Byte", "Char", "Console", "DelayedInit", "Double", "DummyExplicit", "Dynamic", "Enumeration", "Equals", "Float", "Function", "Int", "Long", "MatchError", "None", "Nothing", "Null", "Option", "PartialFunction", "Predef", "Product", "Proxy", "SerialVersionUID", "Short", "Singleton", "Some", "Specializable", "StringContext", "Symbol", "Unit", "ValueOf"];
  const hydraScalaKeywords = [];
  return LibSets.fromList(LibLists.concat([keywords, classNames, hydraScalaKeywords]));
})();
