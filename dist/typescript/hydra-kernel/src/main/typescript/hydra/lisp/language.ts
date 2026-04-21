// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints and reserved words for Lisp (covering Clojure, Emacs Lisp, Common Lisp, and Scheme)
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

export const lispLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.fromList([({ tag: "record" }), ({ tag: "union" }), ({ tag: "wrap" })]);
  const literalVariants = LibSets.fromList([({ tag: "binary" }), ({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })]);
  const floatTypes = LibSets.fromList([({ tag: "bigfloat" }), ({ tag: "float64" })]);
  const functionVariants = LibSets.fromList([({ tag: "elimination" }), ({ tag: "lambda" })]);
  const integerTypes = LibSets.fromList([({ tag: "bigint" })]);
  const termVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "cases" }), ({ tag: "lambda" }), ({ tag: "project" }), ({ tag: "unwrap" }), ({ tag: "typeApplication" }), ({ tag: "typeLambda" }), ({ tag: "let" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "inject" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "wrap" })]);
  const typeVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "function" }), ({ tag: "forall" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "void" }), ({ tag: "wrap" })]);
  const typePredicate = ((_: t0) => true);
  return ({
    name: "hydra.lisp",
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

export const lispReservedWords: ReadonlySet<string> = (() => {
  const clojureKeywords = ["def", "defn", "defn-", "defmacro", "defrecord", "deftype", "defprotocol", "defmulti", "defmethod", "fn", "let", "loop", "recur", "if", "do", "cond", "case", "when", "when-not", "when-let", "if-let", "and", "or", "not", "nil", "true", "false", "throw", "try", "catch", "finally", "quote", "var", "ns", "require", "import", "use", "in-ns", "refer", "new", "set!", "monitor-enter", "monitor-exit"];
  const emacsLispKeywords = ["defun", "defvar", "defconst", "defmacro", "defsubst", "defadvice", "defcustom", "defgroup", "lambda", "let", "let*", "if", "cond", "progn", "prog1", "prog2", "while", "dolist", "dotimes", "and", "or", "not", "nil", "t", "quote", "function", "setq", "setq-default", "require", "provide", "condition-case", "unwind-protect", "save-excursion", "save-restriction", "catch", "throw", "interactive", "cl-defstruct", "cl-case", "cl-loop", "cl-labels", "cl-flet", "pcase", "pcase-let", "seq-let"];
  const commonLispKeywords = ["defun", "defvar", "defparameter", "defconstant", "defmacro", "defgeneric", "defmethod", "defclass", "defstruct", "deftype", "defpackage", "defsetf", "lambda", "let", "let*", "flet", "labels", "macrolet", "symbol-macrolet", "if", "cond", "case", "typecase", "etypecase", "ecase", "progn", "prog1", "prog2", "block", "return-from", "tagbody", "go", "and", "or", "not", "nil", "t", "quote", "function", "setq", "setf", "do", "do*", "dolist", "dotimes", "loop", "values", "multiple-value-bind", "multiple-value-list", "the", "declare", "declaim", "proclaim", "in-package", "use-package", "export", "import", "intern", "handler-case", "handler-bind", "restart-case", "condition", "unwind-protect", "catch", "throw", "eval-when", "load-time-value", "locally", "the", "pi"];
  const schemeKeywords = ["define", "define-syntax", "define-record-type", "define-library", "define-values", "lambda", "let", "let*", "letrec", "letrec*", "let-values", "let*-values", "if", "cond", "case", "when", "unless", "and", "or", "not", "begin", "do", "quote", "quasiquote", "unquote", "unquote-splicing", "set!", "import", "export", "include", "include-ci", "syntax-rules", "syntax-case", "with-syntax", "call-with-current-continuation", "call/cc", "call-with-values", "values", "dynamic-wind", "guard", "parameterize", "else"];
  const hydraLispKeywords = ["Node"];
  return LibSets.fromList(LibLists.concat([clojureKeywords, emacsLispKeywords, commonLispKeywords, schemeKeywords, hydraLispKeywords]));
})();
