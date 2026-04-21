// Note: this is an automatically generated file. Do not edit.

/**
 * String formatting types and functions.
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibChars from "./lib/chars.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function capitalize(v1: string): string {
  return mapFirstLetter(LibStrings.toUpper)(v1);
}

export function convertCase(from: Util.CaseConvention): ((x: Util.CaseConvention) => ((x: string) => string)) {
  return ((to: Util.CaseConvention) => ((original: string) => (() => {
  const parts = (() => {
  const byCaps = (() => {
  const splitOnUppercase = ((acc: ReadonlyArray<ReadonlyArray<number>>) => ((c: number) => LibLists.concat2(LibLogic.ifElse(LibChars.isUpper(c))([[]])([]))(LibLists.cons(LibLists.cons(c)(LibLists.head(acc)))(LibLists.tail(acc)))));
  return LibLists.map(LibStrings.fromList)(LibLists.foldl(splitOnUppercase)([[]])(LibLists.reverse(LibStrings.toList(decapitalize(original)))));
})();
  const byUnderscores = LibStrings.splitOn("_")(original);
  return (() => {
  const _m = from;
  switch (_m.tag) {
    case "camel": return ((_: void) => byCaps)((_m as any).value);
    case "pascal": return ((_: void) => byCaps)((_m as any).value);
    case "lowerSnake": return ((_: void) => byUnderscores)((_m as any).value);
    case "upperSnake": return ((_: void) => byUnderscores)((_m as any).value);
  }
})();
})();
  return (() => {
  const _m = to;
  switch (_m.tag) {
    case "camel": return ((_: void) => decapitalize(LibStrings.cat(LibLists.map(((arg_: string) => capitalize(LibStrings.toLower(arg_))))(parts))))((_m as any).value);
    case "pascal": return ((_: void) => LibStrings.cat(LibLists.map(((arg_: string) => capitalize(LibStrings.toLower(arg_))))(parts)))((_m as any).value);
    case "lowerSnake": return ((_: void) => LibStrings.intercalate("_")(LibLists.map(LibStrings.toLower)(parts)))((_m as any).value);
    case "upperSnake": return ((_: void) => LibStrings.intercalate("_")(LibLists.map(LibStrings.toUpper)(parts)))((_m as any).value);
  }
})();
})()));
}

export function convertCaseCamelOrUnderscoreToLowerSnake(s: string): string {
  return (() => {
  const parts = LibStrings.splitOn("_")(s);
  return (() => {
  const snakeParts = LibLists.map(((p: string) => convertCaseCamelToLowerSnake(p)))(parts);
  return LibStrings.intercalate("_")(snakeParts);
})();
})();
}

export function convertCaseCamelToLowerSnake(v1: string): string {
  return convertCase(({ tag: "camel" }))(({ tag: "lowerSnake" }))(v1);
}

export function convertCaseCamelToUpperSnake(v1: string): string {
  return convertCase(({ tag: "camel" }))(({ tag: "upperSnake" }))(v1);
}

export function convertCasePascalToUpperSnake(v1: string): string {
  return convertCase(({ tag: "pascal" }))(({ tag: "upperSnake" }))(v1);
}

export function decapitalize(v1: string): string {
  return mapFirstLetter(LibStrings.toLower)(v1);
}

export function escapeWithUnderscore(reserved: ReadonlySet<string>): ((x: string) => string) {
  return ((s: string) => LibLogic.ifElse(LibSets.member(s)(reserved))(LibStrings.cat2(s)("_"))(s));
}

export function indentLines(s: string): string {
  return (() => {
  const indent = ((l: string) => LibStrings.cat2("    ")(l));
  return LibStrings.unlines(LibLists.map(indent)(LibStrings.lines(s)));
})();
}

export function javaStyleComment(s: string): string {
  return LibStrings.cat2(LibStrings.cat2(LibStrings.cat2("/**\n")(" * "))(s))("\n */");
}

export function mapFirstLetter(mapping: ((x: string) => string)): ((x: string) => string) {
  return ((s: string) => LibLogic.ifElse(LibStrings.null_(s))(s)((() => {
  const list = LibStrings.toList(s);
  return (() => {
  const firstLetter = mapping(LibStrings.fromList(LibLists.pure(LibLists.head(list))));
  return LibStrings.cat2(firstLetter)(LibStrings.fromList(LibLists.tail(list)));
})();
})()));
}

export function nonAlnumToUnderscores(input: string): string {
  return (() => {
  const isAlnum = ((c: number) => LibLogic.or(LibLogic.and(LibEquality.gte(c)(65))(LibEquality.lte(c)(90)))(LibLogic.or(LibLogic.and(LibEquality.gte(c)(97))(LibEquality.lte(c)(122)))(LibLogic.and(LibEquality.gte(c)(48))(LibEquality.lte(c)(57)))));
  return (() => {
  const replace = ((p: readonly [ReadonlyArray<number>, boolean]) => ((c: number) => (() => {
  const s = LibPairs.first(p);
  return (() => {
  const b = LibPairs.second(p);
  return LibLogic.ifElse(isAlnum(c))([LibLists.cons(c)(s), false])(LibLogic.ifElse(b)([s, true])([LibLists.cons(95)(s), true]));
})();
})()));
  return (() => {
  const result = LibLists.foldl(replace)([[], false])(LibStrings.toList(input));
  return LibStrings.fromList(LibLists.reverse(LibPairs.first(result)));
})();
})();
})();
}

export function normalizeComment(s: string): string {
  return (() => {
  const stripped = stripLeadingAndTrailingWhitespace(s);
  return LibLogic.ifElse(LibStrings.null_(stripped))("")((() => {
  const lastIdx = LibMath.sub(LibStrings.length(stripped))(1);
  return (() => {
  const lastChar = LibStrings.charAt(lastIdx)(stripped);
  return LibLogic.ifElse(LibEquality.equal(lastChar)(46))(stripped)(LibStrings.cat2(stripped)("."));
})();
})());
})();
}

export function sanitizeWithUnderscores(reserved: ReadonlySet<string>): ((x: string) => string) {
  return ((s: string) => escapeWithUnderscore(reserved)(nonAlnumToUnderscores(s)));
}

export function showList<t0>(f: ((x: t0) => string)): ((x: ReadonlyArray<t0>) => string) {
  return ((els: ReadonlyArray<t0>) => LibStrings.cat(["[", LibStrings.intercalate(", ")(LibLists.map(f)(els)), "]"]));
}

export function stripLeadingAndTrailingWhitespace(s: string): string {
  return LibStrings.fromList(LibLists.dropWhile(LibChars.isSpace)(LibLists.reverse(LibLists.dropWhile(LibChars.isSpace)(LibLists.reverse(LibStrings.toList(s))))));
}

export function withCharacterAliases(original: string): string {
  return (() => {
  const aliases = LibMaps.fromList([[32, "sp"], [33, "excl"], [34, "quot"], [35, "num"], [36, "dollar"], [37, "percnt"], [38, "amp"], [39, "apos"], [40, "lpar"], [41, "rpar"], [42, "ast"], [43, "plus"], [44, "comma"], [45, "minus"], [46, "period"], [47, "sol"], [58, "colon"], [59, "semi"], [60, "lt"], [61, "equals"], [62, "gt"], [63, "quest"], [64, "commat"], [91, "lsqb"], [92, "bsol"], [93, "rsqb"], [94, "circ"], [95, "lowbar"], [96, "grave"], [123, "lcub"], [124, "verbar"], [125, "rcub"], [126, "tilde"]]);
  const alias = ((c: number) => LibMaybes.fromMaybe(LibLists.pure(c))(LibMaybes.map(LibStrings.toList)(LibMaps.lookup(c)(aliases))));
  return LibStrings.fromList(LibLists.filter(LibChars.isAlphaNum)(LibLists.concat(LibLists.map(alias)(LibStrings.toList(original)))));
})();
}

export function wrapLine(maxlen: number): ((x: string) => string) {
  return ((input: string) => (() => {
  const helper = ((prev: ReadonlyArray<ReadonlyArray<number>>) => ((rem: ReadonlyArray<number>) => (() => {
  const trunc = LibLists.take(maxlen)(rem);
  const spanResult = LibLists.span(((c: number) => LibLogic.and(LibLogic.not(LibEquality.equal(c)(32)))(LibLogic.not(LibEquality.equal(c)(9)))))(LibLists.reverse(trunc));
  const prefix = LibLists.reverse(LibPairs.second(spanResult));
  const suffix = LibLists.reverse(LibPairs.first(spanResult));
  return LibLogic.ifElse(LibEquality.lte(LibLists.length(rem))(maxlen))(LibLists.reverse(LibLists.cons(rem)(prev)))(LibLogic.ifElse(LibLists.null_(prefix))(helper(LibLists.cons(trunc)(prev))(LibLists.drop(maxlen)(rem)))(helper(LibLists.cons(LibLists.init(prefix))(prev))(LibLists.concat2(suffix)(LibLists.drop(maxlen)(rem)))));
})()));
  return LibStrings.fromList(LibLists.intercalate([10])(helper([])(LibStrings.toList(input))));
})());
}
