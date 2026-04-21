// Note: this is an automatically generated file. Do not edit.

/**
 * JSON parser using Hydra parser combinators
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
import * as JsonModel from "./model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsers from "../parsers.js";
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

export const digit: Parsing.Parser<number> = Parsers.satisfy(((c: number) => LibLogic.and(LibEquality.gte(c)(48))(LibEquality.lte(c)(57))));

export const digits: Parsing.Parser<string> = Parsers.map(LibStrings.fromList)(Parsers.some(digit));

export const jsonArray: Parsing.Parser<JsonModel.Value> = Parsers.map(((x: ReadonlyArray<JsonModel.Value>) => ({ tag: "array", value: x })))(Parsers.between(token(Parsers.char_(91)))(token(Parsers.char_(93)))(Parsers.sepBy(Parsers.lazy(((_: void) => jsonValue)))(token(Parsers.char_(44)))));

export const jsonBool: Parsing.Parser<JsonModel.Value> = Parsers.alt(Parsers.map(((_: string) => ({ tag: "boolean", value: true })))(token(Parsers.string("true"))))(Parsers.map(((_: string) => ({ tag: "boolean", value: false })))(token(Parsers.string("false"))));

export const jsonEscapeChar: Parsing.Parser<number> = Parsers.choice([Parsers.map(((_: number) => 34))(Parsers.char_(34)), Parsers.map(((_: number) => 92))(Parsers.char_(92)), Parsers.map(((_: number) => 47))(Parsers.char_(47)), Parsers.map(((_: number) => 8))(Parsers.char_(98)), Parsers.map(((_: number) => 12))(Parsers.char_(102)), Parsers.map(((_: number) => 10))(Parsers.char_(110)), Parsers.map(((_: number) => 13))(Parsers.char_(114)), Parsers.map(((_: number) => 9))(Parsers.char_(116))]);

export const jsonExponentPart: Parsing.Parser<string | null> = Parsers.optional(Parsers.bind(Parsers.satisfy(((c: number) => LibLogic.or(LibEquality.equal(c)(101))(LibEquality.equal(c)(69)))))(((_: number) => Parsers.bind(Parsers.optional(Parsers.satisfy(((c: number) => LibLogic.or(LibEquality.equal(c)(43))(LibEquality.equal(c)(45))))))(((sign: number | null) => Parsers.map(((digits: string) => LibStrings.cat2(LibStrings.cat2("e")(LibMaybes.maybe("")(((arg_: number) => LibStrings.fromList(LibLists.pure(arg_))))(sign)))(digits)))(digits))))));

export const jsonFractionPart: Parsing.Parser<string | null> = Parsers.optional(Parsers.bind(Parsers.char_(46))(((_: number) => Parsers.map(((d: string) => LibStrings.cat2(".")(d)))(digits))));

export const jsonIntegerPart: Parsing.Parser<string> = Parsers.bind(Parsers.optional(Parsers.char_(45)))(((sign: number | null) => Parsers.bind(digits)(((digits: string) => Parsers.pure(LibMaybes.maybe(digits)(((_: number) => LibStrings.cat2("-")(digits)))(sign))))));

export const jsonKeyValue: Parsing.Parser<readonly [string, JsonModel.Value]> = Parsers.bind(token(Parsers.bind(Parsers.char_(34))(((_: number) => Parsers.bind(Parsers.many(jsonStringChar))(((chars: ReadonlyArray<number>) => Parsers.bind(Parsers.char_(34))(((_2: number) => Parsers.pure(LibStrings.fromList(chars))))))))))(((key: string) => Parsers.bind(token(Parsers.char_(58)))(((_: number) => Parsers.map(((v: JsonModel.Value) => [key, v]))(Parsers.lazy(((_2: void) => jsonValue)))))));

export const jsonNull: Parsing.Parser<JsonModel.Value> = Parsers.map(((_: string) => ({ tag: "null" })))(token(Parsers.string("null")));

export const jsonNumber: Parsing.Parser<JsonModel.Value> = token(Parsers.bind(jsonIntegerPart)(((intPart: string) => Parsers.bind(jsonFractionPart)(((fracPart: string | null) => Parsers.bind(jsonExponentPart)(((expPart: string | null) => (() => {
  const numStr = LibStrings.cat2(LibStrings.cat2(intPart)(LibMaybes.maybe("")(LibEquality.identity)(fracPart)))(LibMaybes.maybe("")(LibEquality.identity)(expPart));
  return Parsers.pure(({ tag: "number", value: LibMaybes.maybe(0.0)(LibEquality.identity)(LibLiterals.readBigfloat(numStr)) }));
})())))))));

export const jsonObject: Parsing.Parser<JsonModel.Value> = Parsers.map(((arg_: ReadonlyArray<readonly [string, JsonModel.Value]>) => ({ tag: "object", value: LibMaps.fromList(arg_) })))(Parsers.between(token(Parsers.char_(123)))(token(Parsers.char_(125)))(Parsers.sepBy(jsonKeyValue)(token(Parsers.char_(44)))));

export const jsonString: Parsing.Parser<JsonModel.Value> = token(Parsers.bind(Parsers.char_(34))(((_: number) => Parsers.bind(Parsers.many(jsonStringChar))(((chars: ReadonlyArray<number>) => Parsers.bind(Parsers.char_(34))(((_2: number) => Parsers.pure(({ tag: "string", value: LibStrings.fromList(chars) })))))))));

export const jsonStringChar: Parsing.Parser<number> = Parsers.alt(Parsers.bind(Parsers.char_(92))(((_: number) => jsonEscapeChar)))(Parsers.satisfy(((c: number) => LibLogic.and(LibLogic.not(LibEquality.equal(c)(34)))(LibLogic.not(LibEquality.equal(c)(92))))));

export const jsonValue: Parsing.Parser<JsonModel.Value> = Parsers.choice([jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject]);

export function parseJson(input: string): Parsing.ParseResult<JsonModel.Value> {
  return ((_x) => _x)(Parsers.bind(whitespace)(((_: void) => Parsers.bind(jsonValue)(((v: JsonModel.Value) => Parsers.bind(whitespace)(((_2: void) => Parsers.bind(Parsers.eof)(((_3: void) => Parsers.pure(v))))))))))(input);
}

export function token<t0>(p: Parsing.Parser<t0>): Parsing.Parser<t0> {
  return Parsers.bind(p)(((x: t0) => Parsers.bind(whitespace)(((_: void) => Parsers.pure(x)))));
}

export const whitespace: Parsing.Parser<void> = Parsers.map(((_: ReadonlyArray<number>) => undefined))(Parsers.many(Parsers.satisfy(((c: number) => LibLists.foldl(LibLogic.or)(false)([LibEquality.equal(c)(32), LibEquality.equal(c)(9), LibEquality.equal(c)(10), LibEquality.equal(c)(13)])))));
