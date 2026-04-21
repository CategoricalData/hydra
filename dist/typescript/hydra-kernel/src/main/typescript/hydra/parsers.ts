// Note: this is an automatically generated file. Do not edit.

/**
 * General-purpose parser combinators
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
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaybes from "./lib/maybes.js";
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

export function alt<t0>(p1: Parsing.Parser<t0>): ((x: Parsing.Parser<t0>) => Parsing.Parser<t0>) {
  return ((p2: Parsing.Parser<t0>) => (() => {
  const parse = ((input: string) => ((v1: Parsing.ParseResult) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "success": return ((s: Parsing.ParseSuccess<t0>) => ({ tag: "success", value: s }))((_m as any).value);
    case "failure": return ((e: Parsing.ParseError) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.remainder)(e))(input))(((_x) => _x)(p2)(input))(({ tag: "failure", value: e })))((_m as any).value);
  }
})())(((_x) => _x)(p1)(input)));
  return parse;
})());
}

export const anyChar: Parsing.Parser<number> = satisfy(((_: number) => true));

export function apply<t0, t1>(pf: Parsing.Parser<((x: t0) => t1)>): ((x: Parsing.Parser<t0>) => Parsing.Parser<t1>) {
  return ((pa: Parsing.Parser<t0>) => (() => {
  const parse = ((input: string) => ((v1: Parsing.ParseResult) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "success": return ((sf: Parsing.ParseSuccess<((x: t0) => t1)>) => ((v12: Parsing.ParseResult) => (() => {
  const _m = v12;
  switch (_m.tag) {
    case "success": return ((sa: Parsing.ParseSuccess<t0>) => ({ tag: "success", value: ({
    value: ((_x) => _x.value)(sf)(((_x) => _x.value)(sa)),
    remainder: ((_x) => _x.remainder)(sa)
  }) }))((_m as any).value);
    case "failure": return ((e: Parsing.ParseError) => ({ tag: "failure", value: e }))((_m as any).value);
  }
})())(((_x) => _x)(pa)(((_x) => _x.remainder)(sf))))((_m as any).value);
    case "failure": return ((e: Parsing.ParseError) => ({ tag: "failure", value: e }))((_m as any).value);
  }
})())(((_x) => _x)(pf)(input)));
  return parse;
})());
}

export function between<t0, t1, t2>(open: Parsing.Parser<t0>): ((x: Parsing.Parser<t1>) => ((x: Parsing.Parser<t2>) => Parsing.Parser<t2>)) {
  return ((close: Parsing.Parser<t1>) => ((p: Parsing.Parser<t2>) => bind(open)(((_: t0) => bind(p)(((x: t2) => bind(close)(((_2: t1) => pure(x)))))))));
}

export function bind<t0, t1>(pa: Parsing.Parser<t0>): ((x: ((x: t0) => Parsing.Parser<t1>)) => Parsing.Parser<t1>) {
  return ((f: ((x: t0) => Parsing.Parser<t1>)) => (() => {
  const parse = ((input: string) => ((v1: Parsing.ParseResult) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "success": return ((s: Parsing.ParseSuccess<t0>) => ((_x) => _x)(f(((_x) => _x.value)(s)))(((_x) => _x.remainder)(s)))((_m as any).value);
    case "failure": return ((e: Parsing.ParseError) => ({ tag: "failure", value: e }))((_m as any).value);
  }
})())(((_x) => _x)(pa)(input)));
  return parse;
})());
}

export function char_(c: number): Parsing.Parser<number> {
  return satisfy(((x: number) => LibEquality.equal(x)(c)));
}

export function choice<t0>(ps: ReadonlyArray<Parsing.Parser<t0>>): Parsing.Parser<t0> {
  return LibLists.foldl(alt)(fail("no choice matched"))(ps);
}

export const eof: Parsing.Parser<void> = ((input: string) => LibLogic.ifElse(LibEquality.equal(input)(""))(({ tag: "success", value: ({
    value: undefined,
    remainder: ""
  }) }))(({ tag: "failure", value: ({
    message: "expected end of input",
    remainder: input
  }) })));

export function fail<t0>(msg: string): Parsing.Parser<t0> {
  return ((input: string) => ({ tag: "failure", value: ({
    message: msg,
    remainder: input
  }) }));
}

export function lazy<t0>(f: ((x: void) => Parsing.Parser<t0>)): Parsing.Parser<t0> {
  return ((input: string) => ((_x) => _x)(f(undefined))(input));
}

export function many<t0>(p: Parsing.Parser<t0>): Parsing.Parser<ReadonlyArray<t0>> {
  return alt(some(p))(pure([]));
}

export function map<t0, t1>(f: ((x: t0) => t1)): ((x: Parsing.Parser<t0>) => Parsing.Parser<t1>) {
  return ((pa: Parsing.Parser<t0>) => (() => {
  const parse = ((input: string) => ((v1: Parsing.ParseResult) => (() => {
  const _m = v1;
  switch (_m.tag) {
    case "success": return ((s: Parsing.ParseSuccess<t0>) => ({ tag: "success", value: ({
    value: f(((_x) => _x.value)(s)),
    remainder: ((_x) => _x.remainder)(s)
  }) }))((_m as any).value);
    case "failure": return ((e: Parsing.ParseError) => ({ tag: "failure", value: e }))((_m as any).value);
  }
})())(((_x) => _x)(pa)(input)));
  return parse;
})());
}

export function optional<t0>(p: Parsing.Parser<t0>): Parsing.Parser<t0 | null> {
  return alt(map(LibMaybes.pure)(p))(pure(null));
}

export function pure<t0>(a: t0): Parsing.Parser<t0> {
  return ((input: string) => ({ tag: "success", value: ({
    value: a,
    remainder: input
  }) }));
}

export function runParser<t0>(p: Parsing.Parser<t0>): ((x: string) => Parsing.ParseResult<t0>) {
  return ((input: string) => ((_x) => _x)(p)(input));
}

export function satisfy(pred: ((x: number) => boolean)): Parsing.Parser<number> {
  return (() => {
  const parse = ((input: string) => (() => {
  const codes = LibStrings.toList(input);
  return LibMaybes.maybe(({ tag: "failure", value: ({
    message: "unexpected end of input",
    remainder: input
  }) }))(((c: number) => (() => {
  const rest = LibStrings.fromList(LibLists.drop(1)(codes));
  return LibLogic.ifElse(pred(c))(({ tag: "success", value: ({
    value: c,
    remainder: rest
  }) }))(({ tag: "failure", value: ({
    message: "character did not satisfy predicate",
    remainder: input
  }) }));
})()))(LibLists.safeHead(codes));
})());
  return parse;
})();
}

export function sepBy<t0, t1>(p: Parsing.Parser<t0>): ((x: Parsing.Parser<t1>) => Parsing.Parser<ReadonlyArray<t0>>) {
  return ((sep: Parsing.Parser<t1>) => alt(sepBy1(p)(sep))(pure([])));
}

export function sepBy1<t0, t1>(p: Parsing.Parser<t0>): ((x: Parsing.Parser<t1>) => Parsing.Parser<ReadonlyArray<t0>>) {
  return ((sep: Parsing.Parser<t1>) => bind(p)(((x: t0) => bind(many(bind(sep)(((_: t1) => p))))(((xs: ReadonlyArray<t0>) => pure(LibLists.cons(x)(xs)))))));
}

export function some<t0>(p: Parsing.Parser<t0>): Parsing.Parser<ReadonlyArray<t0>> {
  return bind(p)(((x: t0) => bind(many(p))(((xs: ReadonlyArray<t0>) => pure(LibLists.cons(x)(xs))))));
}

export function string(str: string): Parsing.Parser<string> {
  return ((input: string) => (() => {
  const strCodes = LibStrings.toList(str);
  return (() => {
  const inputCodes = LibStrings.toList(input);
  return (() => {
  const strLen = LibLists.length(strCodes);
  return (() => {
  const inputPrefix = LibLists.take(strLen)(inputCodes);
  return LibLogic.ifElse(LibEquality.equal(strCodes)(inputPrefix))(({ tag: "success", value: ({
    value: str,
    remainder: LibStrings.fromList(LibLists.drop(strLen)(inputCodes))
  }) }))(({ tag: "failure", value: ({
    message: LibStrings.cat2("expected: ")(str),
    remainder: input
  }) }));
})();
})();
})();
})());
}
