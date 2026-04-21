/**
 * Standard library primitive registration for Hydra-TypeScript.
 *
 * Registers all primitive functions with their type signatures,
 * mirroring the Python and Java registration patterns.
 */

import type { Primitive } from "./core.js";
import { Name } from "./core.js";
import * as prims from "./dsl/prims.js";

import * as chars from "./lib/chars.js";
import * as eithers from "./lib/eithers.js";
import * as equality from "./lib/equality.js";
import * as lists from "./lib/lists.js";
import * as literals from "./lib/literals.js";
import * as logic from "./lib/logic.js";
import * as maps from "./lib/maps.js";
import * as math from "./lib/math.js";
import * as maybes from "./lib/maybes.js";
import * as pairs from "./lib/pairs.js";
import * as regex from "./lib/regex.js";
import * as sets from "./lib/sets.js";
import * as strings from "./lib/strings.js";

function qname(ns: string, local: string): string {
  return `${ns}.${local}`;
}

function registerCharsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.chars";
  const m = new Map<string, Primitive>();

  m.set(qname(ns, "isAlphaNum"), prims.prim1(qname(ns, "isAlphaNum"), chars.isAlphaNum, [], prims.int32(), prims.boolean()));
  m.set(qname(ns, "isLower"), prims.prim1(qname(ns, "isLower"), chars.isLower, [], prims.int32(), prims.boolean()));
  m.set(qname(ns, "isSpace"), prims.prim1(qname(ns, "isSpace"), chars.isSpace, [], prims.int32(), prims.boolean()));
  m.set(qname(ns, "isUpper"), prims.prim1(qname(ns, "isUpper"), chars.isUpper, [], prims.int32(), prims.boolean()));
  m.set(qname(ns, "toLower"), prims.prim1(qname(ns, "toLower"), chars.toLower, [], prims.int32(), prims.int32()));
  m.set(qname(ns, "toUpper"), prims.prim1(qname(ns, "toUpper"), chars.toUpper, [], prims.int32(), prims.int32()));

  return m;
}

function registerEqualityPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.equality";
  const m = new Map<string, Primitive>();
  const x = prims.variable("x");

  m.set(qname(ns, "equal"), prims.prim2(qname(ns, "equal"), equality.equal, ["x"], x, x, prims.boolean()));
  m.set(qname(ns, "gt"), prims.prim2(qname(ns, "gt"), equality.gt, ["x"], x, x, prims.boolean()));
  m.set(qname(ns, "gte"), prims.prim2(qname(ns, "gte"), equality.gte, ["x"], x, x, prims.boolean()));
  m.set(qname(ns, "identity"), prims.prim1(qname(ns, "identity"), equality.identity, ["x"], x, x));
  m.set(qname(ns, "lt"), prims.prim2(qname(ns, "lt"), equality.lt, ["x"], x, x, prims.boolean()));
  m.set(qname(ns, "lte"), prims.prim2(qname(ns, "lte"), equality.lte, ["x"], x, x, prims.boolean()));
  m.set(qname(ns, "max"), prims.prim2(qname(ns, "max"), equality.max, ["x"], x, x, x));
  m.set(qname(ns, "min"), prims.prim2(qname(ns, "min"), equality.min, ["x"], x, x, x));

  return m;
}

function registerEithersPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.eithers";
  const m = new Map<string, Primitive>();
  const x = prims.variable("x");
  const y = prims.variable("y");

  m.set(qname(ns, "fromLeft"), prims.prim2(qname(ns, "fromLeft"), eithers.fromLeft, ["x", "y"], x, prims.either(x, y), x));
  m.set(qname(ns, "fromRight"), prims.prim2(qname(ns, "fromRight"), eithers.fromRight, ["x", "y"], y, prims.either(x, y), y));
  m.set(qname(ns, "isLeft"), prims.prim1(qname(ns, "isLeft"), eithers.isLeft, ["x", "y"], prims.either(x, y), prims.boolean()));
  m.set(qname(ns, "isRight"), prims.prim1(qname(ns, "isRight"), eithers.isRight, ["x", "y"], prims.either(x, y), prims.boolean()));
  m.set(qname(ns, "lefts"), prims.prim1(qname(ns, "lefts"), eithers.lefts, ["x", "y"], prims.list_(prims.either(x, y)), prims.list_(x)));
  m.set(qname(ns, "rights"), prims.prim1(qname(ns, "rights"), eithers.rights, ["x", "y"], prims.list_(prims.either(x, y)), prims.list_(y)));

  return m;
}

function registerListsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.lists";
  const m = new Map<string, Primitive>();
  const a = prims.variable("a");
  const b = prims.variable("b");

  m.set(qname(ns, "at"), prims.prim2(qname(ns, "at"), lists.at, ["a"], prims.int32(), prims.list_(a), a));
  m.set(qname(ns, "concat"), prims.prim1(qname(ns, "concat"), lists.concat, ["a"], prims.list_(prims.list_(a)), prims.list_(a)));
  m.set(qname(ns, "concat2"), prims.prim2(qname(ns, "concat2"), lists.concat2, ["a"], prims.list_(a), prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "cons"), prims.prim2(qname(ns, "cons"), lists.cons, ["a"], a, prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "drop"), prims.prim2(qname(ns, "drop"), lists.drop, ["a"], prims.int32(), prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "elem"), prims.prim2(qname(ns, "elem"), lists.elem, ["a"], a, prims.list_(a), prims.boolean()));
  m.set(qname(ns, "head"), prims.prim1(qname(ns, "head"), lists.head, ["a"], prims.list_(a), a));
  m.set(qname(ns, "init"), prims.prim1(qname(ns, "init"), lists.init, ["a"], prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "intercalate"), prims.prim2(qname(ns, "intercalate"), lists.intercalate, ["a"], prims.list_(a), prims.list_(prims.list_(a)), prims.list_(a)));
  m.set(qname(ns, "intersperse"), prims.prim2(qname(ns, "intersperse"), lists.intersperse, ["a"], a, prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "last"), prims.prim1(qname(ns, "last"), lists.last, ["a"], prims.list_(a), a));
  m.set(qname(ns, "length"), prims.prim1(qname(ns, "length"), lists.length, ["a"], prims.list_(a), prims.int32()));
  m.set(qname(ns, "maybeAt"), prims.prim2(qname(ns, "maybeAt"), lists.maybeAt, ["a"], prims.int32(), prims.list_(a), prims.optional(a)));
  m.set(qname(ns, "maybeHead"), prims.prim1(qname(ns, "maybeHead"), lists.maybeHead, ["a"], prims.list_(a), prims.optional(a)));
  m.set(qname(ns, "maybeInit"), prims.prim1(qname(ns, "maybeInit"), lists.maybeInit, ["a"], prims.list_(a), prims.optional(prims.list_(a))));
  m.set(qname(ns, "maybeLast"), prims.prim1(qname(ns, "maybeLast"), lists.maybeLast, ["a"], prims.list_(a), prims.optional(a)));
  m.set(qname(ns, "maybeTail"), prims.prim1(qname(ns, "maybeTail"), lists.maybeTail, ["a"], prims.list_(a), prims.optional(prims.list_(a))));
  m.set(qname(ns, "nub"), prims.prim1(qname(ns, "nub"), lists.nub, ["a"], prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "null"), prims.prim1(qname(ns, "null"), lists.null_, ["a"], prims.list_(a), prims.boolean()));
  m.set(qname(ns, "pure"), prims.prim1(qname(ns, "pure"), lists.pure, ["a"], a, prims.list_(a)));
  m.set(qname(ns, "replicate"), prims.prim2(qname(ns, "replicate"), lists.replicate, ["a"], prims.int32(), a, prims.list_(a)));
  m.set(qname(ns, "reverse"), prims.prim1(qname(ns, "reverse"), lists.reverse, ["a"], prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "safeHead"), prims.prim1(qname(ns, "safeHead"), lists.safeHead, ["a"], prims.list_(a), prims.optional(a)));
  m.set(qname(ns, "singleton"), prims.prim1(qname(ns, "singleton"), lists.singleton, ["a"], a, prims.list_(a)));
  m.set(qname(ns, "sort"), prims.prim1(qname(ns, "sort"), lists.sort, ["a"], prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "tail"), prims.prim1(qname(ns, "tail"), lists.tail, ["a"], prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "take"), prims.prim2(qname(ns, "take"), lists.take, ["a"], prims.int32(), prims.list_(a), prims.list_(a)));
  m.set(qname(ns, "zip"), prims.prim2(qname(ns, "zip"), lists.zip, ["a", "b"], prims.list_(a), prims.list_(b), prims.list_(prims.pair(a, b))));

  return m;
}

function registerLogicPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.logic";
  const m = new Map<string, Primitive>();
  const a = prims.variable("a");

  m.set(qname(ns, "and"), prims.prim2(qname(ns, "and"), logic.and, [], prims.boolean(), prims.boolean(), prims.boolean()));
  m.set(qname(ns, "ifElse"), prims.prim3(qname(ns, "ifElse"), logic.ifElse, ["a"], prims.boolean(), a, a, a));
  m.set(qname(ns, "not"), prims.prim1(qname(ns, "not"), logic.not, [], prims.boolean(), prims.boolean()));
  m.set(qname(ns, "or"), prims.prim2(qname(ns, "or"), logic.or, [], prims.boolean(), prims.boolean(), prims.boolean()));

  return m;
}

function registerMapsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.maps";
  const m = new Map<string, Primitive>();
  const k = prims.variable("k");
  const v = prims.variable("v");
  const v2 = prims.variable("v2");
  const k2 = prims.variable("k2");
  const mkv = prims.map_(k, v);

  // eslint-disable-next-line @typescript-eslint/no-explicit-any -- type variable coders are inherently polymorphic
  const mkvAny = mkv as prims.TermCoder<any>;
  m.set(qname(ns, "delete"), prims.prim2(qname(ns, "delete"), maps.delete_ as any, ["k", "v"], k, mkvAny, mkvAny));
  m.set(qname(ns, "elems"), prims.prim1(qname(ns, "elems"), maps.elems as any, ["k", "v"], mkvAny, prims.list_(v)));
  m.set(qname(ns, "empty"), prims.prim0(qname(ns, "empty"), maps.empty as any, ["k", "v"], mkvAny));
  m.set(qname(ns, "fromList"), prims.prim1(qname(ns, "fromList"), maps.fromList as any, ["k", "v"], prims.list_(prims.pair(k, v)), mkvAny));
  m.set(qname(ns, "insert"), prims.prim3(qname(ns, "insert"), maps.insert as any, ["k", "v"], k, v, mkvAny, mkvAny));
  m.set(qname(ns, "keys"), prims.prim1(qname(ns, "keys"), maps.keys as any, ["k", "v"], mkvAny, prims.list_(k)));
  m.set(qname(ns, "lookup"), prims.prim2(qname(ns, "lookup"), maps.lookup as any, ["k", "v"], k, mkvAny, prims.optional(v)));
  m.set(qname(ns, "member"), prims.prim2(qname(ns, "member"), maps.member as any, ["k", "v"], k, mkvAny, prims.boolean()));
  m.set(qname(ns, "null"), prims.prim1(qname(ns, "null"), maps.null_ as any, ["k", "v"], mkvAny, prims.boolean()));
  m.set(qname(ns, "singleton"), prims.prim2(qname(ns, "singleton"), maps.singleton as any, ["k", "v"], k, v, mkvAny));
  m.set(qname(ns, "size"), prims.prim1(qname(ns, "size"), maps.size as any, ["k", "v"], mkvAny, prims.int32()));
  m.set(qname(ns, "toList"), prims.prim1(qname(ns, "toList"), maps.toList as any, ["k", "v"], mkvAny, prims.list_(prims.pair(k, v))));
  m.set(qname(ns, "union"), prims.prim2(qname(ns, "union"), maps.union as any, ["k", "v"], mkvAny, mkvAny, mkvAny));

  return m;
}

function registerMathPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.math";
  const m = new Map<string, Primitive>();

  // Int32 primitives
  m.set(qname(ns, "abs"), prims.prim1(qname(ns, "abs"), math.abs, [], prims.int32(), prims.int32()));
  m.set(qname(ns, "add"), prims.prim2(qname(ns, "add"), math.add, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "addFloat64"), prims.prim2(qname(ns, "addFloat64"), math.addFloat64, [], prims.float64(), prims.float64(), prims.float64()));
  m.set(qname(ns, "div"), prims.prim2(qname(ns, "div"), math.div, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "even"), prims.prim1(qname(ns, "even"), math.even, [], prims.int32(), prims.boolean()));
  m.set(qname(ns, "max"), prims.prim2(qname(ns, "max"), math.max, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "maybeDiv"), prims.prim2(qname(ns, "maybeDiv"), math.maybeDiv, [], prims.int32(), prims.int32(), prims.optional(prims.int32())));
  m.set(qname(ns, "maybeMod"), prims.prim2(qname(ns, "maybeMod"), math.maybeMod, [], prims.int32(), prims.int32(), prims.optional(prims.int32())));
  m.set(qname(ns, "maybePred"), prims.prim1(qname(ns, "maybePred"), math.maybePred, [], prims.int32(), prims.optional(prims.int32())));
  m.set(qname(ns, "maybeRem"), prims.prim2(qname(ns, "maybeRem"), math.maybeRem, [], prims.int32(), prims.int32(), prims.optional(prims.int32())));
  m.set(qname(ns, "maybeSucc"), prims.prim1(qname(ns, "maybeSucc"), math.maybeSucc, [], prims.int32(), prims.optional(prims.int32())));
  m.set(qname(ns, "min"), prims.prim2(qname(ns, "min"), math.min, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "mod"), prims.prim2(qname(ns, "mod"), math.mod, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "mul"), prims.prim2(qname(ns, "mul"), math.mul, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "mulFloat64"), prims.prim2(qname(ns, "mulFloat64"), math.mulFloat64, [], prims.float64(), prims.float64(), prims.float64()));
  m.set(qname(ns, "negate"), prims.prim1(qname(ns, "negate"), math.negate, [], prims.int32(), prims.int32()));
  m.set(qname(ns, "negateFloat64"), prims.prim1(qname(ns, "negateFloat64"), math.negateFloat64, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "odd"), prims.prim1(qname(ns, "odd"), math.odd, [], prims.int32(), prims.boolean()));
  m.set(qname(ns, "pred"), prims.prim1(qname(ns, "pred"), math.pred, [], prims.int32(), prims.int32()));
  m.set(qname(ns, "range"), prims.prim2(qname(ns, "range"), math.range, [], prims.int32(), prims.int32(), prims.list_(prims.int32())));
  m.set(qname(ns, "rem"), prims.prim2(qname(ns, "rem"), math.rem, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "signum"), prims.prim1(qname(ns, "signum"), math.signum, [], prims.int32(), prims.int32()));
  m.set(qname(ns, "sub"), prims.prim2(qname(ns, "sub"), math.sub, [], prims.int32(), prims.int32(), prims.int32()));
  m.set(qname(ns, "subFloat64"), prims.prim2(qname(ns, "subFloat64"), math.subFloat64, [], prims.float64(), prims.float64(), prims.float64()));
  m.set(qname(ns, "succ"), prims.prim1(qname(ns, "succ"), math.succ, [], prims.int32(), prims.int32()));

  // Float64 primitives
  m.set(qname(ns, "acos"), prims.prim1(qname(ns, "acos"), math.acos, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "acosh"), prims.prim1(qname(ns, "acosh"), math.acosh, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "asin"), prims.prim1(qname(ns, "asin"), math.asin, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "asinh"), prims.prim1(qname(ns, "asinh"), math.asinh, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "atan"), prims.prim1(qname(ns, "atan"), math.atan, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "atan2"), prims.prim2(qname(ns, "atan2"), math.atan2, [], prims.float64(), prims.float64(), prims.float64()));
  m.set(qname(ns, "atanh"), prims.prim1(qname(ns, "atanh"), math.atanh, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "ceiling"), prims.prim1(qname(ns, "ceiling"), math.ceiling, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "cos"), prims.prim1(qname(ns, "cos"), math.cos, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "cosh"), prims.prim1(qname(ns, "cosh"), math.cosh, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "e"), prims.prim0(qname(ns, "e"), math.e, [], prims.float64()));
  m.set(qname(ns, "exp"), prims.prim1(qname(ns, "exp"), math.exp, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "floor"), prims.prim1(qname(ns, "floor"), math.floor, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "log"), prims.prim1(qname(ns, "log"), math.log, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "logBase"), prims.prim2(qname(ns, "logBase"), math.logBase, [], prims.float64(), prims.float64(), prims.float64()));
  m.set(qname(ns, "pi"), prims.prim0(qname(ns, "pi"), math.pi, [], prims.float64()));
  m.set(qname(ns, "pow"), prims.prim2(qname(ns, "pow"), math.pow, [], prims.float64(), prims.float64(), prims.float64()));
  m.set(qname(ns, "round"), prims.prim1(qname(ns, "round"), math.round, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "roundBigfloat"), prims.prim2(qname(ns, "roundBigfloat"), math.roundBigfloat, [], prims.int32(), prims.bigfloat(), prims.bigfloat()));
  m.set(qname(ns, "roundFloat32"), prims.prim2(qname(ns, "roundFloat32"), math.roundFloat32, [], prims.int32(), prims.float32(), prims.float32()));
  m.set(qname(ns, "roundFloat64"), prims.prim2(qname(ns, "roundFloat64"), math.roundFloat64, [], prims.int32(), prims.float64(), prims.float64()));
  m.set(qname(ns, "sin"), prims.prim1(qname(ns, "sin"), math.sin, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "sinh"), prims.prim1(qname(ns, "sinh"), math.sinh, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "sqrt"), prims.prim1(qname(ns, "sqrt"), math.sqrt, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "tan"), prims.prim1(qname(ns, "tan"), math.tan, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "tanh"), prims.prim1(qname(ns, "tanh"), math.tanh, [], prims.float64(), prims.float64()));
  m.set(qname(ns, "truncate"), prims.prim1(qname(ns, "truncate"), math.truncate, [], prims.float64(), prims.float64()));

  return m;
}

function registerMaybesPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.maybes";
  const m = new Map<string, Primitive>();
  const a = prims.variable("a");
  const b = prims.variable("b");

  m.set(qname(ns, "cat"), prims.prim1(qname(ns, "cat"), maybes.cat, ["a"], prims.list_(prims.optional(a)), prims.list_(a)));
  m.set(qname(ns, "fromJust"), prims.prim1(qname(ns, "fromJust"), maybes.fromJust, ["a"], prims.optional(a), a));
  m.set(qname(ns, "fromMaybe"), prims.prim2(qname(ns, "fromMaybe"), maybes.fromMaybe, ["a"], a, prims.optional(a), a));
  m.set(qname(ns, "isJust"), prims.prim1(qname(ns, "isJust"), maybes.isJust, ["a"], prims.optional(a), prims.boolean()));
  m.set(qname(ns, "isNothing"), prims.prim1(qname(ns, "isNothing"), maybes.isNothing, ["a"], prims.optional(a), prims.boolean()));
  m.set(qname(ns, "pure"), prims.prim1(qname(ns, "pure"), maybes.pure, ["a"], a, prims.optional(a)));
  m.set(qname(ns, "toList"), prims.prim1(qname(ns, "toList"), maybes.toList, ["a"], prims.optional(a), prims.list_(a)));

  return m;
}

function registerSetsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.sets";
  const m = new Map<string, Primitive>();
  const a = prims.variable("a");

  // eslint-disable-next-line @typescript-eslint/no-explicit-any -- type variable coders are inherently polymorphic
  const sa = prims.set_(a) as prims.TermCoder<any>;
  m.set(qname(ns, "delete"), prims.prim2(qname(ns, "delete"), sets.delete_ as any, ["a"], a, sa, sa));
  m.set(qname(ns, "difference"), prims.prim2(qname(ns, "difference"), sets.difference as any, ["a"], sa, sa, sa));
  m.set(qname(ns, "empty"), prims.prim0(qname(ns, "empty"), sets.empty as any, ["a"], sa));
  m.set(qname(ns, "fromList"), prims.prim1(qname(ns, "fromList"), sets.fromList as any, ["a"], prims.list_(a), sa));
  m.set(qname(ns, "insert"), prims.prim2(qname(ns, "insert"), sets.insert as any, ["a"], a, sa, sa));
  m.set(qname(ns, "intersection"), prims.prim2(qname(ns, "intersection"), sets.intersection as any, ["a"], sa, sa, sa));
  m.set(qname(ns, "member"), prims.prim2(qname(ns, "member"), sets.member as any, ["a"], a, sa, prims.boolean()));
  m.set(qname(ns, "null"), prims.prim1(qname(ns, "null"), sets.null_ as any, ["a"], sa, prims.boolean()));
  m.set(qname(ns, "singleton"), prims.prim1(qname(ns, "singleton"), sets.singleton as any, ["a"], a, sa));
  m.set(qname(ns, "size"), prims.prim1(qname(ns, "size"), sets.size as any, ["a"], sa, prims.int32()));
  m.set(qname(ns, "toList"), prims.prim1(qname(ns, "toList"), sets.toList as any, ["a"], sa, prims.list_(a)));
  m.set(qname(ns, "union"), prims.prim2(qname(ns, "union"), sets.union as any, ["a"], sa, sa, sa));
  m.set(qname(ns, "unions"), prims.prim1(qname(ns, "unions"), sets.unions as any, ["a"], prims.list_(sa), sa));

  return m;
}

function registerRegexPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.regex";
  const m = new Map<string, Primitive>();

  m.set(qname(ns, "find"), prims.prim2(qname(ns, "find"), regex.find, [], prims.string(), prims.string(), prims.optional(prims.string())));
  m.set(qname(ns, "findAll"), prims.prim2(qname(ns, "findAll"), regex.findAll, [], prims.string(), prims.string(), prims.list_(prims.string())));
  m.set(qname(ns, "matches"), prims.prim2(qname(ns, "matches"), regex.matches, [], prims.string(), prims.string(), prims.boolean()));
  m.set(qname(ns, "replace"), prims.prim3(qname(ns, "replace"), regex.replace, [], prims.string(), prims.string(), prims.string(), prims.string()));
  m.set(qname(ns, "replaceAll"), prims.prim3(qname(ns, "replaceAll"), regex.replaceAll, [], prims.string(), prims.string(), prims.string(), prims.string()));
  m.set(qname(ns, "split"), prims.prim2(qname(ns, "split"), regex.split, [], prims.string(), prims.string(), prims.list_(prims.string())));

  return m;
}

function registerStringsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.strings";
  const m = new Map<string, Primitive>();

  m.set(qname(ns, "cat"), prims.prim1(qname(ns, "cat"), strings.cat, [], prims.list_(prims.string()), prims.string()));
  m.set(qname(ns, "cat2"), prims.prim2(qname(ns, "cat2"), strings.cat2, [], prims.string(), prims.string(), prims.string()));
  m.set(qname(ns, "charAt"), prims.prim2(qname(ns, "charAt"), strings.charAt, [], prims.int32(), prims.string(), prims.int32()));
  m.set(qname(ns, "fromList"), prims.prim1(qname(ns, "fromList"), strings.fromList, [], prims.list_(prims.int32()), prims.string()));
  m.set(qname(ns, "intercalate"), prims.prim2(qname(ns, "intercalate"), strings.intercalate, [], prims.string(), prims.list_(prims.string()), prims.string()));
  m.set(qname(ns, "length"), prims.prim1(qname(ns, "length"), strings.length, [], prims.string(), prims.int32()));
  m.set(qname(ns, "lines"), prims.prim1(qname(ns, "lines"), strings.lines, [], prims.string(), prims.list_(prims.string())));
  m.set(qname(ns, "maybeCharAt"), prims.prim2(qname(ns, "maybeCharAt"), strings.maybeCharAt, [], prims.int32(), prims.string(), prims.optional(prims.int32())));
  m.set(qname(ns, "null"), prims.prim1(qname(ns, "null"), strings.null_, [], prims.string(), prims.boolean()));
  m.set(qname(ns, "splitOn"), prims.prim2(qname(ns, "splitOn"), strings.splitOn, [], prims.string(), prims.string(), prims.list_(prims.string())));
  m.set(qname(ns, "toList"), prims.prim1(qname(ns, "toList"), strings.toList, [], prims.string(), prims.list_(prims.int32())));
  m.set(qname(ns, "toLower"), prims.prim1(qname(ns, "toLower"), strings.toLower, [], prims.string(), prims.string()));
  m.set(qname(ns, "toUpper"), prims.prim1(qname(ns, "toUpper"), strings.toUpper, [], prims.string(), prims.string()));
  m.set(qname(ns, "unlines"), prims.prim1(qname(ns, "unlines"), strings.unlines, [], prims.list_(prims.string()), prims.string()));

  return m;
}

function registerLiteralsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.literals";
  const m = new Map<string, Primitive>();

  // Conversion primitives
  m.set(qname(ns, "bigfloatToBigint"), prims.prim1(qname(ns, "bigfloatToBigint"), literals.bigfloatToBigint, [], prims.bigfloat(), prims.bigint_()));
  m.set(qname(ns, "bigfloatToFloat32"), prims.prim1(qname(ns, "bigfloatToFloat32"), literals.bigfloatToFloat32, [], prims.bigfloat(), prims.float32()));
  m.set(qname(ns, "bigfloatToFloat64"), prims.prim1(qname(ns, "bigfloatToFloat64"), literals.bigfloatToFloat64, [], prims.bigfloat(), prims.float64()));
  m.set(qname(ns, "bigintToBigfloat"), prims.prim1(qname(ns, "bigintToBigfloat"), literals.bigintToBigfloat, [], prims.bigint_(), prims.bigfloat()));
  m.set(qname(ns, "bigintToInt8"), prims.prim1(qname(ns, "bigintToInt8"), literals.bigintToInt8, [], prims.bigint_(), prims.int8()));
  m.set(qname(ns, "bigintToInt16"), prims.prim1(qname(ns, "bigintToInt16"), literals.bigintToInt16, [], prims.bigint_(), prims.int16()));
  m.set(qname(ns, "bigintToInt32"), prims.prim1(qname(ns, "bigintToInt32"), literals.bigintToInt32, [], prims.bigint_(), prims.int32()));
  m.set(qname(ns, "bigintToInt64"), prims.prim1(qname(ns, "bigintToInt64"), literals.bigintToInt64, [], prims.bigint_(), prims.int64()));
  m.set(qname(ns, "bigintToUint8"), prims.prim1(qname(ns, "bigintToUint8"), literals.bigintToUint8, [], prims.bigint_(), prims.uint8()));
  m.set(qname(ns, "bigintToUint16"), prims.prim1(qname(ns, "bigintToUint16"), literals.bigintToUint16, [], prims.bigint_(), prims.uint16()));
  m.set(qname(ns, "bigintToUint32"), prims.prim1(qname(ns, "bigintToUint32"), literals.bigintToUint32, [], prims.bigint_(), prims.uint32()));
  m.set(qname(ns, "bigintToUint64"), prims.prim1(qname(ns, "bigintToUint64"), literals.bigintToUint64, [], prims.bigint_(), prims.uint64()));
  m.set(qname(ns, "binaryToBytes"), prims.prim1(qname(ns, "binaryToBytes"), literals.binaryToBytes, [], prims.binary(), prims.list_(prims.int32())));
  m.set(qname(ns, "binaryToString"), prims.prim1(qname(ns, "binaryToString"), literals.binaryToString, [], prims.binary(), prims.string()));
  m.set(qname(ns, "float32ToBigfloat"), prims.prim1(qname(ns, "float32ToBigfloat"), literals.float32ToBigfloat, [], prims.float32(), prims.bigfloat()));
  m.set(qname(ns, "float64ToBigfloat"), prims.prim1(qname(ns, "float64ToBigfloat"), literals.float64ToBigfloat, [], prims.float64(), prims.bigfloat()));
  m.set(qname(ns, "int8ToBigint"), prims.prim1(qname(ns, "int8ToBigint"), literals.int8ToBigint, [], prims.int8(), prims.bigint_()));
  m.set(qname(ns, "int16ToBigint"), prims.prim1(qname(ns, "int16ToBigint"), literals.int16ToBigint, [], prims.int16(), prims.bigint_()));
  m.set(qname(ns, "int32ToBigint"), prims.prim1(qname(ns, "int32ToBigint"), literals.int32ToBigint, [], prims.int32(), prims.bigint_()));
  m.set(qname(ns, "int64ToBigint"), prims.prim1(qname(ns, "int64ToBigint"), literals.int64ToBigint, [], prims.int64(), prims.bigint_()));
  m.set(qname(ns, "uint8ToBigint"), prims.prim1(qname(ns, "uint8ToBigint"), literals.uint8ToBigint, [], prims.uint8(), prims.bigint_()));
  m.set(qname(ns, "uint16ToBigint"), prims.prim1(qname(ns, "uint16ToBigint"), literals.uint16ToBigint, [], prims.uint16(), prims.bigint_()));
  m.set(qname(ns, "uint32ToBigint"), prims.prim1(qname(ns, "uint32ToBigint"), literals.uint32ToBigint, [], prims.uint32(), prims.bigint_()));
  m.set(qname(ns, "uint64ToBigint"), prims.prim1(qname(ns, "uint64ToBigint"), literals.uint64ToBigint, [], prims.uint64(), prims.bigint_()));
  m.set(qname(ns, "stringToBinary"), prims.prim1(qname(ns, "stringToBinary"), literals.stringToBinary, [], prims.string(), prims.binary()));

  // Read primitives
  m.set(qname(ns, "readBigfloat"), prims.prim1(qname(ns, "readBigfloat"), literals.readBigfloat, [], prims.string(), prims.optional(prims.bigfloat())));
  m.set(qname(ns, "readBigint"), prims.prim1(qname(ns, "readBigint"), literals.readBigint, [], prims.string(), prims.optional(prims.bigint_())));
  m.set(qname(ns, "readBoolean"), prims.prim1(qname(ns, "readBoolean"), literals.readBoolean, [], prims.string(), prims.optional(prims.boolean())));
  m.set(qname(ns, "readFloat32"), prims.prim1(qname(ns, "readFloat32"), literals.readFloat32, [], prims.string(), prims.optional(prims.float32())));
  m.set(qname(ns, "readFloat64"), prims.prim1(qname(ns, "readFloat64"), literals.readFloat64, [], prims.string(), prims.optional(prims.float64())));
  m.set(qname(ns, "readInt8"), prims.prim1(qname(ns, "readInt8"), literals.readInt8, [], prims.string(), prims.optional(prims.int8())));
  m.set(qname(ns, "readInt16"), prims.prim1(qname(ns, "readInt16"), literals.readInt16, [], prims.string(), prims.optional(prims.int16())));
  m.set(qname(ns, "readInt32"), prims.prim1(qname(ns, "readInt32"), literals.readInt32, [], prims.string(), prims.optional(prims.int32())));
  m.set(qname(ns, "readInt64"), prims.prim1(qname(ns, "readInt64"), literals.readInt64, [], prims.string(), prims.optional(prims.int64())));
  m.set(qname(ns, "readString"), prims.prim1(qname(ns, "readString"), literals.readString, [], prims.string(), prims.optional(prims.string())));
  m.set(qname(ns, "readUint8"), prims.prim1(qname(ns, "readUint8"), literals.readUint8, [], prims.string(), prims.optional(prims.uint8())));
  m.set(qname(ns, "readUint16"), prims.prim1(qname(ns, "readUint16"), literals.readUint16, [], prims.string(), prims.optional(prims.uint16())));
  m.set(qname(ns, "readUint32"), prims.prim1(qname(ns, "readUint32"), literals.readUint32, [], prims.string(), prims.optional(prims.uint32())));
  m.set(qname(ns, "readUint64"), prims.prim1(qname(ns, "readUint64"), literals.readUint64, [], prims.string(), prims.optional(prims.uint64())));

  // Show primitives
  m.set(qname(ns, "showBigfloat"), prims.prim1(qname(ns, "showBigfloat"), literals.showBigfloat, [], prims.bigfloat(), prims.string()));
  m.set(qname(ns, "showBigint"), prims.prim1(qname(ns, "showBigint"), literals.showBigint, [], prims.bigint_(), prims.string()));
  m.set(qname(ns, "showBoolean"), prims.prim1(qname(ns, "showBoolean"), literals.showBoolean, [], prims.boolean(), prims.string()));
  m.set(qname(ns, "showFloat32"), prims.prim1(qname(ns, "showFloat32"), literals.showFloat32, [], prims.float32(), prims.string()));
  m.set(qname(ns, "showFloat64"), prims.prim1(qname(ns, "showFloat64"), literals.showFloat64, [], prims.float64(), prims.string()));
  m.set(qname(ns, "showInt8"), prims.prim1(qname(ns, "showInt8"), literals.showInt8, [], prims.int8(), prims.string()));
  m.set(qname(ns, "showInt16"), prims.prim1(qname(ns, "showInt16"), literals.showInt16, [], prims.int16(), prims.string()));
  m.set(qname(ns, "showInt32"), prims.prim1(qname(ns, "showInt32"), literals.showInt32, [], prims.int32(), prims.string()));
  m.set(qname(ns, "showInt64"), prims.prim1(qname(ns, "showInt64"), literals.showInt64, [], prims.int64(), prims.string()));
  m.set(qname(ns, "showString"), prims.prim1(qname(ns, "showString"), literals.showString, [], prims.string(), prims.string()));
  m.set(qname(ns, "showUint8"), prims.prim1(qname(ns, "showUint8"), literals.showUint8, [], prims.uint8(), prims.string()));
  m.set(qname(ns, "showUint16"), prims.prim1(qname(ns, "showUint16"), literals.showUint16, [], prims.uint16(), prims.string()));
  m.set(qname(ns, "showUint32"), prims.prim1(qname(ns, "showUint32"), literals.showUint32, [], prims.uint32(), prims.string()));
  m.set(qname(ns, "showUint64"), prims.prim1(qname(ns, "showUint64"), literals.showUint64, [], prims.uint64(), prims.string()));

  return m;
}

function registerPairsPrimitives(): ReadonlyMap<string, Primitive> {
  const ns = "hydra.lib.pairs";
  const m = new Map<string, Primitive>();
  const a = prims.variable("a");
  const b = prims.variable("b");

  m.set(qname(ns, "first"), prims.prim1(qname(ns, "first"), pairs.first, ["a", "b"], prims.pair(a, b), a));
  m.set(qname(ns, "second"), prims.prim1(qname(ns, "second"), pairs.second, ["a", "b"], prims.pair(a, b), b));

  return m;
}

/**
 * Returns all standard library primitives as a map from qualified name to Primitive.
 */
export function standardLibrary(): ReadonlyMap<string, Primitive> {
  const all = new Map<string, Primitive>();
  const categories = [
    registerCharsPrimitives(),
    registerEithersPrimitives(),
    registerEqualityPrimitives(),
    registerListsPrimitives(),
    registerLiteralsPrimitives(),
    registerLogicPrimitives(),
    registerMapsPrimitives(),
    registerMathPrimitives(),
    registerMaybesPrimitives(),
    registerPairsPrimitives(),
    registerRegexPrimitives(),
    registerSetsPrimitives(),
    registerStringsPrimitives(),
  ];
  for (const cat of categories) {
    for (const [k, v] of cat) {
      all.set(k, v);
    }
  }
  return all;
}
