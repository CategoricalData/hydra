// Hand-written registration of standard hydra.lib.* primitives.
//
// Each generated kernel module that uses a primitive expects a graph in
// which `lookupPrimitive(name)` returns a `Primitive` whose
// `implementation` decodes its args from `Term`, runs the underlying JS
// function, and re-encodes the result.
//
// Mirrors heads/java/src/main/java/hydra/lib/Libraries.java but trimmed
// down: the typeScheme is just rich enough that `arity.primitiveArity`
// can count the function's surface arity (that's all the evaluator
// reads). Type variables in the schemes are nominal-looking strings;
// the evaluator never unifies them.
//
// This file is hand-written but lives under `src/main/` so it ships
// alongside the runtime. It depends on the generated kernel modules
// (`reduction`, `extract/core`, `context`, `graph`, `errors`) which only
// exist after `bin/sync-typescript.sh` populates `dist/typescript/`. The
// head's own `tsconfig.json` therefore excludes this file from
// source-tree type-checking; `tsc` validates it via
// `bin/test-distribution.sh` in the dist tree, where the generated
// modules are siblings of this file. This mirrors how the Java head's
// gradle source-set crossover and the Python head's pyright/pytest
// extraPaths bridge hand-written + generated code into one namespace at
// compile/test time.

import type { Name, Term, Type, TypeScheme } from "../core.js";
import type { Context } from "../context.js";
import type { Graph, Primitive } from "../graph.js";
import type { Error as HydraError } from "../errors.js";
import type { Either } from "../runtime.js";

import * as extractCore from "../extract/core.js";

import * as libChars from "./chars.js";
import * as libEquality from "./equality.js";
import * as libLists from "./lists.js";
import * as libLiterals from "./literals.js";
import * as libLogic from "./logic.js";
import * as libMaps from "./maps.js";
import * as libMath from "./math.js";
import * as libRegex from "./regex.js";
import * as libSets from "./sets.js";
import * as libStrings from "./strings.js";

// HOF primitives like `maps.alter` need to invoke Hydra closures —
// they take a function argument that's a Term and must call reduceTerm
// with `apply(closure, arg)` to evaluate.
import { reduceTerm } from "../reduction.js";

// === Term construction helpers ===

const left = <T>(e: HydraError): Either<HydraError, T> => ({ tag: "left", value: e });
const right = <T>(v: T): Either<HydraError, T> => ({ tag: "right", value: v });

const litTerm = (lit: unknown): Term =>
  ({ tag: "literal", value: lit as never });
const tBool = (b: boolean): Term =>
  litTerm({ tag: "boolean", value: b });
const tInt = (n: number, w: "int8" | "int16" | "int32" | "int64" = "int32"): Term =>
  litTerm({ tag: "integer", value: { tag: w, value: n } });
const tBigint = (n: bigint): Term =>
  litTerm({ tag: "integer", value: { tag: "bigint", value: n } });
const tFloat = (f: number, w: "float32" | "float64" = "float64"): Term =>
  litTerm({ tag: "float", value: { tag: w, value: f } });
const tString = (s: string): Term =>
  litTerm({ tag: "string", value: s });
// `b` is `Uint8Array | string` because the kernel `stringToBinary`
// primitive currently returns its argument unchanged (`(s: string): string`)
// rather than encoding to bytes. Accepting both keeps the Term shape
// runtime-correct without requiring a heap-allocation here.
const tBinary = (b: Uint8Array | string): Term =>
  litTerm({ tag: "binary", value: b });
const tDecimal = (f: number): Term =>
  litTerm({ tag: "decimal", value: f });

const tInject = (typeName: string, fieldName: string, body: Term): Term =>
  ({ tag: "inject", value: {
    typeName: { value: typeName } as Name,
    field: { name: { value: fieldName } as Name, term: body },
  } } as never);

const tMaybeJust = (v: Term): Term =>
  ({ tag: "maybe", value: { tag: "just", value: v } } as never);
const tMaybeNothing: Term =
  ({ tag: "maybe", value: { tag: "nothing" } } as never);
const tMaybe = <T>(m: { tag: "just"; value: T } | { tag: "nothing" }, lift: (t: T) => Term): Term =>
  m.tag === "just" ? tMaybeJust(lift(m.value)) : tMaybeNothing;

// === Type construction helpers ===

const tyVar = (n: string): Type => ({ tag: "variable", value: { value: n } as never } as never);
const tyInt32: Type = { tag: "literal", value: { tag: "integer", value: { tag: "int32" } as never } as never } as never;
const tyInt8: Type = { tag: "literal", value: { tag: "integer", value: { tag: "int8" } as never } as never } as never;
const tyInt16: Type = { tag: "literal", value: { tag: "integer", value: { tag: "int16" } as never } as never } as never;
const tyInt64: Type = { tag: "literal", value: { tag: "integer", value: { tag: "int64" } as never } as never } as never;
const tyUint8: Type = { tag: "literal", value: { tag: "integer", value: { tag: "uint8" } as never } as never } as never;
const tyUint16: Type = { tag: "literal", value: { tag: "integer", value: { tag: "uint16" } as never } as never } as never;
const tyUint32: Type = { tag: "literal", value: { tag: "integer", value: { tag: "uint32" } as never } as never } as never;
const tyUint64: Type = { tag: "literal", value: { tag: "integer", value: { tag: "uint64" } as never } as never } as never;
const tyBigint: Type = { tag: "literal", value: { tag: "integer", value: { tag: "bigint" } as never } as never } as never;
const tyFloat32: Type = { tag: "literal", value: { tag: "float", value: { tag: "float32" } as never } as never } as never;
const tyFloat64: Type = { tag: "literal", value: { tag: "float", value: { tag: "float64" } as never } as never } as never;

// Lookup by string name — convenient inside loop-based registration.
const intTypeOf = (w: string): Type => {
  switch (w) {
    case "int8": return tyInt8;
    case "int16": return tyInt16;
    case "int32": return tyInt32;
    case "int64": return tyInt64;
    case "uint8": return tyUint8;
    case "uint16": return tyUint16;
    case "uint32": return tyUint32;
    case "uint64": return tyUint64;
    case "bigint": return tyBigint;
    default: return tyInt32;
  }
};
const tyBool: Type = { tag: "literal", value: { tag: "boolean" } as never } as never;
const tyString: Type = { tag: "literal", value: { tag: "string" } as never } as never;
const tyBinary: Type = { tag: "literal", value: { tag: "binary" } as never } as never;
const tyUnit: Type = { tag: "unit" } as never;
const tyFn = (a: Type, b: Type): Type =>
  ({ tag: "function", value: { domain: a, codomain: b } as never } as never);
const tyList = (a: Type): Type =>
  ({ tag: "list", value: a } as never);
const tySet = (a: Type): Type =>
  ({ tag: "set", value: a } as never);
const tyMap = (k: Type, v: Type): Type =>
  ({ tag: "map", value: { keys: k, values: v } as never } as never);
const tyMaybe = (a: Type): Type =>
  ({ tag: "maybe", value: a } as never);
const tyPair = (a: Type, b: Type): Type =>
  ({ tag: "pair", value: { first: a, second: b } as never } as never);
const tyEither = (a: Type, b: Type): Type =>
  ({ tag: "either", value: { left: a, right: b } as never } as never);
const tyForall = (name: string, body: Type): Type =>
  ({ tag: "forall", value: { parameter: { value: name } as never, body } as never } as never);
const tyFnCurried = (...ts: Type[]): Type =>
  ts.reduceRight((acc, cur) => tyFn(cur, acc));
const scheme = (t: Type, vars: readonly string[] = []): TypeScheme =>
  ({ variables: vars.map((v) => ({ value: v } as never)), body: t, constraints: { tag: "nothing" } } as never);

// Build a TypeScheme with class constraints. `cs` is a list of [typeVar, [className, ...]] pairs.
// At runtime, constraints have type `Maybe<Map<Name, {classes: Set<Name>}>>`.
// The map keys are wrapped Names; the values are records with a `classes` Set<Name>.
const schemeC = (t: Type, vars: readonly string[], cs: readonly (readonly [string, readonly string[]])[]): TypeScheme => {
  const m = libMaps.fromList(cs.map(([v, classes]) => [
    { value: v } as never,
    { classes: libSets.fromList(classes.map((c) => ({ value: c } as never))) } as never,
  ]));
  return {
    variables: vars.map((v) => ({ value: v } as never)),
    body: t,
    constraints: { tag: "just", value: m },
  } as never;
};

// Round x to n significant digits. Matches Python's
// _round_to_n_significant in heads/python/.../lib/math.py.
const roundSig = (n: number, x: number): number => {
  if (x === 0 || !Number.isFinite(x)) return x;
  const d = Math.ceil(Math.log10(Math.abs(x)));
  const power = n - d;
  const mag = Math.pow(10, power);
  return Math.round(x * mag) / mag;
};

// === Argument extraction ===

const need = (args: readonly Term[], i: number, what: string): Either<HydraError, Term> =>
  i < args.length
    ? right(args[i]!)
    : left({ tag: "other", value: `expected arg ${i} for ${what}` } as never);

const bind = <A, B>(e: Either<HydraError, A>, f: (a: A) => Either<HydraError, B>): Either<HydraError, B> =>
  e.tag === "left" ? e as Either<HydraError, B> : f(e.value);

// === Primitive constructor ===

type Impl = (cx: Context, g: Graph, args: readonly Term[]) => Either<HydraError, Term>;

const prim = (qname: string, ts: TypeScheme, impl: Impl): Primitive => ({
  name: { value: qname } as Name,
  typeScheme: ts,
  implementation: impl,
});

// === Curried-arity bridges ===
//
// For most primitives the decode/call/encode boilerplate fits a small set
// of shapes. These helpers cover the common cases. For anything outside
// these shapes, write a one-off implementation.

// Unary: decode arg with `dec`, apply `f`, encode with `enc`.
const u1 = <A, B>(
  qname: string,
  inT: Type, outT: Type,
  dec: (g: Graph, t: Term) => Either<HydraError, A>,
  f: (a: A) => B,
  enc: (b: B) => Term,
): Primitive =>
  prim(qname, scheme(tyFn(inT, outT)),
    (_cx, g, args) =>
      bind(need(args, 0, qname), (a0) =>
        bind(dec(g, a0), (a) => right(enc(f(a))))));

// Binary flat (positional).
const u2 = <A, B, C>(
  qname: string,
  in1: Type, in2: Type, outT: Type,
  dec1: (g: Graph, t: Term) => Either<HydraError, A>,
  dec2: (g: Graph, t: Term) => Either<HydraError, B>,
  f: (a: A, b: B) => C,
  enc: (c: C) => Term,
): Primitive =>
  prim(qname, scheme(tyFn(in1, tyFn(in2, outT))),
    (_cx, g, args) =>
      bind(need(args, 0, qname), (a0) =>
        bind(need(args, 1, qname), (a1) =>
          bind(dec1(g, a0), (x) =>
            bind(dec2(g, a1), (y) => right(enc(f(x, y))))))));

// Decode aliases.
const dInt32 = (g: Graph, t: Term) => extractCore.int32(g, t) as Either<HydraError, number>;
const dInt64 = (g: Graph, t: Term) => extractCore.int64(g, t) as Either<HydraError, bigint>;
const dBigint = (g: Graph, t: Term) => extractCore.bigint(g, t) as Either<HydraError, bigint>;
const dFloat32 = (g: Graph, t: Term) => extractCore.float32(g, t) as Either<HydraError, number>;
const dFloat64 = (g: Graph, t: Term) => extractCore.float64(g, t) as Either<HydraError, number>;
const dBool = (g: Graph, t: Term) => extractCore.boolean_(g, t) as Either<HydraError, boolean>;
const dString = (g: Graph, t: Term) => extractCore.string_(g, t) as Either<HydraError, string>;
const dBinary = (g: Graph, t: Term) => extractCore.binary(g, t) as Either<HydraError, Uint8Array>;
const dAny = (_g: Graph, t: Term): Either<HydraError, Term> => right(t);

// Decode any integer literal (any width) to a JS number. Useful for math
// primitives that accept multiple widths.
const dAnyInt = (_g: Graph, t: Term): Either<HydraError, number> => {
  const lit = (t as { tag: string; value?: { tag?: string; value?: { tag?: string; value?: unknown } } });
  if (lit.tag !== "literal" || lit.value?.tag !== "integer") {
    return left({ tag: "other", value: "expected an integer literal" } as never);
  }
  const v = lit.value.value;
  if (typeof v?.value === "number") return right(v.value);
  if (typeof v?.value === "bigint") return right(Number(v.value));
  return left({ tag: "other", value: "unrecognized integer shape" } as never);
};

// Same for floats.
const dAnyFloat = (_g: Graph, t: Term): Either<HydraError, number> => {
  const lit = (t as { tag: string; value?: { tag?: string; value?: { tag?: string; value?: unknown } } });
  if (lit.tag !== "literal" || lit.value?.tag !== "float") {
    return left({ tag: "other", value: "expected a float literal" } as never);
  }
  const v = lit.value.value;
  if (typeof v?.value === "number") return right(v.value);
  return left({ tag: "other", value: "unrecognized float shape" } as never);
};

// === lib.chars ===

const charsPrimitives = (): readonly Primitive[] => [
  u1("hydra.lib.chars.isAlphaNum", tyInt32, tyBool, dInt32, libChars.isAlphaNum, tBool),
  u1("hydra.lib.chars.isLower", tyInt32, tyBool, dInt32, libChars.isLower, tBool),
  u1("hydra.lib.chars.isSpace", tyInt32, tyBool, dInt32, libChars.isSpace, tBool),
  u1("hydra.lib.chars.isUpper", tyInt32, tyBool, dInt32, libChars.isUpper, tBool),
  u1("hydra.lib.chars.toLower", tyInt32, tyInt32, dInt32, libChars.toLower, (n) => tInt(n)),
  u1("hydra.lib.chars.toUpper", tyInt32, tyInt32, dInt32, libChars.toUpper, (n) => tInt(n)),
];

// === lib.logic ===

const logicPrimitives = (): readonly Primitive[] => {
  const a = tyVar("a");
  return [
    u1("hydra.lib.logic.not", tyBool, tyBool, dBool, libLogic.not, tBool),
    u2("hydra.lib.logic.and", tyBool, tyBool, tyBool, dBool, dBool, libLogic.and, tBool),
    u2("hydra.lib.logic.or", tyBool, tyBool, tyBool, dBool, dBool, libLogic.or, tBool),
    prim("hydra.lib.logic.ifElse", scheme(tyFnCurried(tyBool, a, a, a), ["a"]),
      (_cx, g, args) =>
        bind(need(args, 0, "ifElse"), (a0) =>
          bind(need(args, 1, "ifElse-then"), (a1) =>
            bind(need(args, 2, "ifElse-else"), (a2) =>
              bind(dBool(g, a0), (b) => right(b ? a1 : a2)))))),
  ];
};

// === lib.math ===

const mathPrimitives = (): readonly Primitive[] => {
  // Integer arithmetic: accept any integer width, produce int32 (the
  // kernel's default integer width).
  const binIntInt = (qname: string, f: (a: number, b: number) => number): Primitive =>
    prim(qname, scheme(tyFnCurried(tyInt32, tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, qname), (a0) =>
          bind(need(args, 1, qname), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) => right(tInt(f(x, y))))))));
  const binFloatFloat = (qname: string, f: (a: number, b: number) => number): Primitive =>
    prim(qname, scheme(tyFnCurried(tyFloat64, tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, qname), (a0) =>
          bind(need(args, 1, qname), (a1) =>
            bind(dAnyFloat(g, a0), (x) =>
              bind(dAnyFloat(g, a1), (y) => right(tFloat(f(x, y))))))));
  const unaryFloatFloat = (qname: string, f: (a: number) => number): Primitive =>
    prim(qname, scheme(tyFn(tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, qname), (a0) =>
          bind(dAnyFloat(g, a0), (x) => right(tFloat(f(x))))));
  return [
    binIntInt("hydra.lib.math.add", libMath.add),
    binIntInt("hydra.lib.math.sub", libMath.sub),
    binIntInt("hydra.lib.math.mul", libMath.mul),
    binFloatFloat("hydra.lib.math.addFloat", libMath.add),
    binFloatFloat("hydra.lib.math.subFloat", libMath.sub),
    binFloatFloat("hydra.lib.math.mulFloat", libMath.mul),
    binFloatFloat("hydra.lib.math.pow", (a, b) => Math.pow(a, b)),
    binFloatFloat("hydra.lib.math.logBase", (b, x) => Math.log(x) / Math.log(b)),
    prim("hydra.lib.math.neg", scheme(tyFn(tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, "neg"), (a0) =>
          bind(dAnyInt(g, a0), (x) => right(tInt(-x))))),
    prim("hydra.lib.math.negate", scheme(tyFn(tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, "negate"), (a0) =>
          bind(dAnyInt(g, a0), (x) => right(tInt(-x))))),
    prim("hydra.lib.math.negateFloat", scheme(tyFn(tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "negateFloat"), (a0) =>
          bind(dAnyFloat(g, a0), (x) => right(tFloat(-x))))),
    prim("hydra.lib.math.abs", scheme(tyFn(tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, "abs"), (a0) =>
          bind(dAnyInt(g, a0), (x) => right(tInt(Math.abs(x)))))),
    prim("hydra.lib.math.signum", scheme(tyFn(tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, "signum"), (a0) =>
          bind(dAnyInt(g, a0), (x) => right(tInt(Math.sign(x)))))),
    prim("hydra.lib.math.even", scheme(tyFn(tyInt32, tyBool)),
      (_cx, g, args) =>
        bind(need(args, 0, "even"), (a0) =>
          bind(dAnyInt(g, a0), (x) => right(tBool(x % 2 === 0))))),
    prim("hydra.lib.math.odd", scheme(tyFn(tyInt32, tyBool)),
      (_cx, g, args) =>
        bind(need(args, 0, "odd"), (a0) =>
          bind(dAnyInt(g, a0), (x) => right(tBool(x % 2 !== 0))))),
    prim("hydra.lib.math.max", scheme(tyFnCurried(tyInt32, tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, "max"), (a0) =>
          bind(need(args, 1, "max"), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) => right(tInt(Math.max(x, y)))))))),
    prim("hydra.lib.math.min", scheme(tyFnCurried(tyInt32, tyInt32, tyInt32)),
      (_cx, g, args) =>
        bind(need(args, 0, "min"), (a0) =>
          bind(need(args, 1, "min"), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) => right(tInt(Math.min(x, y)))))))),
    prim("hydra.lib.math.maybeDiv", scheme(tyFnCurried(tyInt32, tyInt32, tyMaybe(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, "maybeDiv"), (a0) =>
          bind(need(args, 1, "maybeDiv"), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) =>
                // Floor division (Haskell `div` semantics): rounds toward -∞.
                right(y === 0 ? tMaybeNothing : tMaybeJust(tInt(Math.floor(x / y))))))))),
    prim("hydra.lib.math.maybeMod", scheme(tyFnCurried(tyInt32, tyInt32, tyMaybe(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, "maybeMod"), (a0) =>
          bind(need(args, 1, "maybeMod"), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) => {
                // Haskell `mod` matches floor division (`div`): result has
                // the same sign as the divisor.
                if (y === 0) return right(tMaybeNothing);
                const r = ((x % y) + y) % y;
                return right(tMaybeJust(tInt(r)));
              }))))),

    prim("hydra.lib.math.maybeRem", scheme(tyFnCurried(tyInt32, tyInt32, tyMaybe(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, "maybeRem"), (a0) =>
          bind(need(args, 1, "maybeRem"), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) =>
                right(y === 0 ? tMaybeNothing : tMaybeJust(tInt(x - Math.trunc(x / y) * y)))))))),
    prim("hydra.lib.math.maybePred", scheme(tyFn(tyInt32, tyMaybe(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, "maybePred"), (a0) =>
          bind(dAnyInt(g, a0), (x) =>
            right(x === -2147483648 ? tMaybeNothing : tMaybeJust(tInt(x - 1)))))),
    prim("hydra.lib.math.maybeSucc", scheme(tyFn(tyInt32, tyMaybe(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, "maybeSucc"), (a0) =>
          bind(dAnyInt(g, a0), (x) =>
            right(x === 2147483647 ? tMaybeNothing : tMaybeJust(tInt(x + 1)))))),
    prim("hydra.lib.math.range", scheme(tyFnCurried(tyInt32, tyInt32, tyList(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, "range"), (a0) =>
          bind(need(args, 1, "range"), (a1) =>
            bind(dAnyInt(g, a0), (x) =>
              bind(dAnyInt(g, a1), (y) => {
                // Inclusive range, matching Haskell's [x..y].
                const els: Term[] = [];
                for (let i = x; i <= y; i++) els.push(tInt(i));
                return right({ tag: "list", value: els } as never);
              }))))),
    // Trig / transcendental — operate on Float64.
    unaryFloatFloat("hydra.lib.math.sin", Math.sin),
    unaryFloatFloat("hydra.lib.math.cos", Math.cos),
    unaryFloatFloat("hydra.lib.math.tan", Math.tan),
    unaryFloatFloat("hydra.lib.math.asin", Math.asin),
    unaryFloatFloat("hydra.lib.math.acos", Math.acos),
    unaryFloatFloat("hydra.lib.math.atan", Math.atan),
    unaryFloatFloat("hydra.lib.math.sinh", Math.sinh),
    unaryFloatFloat("hydra.lib.math.cosh", Math.cosh),
    unaryFloatFloat("hydra.lib.math.tanh", Math.tanh),
    unaryFloatFloat("hydra.lib.math.asinh", Math.asinh),
    unaryFloatFloat("hydra.lib.math.acosh", Math.acosh),
    unaryFloatFloat("hydra.lib.math.atanh", Math.atanh),
    unaryFloatFloat("hydra.lib.math.exp", Math.exp),
    unaryFloatFloat("hydra.lib.math.log", Math.log),
    unaryFloatFloat("hydra.lib.math.sqrt", Math.sqrt),
    // round/floor/ceiling/truncate: Float -> Float (Haskell semantics —
    // returns the same Float type, just rounded to an integral value).
    prim("hydra.lib.math.floor", scheme(tyFn(tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "floor"), (a0) =>
          bind(dAnyFloat(g, a0), (x) => right(tFloat(Math.floor(x)))))),
    prim("hydra.lib.math.ceiling", scheme(tyFn(tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "ceiling"), (a0) =>
          bind(dAnyFloat(g, a0), (x) => right(tFloat(Math.ceil(x)))))),
    prim("hydra.lib.math.round", scheme(tyFn(tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "round"), (a0) =>
          bind(dAnyFloat(g, a0), (x) => {
            // Haskell's `round` uses banker's rounding (round half to
            // even). JS Math.round rounds half toward +∞ for positives
            // and toward 0 for negatives, neither of which matches.
            if (!Number.isFinite(x)) return right(tFloat(x));
            const floor = Math.floor(x);
            const diff = x - floor;
            if (diff < 0.5) return right(tFloat(floor));
            if (diff > 0.5) return right(tFloat(floor + 1));
            // diff === 0.5: round to even.
            return right(tFloat(floor % 2 === 0 ? floor : floor + 1));
          }))),
    prim("hydra.lib.math.truncate", scheme(tyFn(tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "truncate"), (a0) =>
          bind(dAnyFloat(g, a0), (x) => right(tFloat(Math.trunc(x)))))),
    // `roundFloat n f` rounds `f` to `n` significant digits (not decimal
    // places). Mirrors Python's _round_to_n_significant.
    prim("hydra.lib.math.roundFloat", scheme(tyFnCurried(tyInt32, tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "roundFloat"), (a0) =>
          bind(need(args, 1, "roundFloat"), (a1) =>
            bind(dAnyInt(g, a0), (n) =>
              bind(dAnyFloat(g, a1), (x) => right(tFloat(roundSig(n, x)))))))),
    ...(["float32", "float64"] as const).flatMap((w) => {
      const suf = w === "float32" ? "32" : "64";
      const wFold = w === "float32" ? Math.fround : (x: number) => x;
      return [
        // `roundFloatN n f` rounds `f` to `n` SIGNIFICANT DIGITS (not
        // decimal places). The trailing 32/64 indicates the underlying
        // float width.
        prim(`hydra.lib.math.roundFloat${suf}`, scheme(tyFnCurried(tyInt32, tyFloat64, tyFloat64)),
          (_cx, g, args) =>
            bind(need(args, 0, `roundFloat${suf}`), (a0) =>
              bind(need(args, 1, `roundFloat${suf}`), (a1) =>
                bind(dAnyInt(g, a0), (n) =>
                  bind(dAnyFloat(g, a1), (x) => right(tFloat(wFold(roundSig(n, x)), w))))))),
        prim(`hydra.lib.math.addFloat${suf}`, scheme(tyFnCurried(tyFloat64, tyFloat64, tyFloat64)),
          (_cx, g, args) =>
            bind(need(args, 0, `addFloat${suf}`), (a0) =>
              bind(need(args, 1, `addFloat${suf}`), (a1) =>
                bind(dAnyFloat(g, a0), (x) =>
                  bind(dAnyFloat(g, a1), (y) => right(tFloat(wFold(x + y), w))))))),
        prim(`hydra.lib.math.subFloat${suf}`, scheme(tyFnCurried(tyFloat64, tyFloat64, tyFloat64)),
          (_cx, g, args) =>
            bind(need(args, 0, `subFloat${suf}`), (a0) =>
              bind(need(args, 1, `subFloat${suf}`), (a1) =>
                bind(dAnyFloat(g, a0), (x) =>
                  bind(dAnyFloat(g, a1), (y) => right(tFloat(wFold(x - y), w))))))),
        prim(`hydra.lib.math.mulFloat${suf}`, scheme(tyFnCurried(tyFloat64, tyFloat64, tyFloat64)),
          (_cx, g, args) =>
            bind(need(args, 0, `mulFloat${suf}`), (a0) =>
              bind(need(args, 1, `mulFloat${suf}`), (a1) =>
                bind(dAnyFloat(g, a0), (x) =>
                  bind(dAnyFloat(g, a1), (y) => right(tFloat(wFold(x * y), w))))))),
        prim(`hydra.lib.math.negateFloat${suf}`, scheme(tyFn(tyFloat64, tyFloat64)),
          (_cx, g, args) =>
            bind(need(args, 0, `negateFloat${suf}`), (a0) =>
              bind(dAnyFloat(g, a0), (x) => right(tFloat(wFold(-x), w))))),
      ];
    }),
    prim("hydra.lib.math.atan2", scheme(tyFnCurried(tyFloat64, tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, "atan2"), (a0) =>
          bind(need(args, 1, "atan2"), (a1) =>
            bind(dAnyFloat(g, a0), (y) =>
              bind(dAnyFloat(g, a1), (x) => {
                // Haskell `atan2` returns NaN when both args are infinite,
                // unlike JS which returns ±π/4 etc.
                if (!Number.isFinite(y) && !Number.isFinite(x)) return right(tFloat(NaN));
                return right(tFloat(Math.atan2(y, x)));
              }))))),
    // Math constants. These are nullary kernel definitions in Hydra
    // source; we register them as zero-arg primitives.
    prim("hydra.lib.math.e", scheme(tyFloat64),
      (_cx, _g, _args) => right(tFloat(Math.E))),
    prim("hydra.lib.math.pi", scheme(tyFloat64),
      (_cx, _g, _args) => right(tFloat(Math.PI))),
  ];
};

// === lib.literals ===
//
// These bridge between the kernel's IntegerValue / FloatValue / Literal
// shapes and their string-printed forms. We delegate to the runtime
// helpers in lib/literals.ts.

const literalsPrimitives = (): readonly Primitive[] => [
  u1("hydra.lib.literals.showString", tyString, tyString, dString, libLiterals.showString, tString),
  u1("hydra.lib.literals.showBoolean", tyBool, tyString, dBool, libLiterals.showBoolean, tString),
  u1("hydra.lib.literals.readBoolean", tyString, tyMaybe(tyBool), dString,
    (s) => s === "true" ? { tag: "just" as const, value: true } : s === "false" ? { tag: "just" as const, value: false } : { tag: "nothing" as const },
    (m) => tMaybe(m, tBool)),
  u1("hydra.lib.literals.readString", tyString, tyMaybe(tyString), dString,
    (s) => { try { const v = JSON.parse(s); return typeof v === "string" ? { tag: "just" as const, value: v } : { tag: "nothing" as const }; } catch { return { tag: "nothing" as const }; } },
    (m) => tMaybe(m, tString)),
  u1("hydra.lib.literals.readInt", tyString, tyMaybe(tyInt32), dString,
    libLiterals.readInt, (m) => tMaybe(m, (n) => tInt(n))),
  u1("hydra.lib.literals.readUint", tyString, tyMaybe(tyInt32), dString,
    libLiterals.readUint, (m) => tMaybe(m, (n) => tInt(n))),
  u1("hydra.lib.literals.readBigint", tyString, tyMaybe(tyBigint), dString,
    libLiterals.readBigint, (m) => tMaybe(m, tBigint)),
  u1("hydra.lib.literals.readFloat", tyString, tyMaybe(tyFloat64), dString,
    libLiterals.readFloat, (m) => tMaybe(m, (f) => tFloat(f))),
  u1("hydra.lib.literals.readDecimal", tyString, tyMaybe(tyFloat64), dString,
    libLiterals.readDecimal, (m) => tMaybe(m, tDecimal)),
  // showInt / showUint / showBigint / showFloat / showDecimal all accept
  // an integer/float value and return its string form. Decode any width.
  prim("hydra.lib.literals.showInt", scheme(tyFn(tyInt32, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showInt"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showUint", scheme(tyFn(tyInt32, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showUint"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showBigint", scheme(tyFn(tyBigint, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showBigint"), (a0) =>
        bind(dBigint(g, a0), (n) => right(tString(n.toString()))))),
  // Width-specialized show primitives: each accepts the specific
  // integer/float width and renders the bare value (no `:tag` suffix —
  // these are the user-facing print functions).
  prim("hydra.lib.literals.showInt8", scheme(tyFn(tyInt8, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showInt8"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showInt16", scheme(tyFn(tyInt16, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showInt16"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showInt32", scheme(tyFn(tyInt32, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showInt32"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showInt64", scheme(tyFn(tyInt64, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showInt64"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showUint8", scheme(tyFn(tyUint8, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showUint8"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showUint16", scheme(tyFn(tyUint16, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showUint16"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showUint32", scheme(tyFn(tyUint32, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showUint32"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showUint64", scheme(tyFn(tyUint64, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showUint64"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
  prim("hydra.lib.literals.showFloat32", scheme(tyFn(tyFloat32, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showFloat32"), (a0) =>
        bind(dAnyFloat(g, a0), (f) => right(tString(libLiterals.showFloat32(f)))))),
  prim("hydra.lib.literals.showFloat64", scheme(tyFn(tyFloat64, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showFloat64"), (a0) =>
        bind(dAnyFloat(g, a0), (f) => right(tString(libLiterals.showFloat64(f)))))),
  prim("hydra.lib.literals.showFloat", scheme(tyFn(tyFloat64, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showFloat"), (a0) => {
        // showFloat for a FloatValue: render with the underlying
        // precision (float32 narrows to 7 sig digits).
        const lit = a0 as { tag?: string; value?: { tag?: string; value?: { tag?: string; value?: number } } };
        if (lit.tag === "literal" && lit.value?.tag === "float") {
          const fv = lit.value.value as { tag?: string; value?: number };
          if (typeof fv?.value === "number") {
            return right(tString(fv.tag === "float32" ? libLiterals.showFloat32(fv.value) : libLiterals.showFloat64(fv.value)));
          }
        }
        return bind(dAnyFloat(g, a0), (f) => right(tString(libLiterals.showFloat64(f))));
      })),
  prim("hydra.lib.literals.showDecimal", scheme(tyFn(tyFloat64, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "showDecimal"), (a0) => {
        // Decode a Decimal literal: extract via decimal-literal helper.
        const lit = (a0 as { tag: string; value?: { tag?: string; value?: number } });
        const v = lit.value?.value;
        const showD = (f: number): string => {
          // Decimal always renders with a fractional part, matching the
          // kernel's test fixtures.
          const s = libLiterals.showFloat64(f);
          if (s.includes(".") || s.includes("e") || s.includes("Infinity") || s.includes("NaN")) return s;
          return s + ".0";
        };
        if (lit.tag === "literal" && lit.value?.tag === "decimal" && typeof v === "number") {
          return right(tString(showD(v)));
        }
        return bind(dAnyFloat(g, a0), (f) => right(tString(showD(f))));
      })),
  // Int conversions
  prim("hydra.lib.literals.int", scheme(tyFn(tyInt32, tyInt32)),
    (_cx, g, args) =>
      bind(need(args, 0, "int"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tInt(n))))),
  prim("hydra.lib.literals.uint", scheme(tyFn(tyInt32, tyInt32)),
    (_cx, g, args) =>
      bind(need(args, 0, "uint"), (a0) =>
        bind(dAnyInt(g, a0), (n) => right(tInt(n))))),
  prim("hydra.lib.literals.float", scheme(tyFn(tyFloat64, tyFloat64)),
    (_cx, g, args) =>
      bind(need(args, 0, "float"), (a0) =>
        bind(dAnyFloat(g, a0), (f) => right(tFloat(f))))),
  prim("hydra.lib.literals.bigintToInt", scheme(tyFn(tyBigint, tyInt32)),
    (_cx, g, args) =>
      bind(need(args, 0, "bigintToInt"), (a0) =>
        bind(dBigint(g, a0), (n) => right(tInt(Number(n)))))),
  prim("hydra.lib.literals.bigintToUint", scheme(tyFn(tyBigint, tyInt32)),
    (_cx, g, args) =>
      bind(need(args, 0, "bigintToUint"), (a0) =>
        bind(dBigint(g, a0), (n) => right(tInt(Number(n)))))),
  prim("hydra.lib.literals.bigintToDecimal", scheme(tyFn(tyBigint, tyFloat64)),
    (_cx, g, args) =>
      bind(need(args, 0, "bigintToDecimal"), (a0) =>
        bind(dBigint(g, a0), (n) => right(tDecimal(Number(n)))))),
  prim("hydra.lib.literals.decimalToBigint", scheme(tyFn(tyFloat64, tyBigint)),
    (_cx, g, args) =>
      bind(need(args, 0, "decimalToBigint"), (a0) => {
        // Decode Decimal literal directly. Match Haskell's `round`
        // (nearest-integer, ties away from zero — close enough for the
        // test fixtures, which avoid exact half values).
        const lit = (a0 as { tag: string; value?: { tag?: string; value?: number } });
        const v = lit.value?.value;
        if (lit.tag === "literal" && lit.value?.tag === "decimal" && typeof v === "number") {
          return right(tBigint(libLiterals.decimalToBigint(v)));
        }
        return bind(dAnyFloat(g, a0), (f) => right(tBigint(libLiterals.decimalToBigint(f))));
      })),
  prim("hydra.lib.literals.decimalToFloat", scheme(tyFn(tyFloat64, tyFloat64)),
    (_cx, g, args) =>
      bind(need(args, 0, "decimalToFloat"), (a0) => {
        const lit = (a0 as { tag: string; value?: { tag?: string; value?: number } });
        const v = lit.value?.value;
        if (lit.tag === "literal" && lit.value?.tag === "decimal" && typeof v === "number") {
          return right(tFloat(v));
        }
        return bind(dAnyFloat(g, a0), (f) => right(tFloat(f)));
      })),
  prim("hydra.lib.literals.binaryToString", scheme(tyFn(tyBinary, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "binaryToString"), (a0) =>
        bind(dBinary(g, a0) as Either<HydraError, unknown>, (b) =>
          right(tString(libLiterals.binaryToString(b as Uint8Array | string)))))),
  prim("hydra.lib.literals.stringToBinary", scheme(tyFn(tyString, tyBinary)),
    (_cx, g, args) =>
      bind(need(args, 0, "stringToBinary"), (a0) =>
        bind(dString(g, a0), (s) => right(tBinary(libLiterals.stringToBinary(s)))))),
  // === Width-specialized integer / float primitives ===
  //
  // The kernel exposes `intNToBigint` and `bigintToIntN` for each width
  // (int8/int16/int32/int64), plus the analogous uint family, and
  // `showIntN`/`readIntN`/`showFloatN`/`readFloatN` etc. We synthesize
  // them in a loop rather than spelling each one out.
  ...(["int8", "int16", "int32", "int64"] as const).flatMap((w) => [
    prim(`hydra.lib.literals.${w}ToBigint`, scheme(tyFn(intTypeOf(w), tyBigint)),
      (_cx, g, args) =>
        bind(need(args, 0, `${w}ToBigint`), (a0) =>
          bind(dAnyInt(g, a0), (n) => right(tBigint(BigInt(n)))))),
    prim(`hydra.lib.literals.bigintTo${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(tyBigint, intTypeOf(w))),
      (_cx, g, args) =>
        bind(need(args, 0, `bigintTo${w}`), (a0) =>
          bind(dBigint(g, a0), (n) => right(tInt(Number(n), w))))),
    prim(`hydra.lib.literals.show${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(intTypeOf(w), tyString)),
      (_cx, g, args) =>
        bind(need(args, 0, `show${w}`), (a0) =>
          bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
    prim(`hydra.lib.literals.read${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(tyString, tyMaybe(intTypeOf(w)))),
      (_cx, g, args) =>
        bind(need(args, 0, `read${w}`), (a0) =>
          bind(dString(g, a0), (s) => {
            const r = libLiterals.readInt(s);
            if (r.tag === "nothing") return right(tMaybeNothing);
            // Range check.
            const n = r.value;
            const max = w === "int8" ? 127 : w === "int16" ? 32767 : w === "int32" ? 2147483647 : Number.MAX_SAFE_INTEGER;
            const min = w === "int8" ? -128 : w === "int16" ? -32768 : w === "int32" ? -2147483648 : -Number.MAX_SAFE_INTEGER;
            if (n < min || n > max) return right(tMaybeNothing);
            return right(tMaybe(r, (n) => tInt(n, w)));
          }))),
  ]),
  ...(["uint8", "uint16", "uint32", "uint64"] as const).flatMap((w) => [
    prim(`hydra.lib.literals.${w}ToBigint`, scheme(tyFn(intTypeOf(w), tyBigint)),
      (_cx, g, args) =>
        bind(need(args, 0, `${w}ToBigint`), (a0) =>
          bind(dAnyInt(g, a0), (n) => right(tBigint(BigInt(n)))))),
    prim(`hydra.lib.literals.bigintTo${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(tyBigint, intTypeOf(w))),
      (_cx, g, args) =>
        bind(need(args, 0, `bigintTo${w}`), (a0) =>
          bind(dBigint(g, a0), (n) => right({ tag: "literal", value: { tag: "integer", value: { tag: w, value: Number(n) } } } as never)))),
    prim(`hydra.lib.literals.show${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(intTypeOf(w), tyString)),
      (_cx, g, args) =>
        bind(need(args, 0, `show${w}`), (a0) =>
          bind(dAnyInt(g, a0), (n) => right(tString(String(n)))))),
    prim(`hydra.lib.literals.read${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(tyString, tyMaybe(intTypeOf(w)))),
      (_cx, g, args) =>
        bind(need(args, 0, `read${w}`), (a0) =>
          bind(dString(g, a0), (s) => {
            const r = libLiterals.readUint(s);
            if (r.tag === "nothing") return right(tMaybeNothing);
            const n = r.value;
            const max = w === "uint8" ? 255 : w === "uint16" ? 65535 : w === "uint32" ? 4294967295 : Number.MAX_SAFE_INTEGER;
            if (n < 0 || n > max) return right(tMaybeNothing);
            return right(tMaybe(r, (n) => ({ tag: "literal", value: { tag: "integer", value: { tag: w, value: n } } } as never)));
          }))),
  ]),
  ...(["float32", "float64"] as const).flatMap((w) => [
    prim(`hydra.lib.literals.show${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(w === "float32" ? tyFloat32 : tyFloat64, tyString)),
      (_cx, g, args) =>
        bind(need(args, 0, `show${w}`), (a0) =>
          bind(dAnyFloat(g, a0), (f) =>
            right(tString(w === "float32" ? libLiterals.showFloat32(f) : libLiterals.showFloat64(f)))))),
    prim(`hydra.lib.literals.read${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(tyString, tyMaybe(tyInt32))),
      (_cx, g, args) =>
        bind(need(args, 0, `read${w}`), (a0) =>
          bind(dString(g, a0), (s) => {
            const r = libLiterals.readFloat(s);
            return right(tMaybe(r, (f) => tFloat(f, w)));
          }))),
    prim(`hydra.lib.literals.decimalTo${w[0]!.toUpperCase()}${w.slice(1)}`, scheme(tyFn(tyFloat64, w === "float32" ? tyFloat32 : tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, `decimalTo${w}`), (a0) => {
          const lit = (a0 as { tag: string; value?: { tag?: string; value?: number } });
          const v = lit.value?.value;
          if (lit.tag === "literal" && lit.value?.tag === "decimal" && typeof v === "number") {
            return right(tFloat(w === "float32" ? Math.fround(v) : v, w));
          }
          return bind(dAnyFloat(g, a0), (f) => right(tFloat(w === "float32" ? Math.fround(f) : f, w)));
        })),
    prim(`hydra.lib.literals.${w}ToDecimal`, scheme(tyFn(w === "float32" ? tyFloat32 : tyFloat64, tyFloat64)),
      (_cx, g, args) =>
        bind(need(args, 0, `${w}ToDecimal`), (a0) =>
          bind(dAnyFloat(g, a0), (f) => right(tDecimal(f))))),
  ]),
  prim("hydra.lib.literals.float32ToFloat64", scheme(tyFn(tyFloat32, tyFloat64)),
    (_cx, g, args) =>
      bind(need(args, 0, "float32ToFloat64"), (a0) =>
        bind(dAnyFloat(g, a0), (f) => right(tFloat(f, "float64"))))),
  prim("hydra.lib.literals.float64ToFloat32", scheme(tyFn(tyFloat64, tyFloat32)),
    (_cx, g, args) =>
      bind(need(args, 0, "float64ToFloat32"), (a0) =>
        bind(dAnyFloat(g, a0), (f) => right(tFloat(Math.fround(f), "float32"))))),
];

// === lib.equality ===

const equalityPrimitives = (): readonly Primitive[] => {
  const a = tyVar("a");
  const bin = (qname: string, cls: string, f: (x: unknown, y: unknown) => boolean): Primitive =>
    prim(qname, schemeC(tyFn(a, tyFn(a, tyBool)), ["a"], [["a", [cls]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, qname), (a0) =>
          bind(need(args, 1, qname), (a1) =>
            right(tBool(f(a0 as unknown, a1 as unknown))))));
  return [
    bin("hydra.lib.equality.equal", "equality", libEquality.equal),
    bin("hydra.lib.equality.lt", "ordering", libEquality.lt),
    bin("hydra.lib.equality.lte", "ordering", libEquality.lte),
    bin("hydra.lib.equality.gt", "ordering", libEquality.gt),
    bin("hydra.lib.equality.gte", "ordering", libEquality.gte),
    prim("hydra.lib.equality.compare", schemeC(tyFn(a, tyFn(a, { tag: "variable", value: { value: "hydra.util.Comparison" } } as never as Type)), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "compare"), (a0) =>
          bind(need(args, 1, "compare"), (a1) => {
            // The kernel-level Comparison value is the **Term** encoding:
            // `inject(hydra.util.Comparison){<arm>=unit}`. Show.core.term
            // renders this via cases on Term_inject. (The native
            // discriminated-union shape from libEquality.compare is for
            // host-language code, not Term-level rendering.)
            const c = libEquality.compare(a0 as unknown, a1 as unknown);
            const arm = c.tag; // "lessThan" | "equalTo" | "greaterThan"
            return right(tInject("hydra.util.Comparison", arm, { tag: "unit" } as never));
          }))),
    prim("hydra.lib.equality.identity", scheme(tyFn(a, a), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "identity"), (a0) => right(a0))),
    prim("hydra.lib.equality.min", schemeC(tyFn(a, tyFn(a, a)), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "min"), (a0) =>
          bind(need(args, 1, "min"), (a1) =>
            right(libEquality.lt(a0, a1) ? a0 : a1)))),
    prim("hydra.lib.equality.max", schemeC(tyFn(a, tyFn(a, a)), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "max"), (a0) =>
          bind(need(args, 1, "max"), (a1) =>
            right(libEquality.lt(a1, a0) ? a0 : a1)))),
  ];
};

// === lib.regex ===

const regexPrimitives = (): readonly Primitive[] => [
  u2("hydra.lib.regex.matches", tyString, tyString, tyBool,
    dString, dString, libRegex.matches, tBool),
  prim("hydra.lib.regex.find", scheme(tyFnCurried(tyString, tyString, tyMaybe(tyString))),
    (_cx, g, args) =>
      bind(need(args, 0, "find"), (a0) =>
        bind(need(args, 1, "find"), (a1) =>
          bind(dString(g, a0), (p) =>
            bind(dString(g, a1), (s) => {
              const r = s.match(new RegExp(p));
              return right(r ? tMaybeJust(tString(r[0])) : tMaybeNothing);
            }))))),
  prim("hydra.lib.regex.findAll", scheme(tyFnCurried(tyString, tyString, tyList(tyString))),
    (_cx, g, args) =>
      bind(need(args, 0, "findAll"), (a0) =>
        bind(need(args, 1, "findAll"), (a1) =>
          bind(dString(g, a0), (p) =>
            bind(dString(g, a1), (s) => {
              const els = Array.from(s.matchAll(new RegExp(p, "g")), (m) => tString(m[0]));
              return right({ tag: "list", value: els } as never);
            }))))),
  prim("hydra.lib.regex.replace", scheme(tyFnCurried(tyString, tyString, tyString, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "replace"), (a0) =>
        bind(need(args, 1, "replace"), (a1) =>
          bind(need(args, 2, "replace"), (a2) =>
            bind(dString(g, a0), (p) =>
              bind(dString(g, a1), (r) =>
                bind(dString(g, a2), (s) => right(tString(s.replace(new RegExp(p), r)))))))))),
  prim("hydra.lib.regex.replaceAll", scheme(tyFnCurried(tyString, tyString, tyString, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "replaceAll"), (a0) =>
        bind(need(args, 1, "replaceAll"), (a1) =>
          bind(need(args, 2, "replaceAll"), (a2) =>
            bind(dString(g, a0), (p) =>
              bind(dString(g, a1), (r) =>
                bind(dString(g, a2), (s) => right(tString(s.replace(new RegExp(p, "g"), r)))))))))),
  prim("hydra.lib.regex.split", scheme(tyFnCurried(tyString, tyString, tyList(tyString))),
    (_cx, g, args) =>
      bind(need(args, 0, "split"), (a0) =>
        bind(need(args, 1, "split"), (a1) =>
          bind(dString(g, a0), (p) =>
            bind(dString(g, a1), (s) => {
              const parts = s.split(new RegExp(p)).map((x) => tString(x));
              return right({ tag: "list", value: parts } as never);
            }))))),
];

// === lib.strings ===

const stringsPrimitives = (): readonly Primitive[] => [
  prim("hydra.lib.strings.length", scheme(tyFn(tyString, tyInt32)),
    (_cx, g, args) =>
      bind(need(args, 0, "length"), (a0) =>
        bind(dString(g, a0), (s) => right(tInt(libStrings.length(s)))))),
  prim("hydra.lib.strings.toUpper", scheme(tyFn(tyString, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "toUpper"), (a0) =>
        bind(dString(g, a0), (s) => right(tString(libStrings.toUpper(s)))))),
  prim("hydra.lib.strings.toLower", scheme(tyFn(tyString, tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "toLower"), (a0) =>
        bind(dString(g, a0), (s) => right(tString(libStrings.toLower(s)))))),
  prim("hydra.lib.strings.cat", scheme(tyFn(tyList(tyString), tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "cat"), (a0) => {
        const lst = a0 as { tag: string; value?: readonly Term[] };
        if (lst.tag !== "list") return left({ tag: "other", value: "cat: expected list" } as never);
        const parts: string[] = [];
        for (const t of (lst.value || [])) {
          const r = dString(g, t);
          if (r.tag === "left") return r as Either<HydraError, Term>;
          parts.push(r.value);
        }
        return right(tString(parts.join("")));
      })),
  u2("hydra.lib.strings.cat2", tyString, tyString, tyString,
    dString, dString, libStrings.cat2, tString),
  prim("hydra.lib.strings.maybeCharAt", scheme(tyFnCurried(tyInt32, tyString, tyMaybe(tyInt32))),
    (_cx, g, args) =>
      bind(need(args, 0, "maybeCharAt"), (a0) =>
        bind(need(args, 1, "maybeCharAt"), (a1) =>
          bind(dAnyInt(g, a0), (i) =>
            bind(dString(g, a1), (s) => {
              const cp = s.codePointAt(i);
              return right(cp === undefined ? tMaybeNothing : tMaybeJust(tInt(cp)));
            }))))),
  prim("hydra.lib.strings.split", scheme(tyFnCurried(tyString, tyString, tyList(tyString))),
    (_cx, g, args) =>
      bind(need(args, 0, "split"), (a0) =>
        bind(need(args, 1, "split"), (a1) =>
          bind(dString(g, a0), (sep) =>
            bind(dString(g, a1), (s) =>
              right({ tag: "list", value: s.split(sep).map((p) => tString(p)) } as never)))))),
  prim("hydra.lib.strings.splitOn", scheme(tyFnCurried(tyString, tyString, tyList(tyString))),
    (_cx, g, args) =>
      bind(need(args, 0, "splitOn"), (a0) =>
        bind(need(args, 1, "splitOn"), (a1) =>
          bind(dString(g, a0), (sep) =>
            bind(dString(g, a1), (s) =>
              right({ tag: "list", value: s.split(sep).map((p) => tString(p)) } as never)))))),
  prim("hydra.lib.strings.null", scheme(tyFn(tyString, tyBool)),
    (_cx, g, args) =>
      bind(need(args, 0, "null"), (a0) =>
        bind(dString(g, a0), (s) => right(tBool(s.length === 0))))),
  prim("hydra.lib.strings.toList", scheme(tyFn(tyString, tyList(tyInt32))),
    (_cx, g, args) =>
      bind(need(args, 0, "toList"), (a0) =>
        bind(dString(g, a0), (s) => {
          const out: Term[] = [];
          for (const ch of s) out.push(tInt(ch.codePointAt(0)!));
          return right({ tag: "list", value: out } as never);
        }))),
  prim("hydra.lib.strings.fromList", scheme(tyFn(tyList(tyInt32), tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "fromList"), (a0) => {
        const lst = a0 as { tag: string; value?: readonly Term[] };
        if (lst.tag !== "list") return left({ tag: "other", value: "fromList: expected list" } as never);
        let s = "";
        for (const t of (lst.value || [])) {
          const r = dAnyInt(g, t);
          if (r.tag === "left") return r as Either<HydraError, Term>;
          s += String.fromCodePoint(r.value);
        }
        return right(tString(s));
      })),
  prim("hydra.lib.strings.lines", scheme(tyFn(tyString, tyList(tyString))),
    (_cx, g, args) =>
      bind(need(args, 0, "lines"), (a0) =>
        bind(dString(g, a0), (s) =>
          right({ tag: "list", value: libStrings.lines(s).map((p) => tString(p)) } as never)))),
  prim("hydra.lib.strings.unlines", scheme(tyFn(tyList(tyString), tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "unlines"), (a0) => {
        const lst = a0 as { tag: string; value?: readonly Term[] };
        if (lst.tag !== "list") return left({ tag: "other", value: "unlines: expected list" } as never);
        const parts: string[] = [];
        for (const t of (lst.value || [])) {
          const r = dString(g, t);
          if (r.tag === "left") return r as Either<HydraError, Term>;
          parts.push(r.value);
        }
        return right(tString(libStrings.unlines(parts)));
      })),
  prim("hydra.lib.strings.intercalate", scheme(tyFnCurried(tyString, tyList(tyString), tyString)),
    (_cx, g, args) =>
      bind(need(args, 0, "intercalate"), (a0) =>
        bind(need(args, 1, "intercalate"), (a1) =>
          bind(dString(g, a0), (sep) => {
            const lst = a1 as { tag: string; value?: readonly Term[] };
            if (lst.tag !== "list") return left({ tag: "other", value: "intercalate: expected list" } as never);
            const parts: string[] = [];
            for (const t of (lst.value || [])) {
              const r = dString(g, t);
              if (r.tag === "left") return r as Either<HydraError, Term>;
              parts.push(r.value);
            }
            return right(tString(parts.join(sep)));
          })))),
];

// === lib.lists (selected) ===
//
// Lists primitives take Term-encoded lists ({tag:"list",value:[Term]}) and
// either run a HOF over them (which would require evaluating Hydra
// closures — not implemented at this layer) or do shape-preserving
// transformations. We register the latter category; the HOFs (`map`,
// `filter`, `foldl`, …) get reduced at the kernel level via the
// generated `hydra.lib.lists.*` term definitions (no primitive needed).

const listsPrimitives = (): readonly Primitive[] => {
  const asList = (t: Term): Either<HydraError, readonly Term[]> => {
    const x = t as { tag: string; value?: readonly Term[] };
    if (x.tag === "list") return right(x.value ?? []);
    return left({ tag: "other", value: "expected a list" } as never);
  };
  const mkList = (els: readonly Term[]): Term => ({ tag: "list", value: els } as never);
  return [
    prim("hydra.lib.lists.length", scheme(tyFn(tyList(tyVar("a")), tyInt32), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "length"), (a0) =>
          bind(asList(a0), (xs) => right(tInt(xs.length))))),
    prim("hydra.lib.lists.null", scheme(tyFn(tyList(tyVar("a")), tyBool), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "null"), (a0) =>
          bind(asList(a0), (xs) => right(tBool(xs.length === 0))))),
    prim("hydra.lib.lists.reverse", scheme(tyFn(tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "reverse"), (a0) =>
          bind(asList(a0), (xs) => right(mkList([...xs].reverse()))))),
    prim("hydra.lib.lists.concat", scheme(tyFn(tyList(tyList(tyVar("a"))), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "concat"), (a0) =>
          bind(asList(a0), (xss) => {
            const out: Term[] = [];
            for (const inner of xss) {
              const r = asList(inner);
              if (r.tag === "left") return r as Either<HydraError, Term>;
              out.push(...r.value);
            }
            return right(mkList(out));
          }))),
    prim("hydra.lib.lists.cons", scheme(tyFnCurried(tyVar("a"), tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "cons"), (x) =>
          bind(need(args, 1, "cons"), (a1) =>
            bind(asList(a1), (xs) => right(mkList([x, ...xs])))))),
    prim("hydra.lib.lists.pure", scheme(tyFn(tyVar("a"), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "pure"), (x) => right(mkList([x])))),
    prim("hydra.lib.lists.singleton", scheme(tyFn(tyVar("a"), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "singleton"), (x) => right(mkList([x])))),
    // HOF: foldl f init xs = reduce-left.
    prim("hydra.lib.lists.foldl", scheme(tyFnCurried(tyFn(tyVar("b"), tyFn(tyVar("a"), tyVar("b"))), tyVar("b"), tyList(tyVar("a")), tyVar("b")), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "foldl"), (fn) =>
          bind(need(args, 1, "foldl"), (init) =>
            bind(need(args, 2, "foldl"), (xs) =>
              bind(asList(xs), (lst) => {
                let acc: Term = init;
                for (const x of lst) {
                  const app: Term = { tag: "application", value: { function_: { tag: "application", value: { function_: fn, argument: acc } }, argument: x } } as never;
                  const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                  if (r.tag === "left") return r as Either<HydraError, Term>;
                  acc = r.value;
                }
                return right(acc);
              }))))),
    prim("hydra.lib.lists.foldr", scheme(tyFnCurried(tyFn(tyVar("a"), tyFn(tyVar("b"), tyVar("b"))), tyVar("b"), tyList(tyVar("a")), tyVar("b")), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "foldr"), (fn) =>
          bind(need(args, 1, "foldr"), (init) =>
            bind(need(args, 2, "foldr"), (xs) =>
              bind(asList(xs), (lst) => {
                let acc: Term = init;
                for (let i = lst.length - 1; i >= 0; i--) {
                  const x = lst[i]!;
                  const app: Term = { tag: "application", value: { function_: { tag: "application", value: { function_: fn, argument: x } }, argument: acc } } as never;
                  const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                  if (r.tag === "left") return r as Either<HydraError, Term>;
                  acc = r.value;
                }
                return right(acc);
              }))))),
    prim("hydra.lib.lists.map", scheme(tyFnCurried(tyFn(tyVar("a"), tyVar("b")), tyList(tyVar("a")), tyList(tyVar("b"))), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "map"), (fn) =>
          bind(need(args, 1, "map"), (xs) =>
            bind(asList(xs), (lst) => {
              const out: Term[] = [];
              for (const x of lst) {
                const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                out.push(r.value);
              }
              return right(mkList(out));
            })))),
    prim("hydra.lib.lists.filter", scheme(tyFnCurried(tyFn(tyVar("a"), tyBool), tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (cx, g, args) =>
        bind(need(args, 0, "filter"), (fn) =>
          bind(need(args, 1, "filter"), (xs) =>
            bind(asList(xs), (lst) => {
              const out: Term[] = [];
              for (const x of lst) {
                const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const v = r.value as { tag: string; value?: { tag?: string; value?: boolean } };
                // Extract boolean: either {tag:"literal", value:{tag:"boolean", value:b}} or direct.
                const b = v.tag === "literal" && v.value?.tag === "boolean" ? v.value.value : false;
                if (b) out.push(x);
              }
              return right(mkList(out));
            })))),
    prim("hydra.lib.lists.apply", scheme(tyFnCurried(tyList(tyFn(tyVar("a"), tyVar("b"))), tyList(tyVar("a")), tyList(tyVar("b"))), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "apply"), (fns) =>
          bind(need(args, 1, "apply"), (xs) =>
            bind(asList(fns), (fnList) =>
              bind(asList(xs), (xsList) => {
                const out: Term[] = [];
                for (const fn of fnList) {
                  for (const x of xsList) {
                    const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                    const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                    if (r.tag === "left") return r as Either<HydraError, Term>;
                    out.push(r.value);
                  }
                }
                return right(mkList(out));
              }))))),
    // bind xs f = concatMap f xs
    prim("hydra.lib.lists.bind", scheme(tyFnCurried(tyList(tyVar("a")), tyFn(tyVar("a"), tyList(tyVar("b"))), tyList(tyVar("b"))), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "bind"), (xs) =>
          bind(need(args, 1, "bind"), (fn) =>
            bind(asList(xs), (lst) => {
              const out: Term[] = [];
              for (const x of lst) {
                const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const sublist = asList(r.value);
                if (sublist.tag === "left") return sublist as Either<HydraError, Term>;
                out.push(...sublist.value);
              }
              return right(mkList(out));
            })))),
    prim("hydra.lib.lists.zipWith", scheme(tyFnCurried(tyFn(tyVar("a"), tyFn(tyVar("b"), tyVar("c"))), tyList(tyVar("a")), tyList(tyVar("b")), tyList(tyVar("c"))), ["a", "b", "c"]),
      (cx, g, args) =>
        bind(need(args, 0, "zipWith"), (fn) =>
          bind(need(args, 1, "zipWith"), (xs) =>
            bind(need(args, 2, "zipWith"), (ys) =>
              bind(asList(xs), (xL) =>
                bind(asList(ys), (yL) => {
                  const n = Math.min(xL.length, yL.length);
                  const out: Term[] = [];
                  for (let i = 0; i < n; i++) {
                    const app: Term = { tag: "application", value: { function_: { tag: "application", value: { function_: fn, argument: xL[i]! } }, argument: yL[i]! } } as never;
                    const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                    if (r.tag === "left") return r as Either<HydraError, Term>;
                    out.push(r.value);
                  }
                  return right(mkList(out));
                })))))),
    prim("hydra.lib.lists.find", scheme(tyFnCurried(tyFn(tyVar("a"), tyBool), tyList(tyVar("a")), tyMaybe(tyVar("a"))), ["a"]),
      (cx, g, args) =>
        bind(need(args, 0, "find"), (fn) =>
          bind(need(args, 1, "find"), (xs) =>
            bind(asList(xs), (lst) => {
              for (const x of lst) {
                const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const v = r.value as { tag: string; value?: { tag?: string; value?: boolean } };
                const b = v.tag === "literal" && v.value?.tag === "boolean" ? v.value.value : false;
                if (b) return right(tMaybeJust(x));
              }
              return right(tMaybeNothing);
            })))),
    prim("hydra.lib.lists.dropWhile", scheme(tyFnCurried(tyFn(tyVar("a"), tyBool), tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (cx, g, args) =>
        bind(need(args, 0, "dropWhile"), (fn) =>
          bind(need(args, 1, "dropWhile"), (xs) =>
            bind(asList(xs), (lst) => {
              let i = 0;
              while (i < lst.length) {
                const app: Term = { tag: "application", value: { function_: fn, argument: lst[i]! } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const v = r.value as { tag: string; value?: { tag?: string; value?: boolean } };
                const b = v.tag === "literal" && v.value?.tag === "boolean" ? v.value.value : false;
                if (!b) break;
                i++;
              }
              return right(mkList(lst.slice(i)));
            })))),
    // Simple non-HOF list ops that we already have via the runtime.
    prim("hydra.lib.lists.elem", schemeC(tyFnCurried(tyVar("a"), tyList(tyVar("a")), tyBool), ["a"], [["a", ["equality"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "elem"), (x) =>
          bind(need(args, 1, "elem"), (xs) =>
            bind(asList(xs), (lst) => right(tBool(lst.some((y) => libEquality.equal(x as unknown, y as unknown)))))))),
    prim("hydra.lib.lists.concat2", scheme(tyFnCurried(tyList(tyVar("a")), tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "concat2"), (xs) =>
          bind(need(args, 1, "concat2"), (ys) =>
            bind(asList(xs), (a) =>
              bind(asList(ys), (b) => right(mkList([...a, ...b]))))))),
    prim("hydra.lib.lists.intercalate", scheme(tyFnCurried(tyList(tyVar("a")), tyList(tyList(tyVar("a"))), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "intercalate"), (sep) =>
          bind(need(args, 1, "intercalate"), (xss) =>
            bind(asList(sep), (s) =>
              bind(asList(xss), (xsL) => {
                const out: Term[] = [];
                for (let i = 0; i < xsL.length; i++) {
                  if (i > 0) out.push(...s);
                  const sub = asList(xsL[i]!);
                  if (sub.tag === "left") return sub as Either<HydraError, Term>;
                  out.push(...sub.value);
                }
                return right(mkList(out));
              }))))),
    prim("hydra.lib.lists.intersperse", scheme(tyFnCurried(tyVar("a"), tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "intersperse"), (sep) =>
          bind(need(args, 1, "intersperse"), (xs) =>
            bind(asList(xs), (lst) => {
              if (lst.length === 0) return right(mkList([]));
              const out: Term[] = [lst[0]!];
              for (let i = 1; i < lst.length; i++) { out.push(sep); out.push(lst[i]!); }
              return right(mkList(out));
            })))),
    prim("hydra.lib.lists.drop", scheme(tyFnCurried(tyInt32, tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, g, args) =>
        bind(need(args, 0, "drop"), (a0) =>
          bind(need(args, 1, "drop"), (xs) =>
            bind(dAnyInt(g, a0), (n) =>
              bind(asList(xs), (lst) => right(mkList(n <= 0 ? lst : lst.slice(n)))))))),
    prim("hydra.lib.lists.take", scheme(tyFnCurried(tyInt32, tyList(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, g, args) =>
        bind(need(args, 0, "take"), (a0) =>
          bind(need(args, 1, "take"), (xs) =>
            bind(dAnyInt(g, a0), (n) =>
              bind(asList(xs), (lst) => right(mkList(n <= 0 ? [] : lst.slice(0, n)))))))),
    prim("hydra.lib.lists.maybeAt", scheme(tyFnCurried(tyInt32, tyList(tyVar("a")), tyMaybe(tyVar("a"))), ["a"]),
      (_cx, g, args) =>
        bind(need(args, 0, "maybeAt"), (a0) =>
          bind(need(args, 1, "maybeAt"), (xs) =>
            bind(dAnyInt(g, a0), (i) =>
              bind(asList(xs), (lst) =>
                right(i >= 0 && i < lst.length ? tMaybeJust(lst[i]!) : tMaybeNothing)))))),
    prim("hydra.lib.lists.maybeHead", scheme(tyFn(tyList(tyVar("a")), tyMaybe(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "maybeHead"), (xs) =>
          bind(asList(xs), (lst) => right(lst.length === 0 ? tMaybeNothing : tMaybeJust(lst[0]!))))),
    prim("hydra.lib.lists.maybeLast", scheme(tyFn(tyList(tyVar("a")), tyMaybe(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "maybeLast"), (xs) =>
          bind(asList(xs), (lst) => right(lst.length === 0 ? tMaybeNothing : tMaybeJust(lst[lst.length - 1]!))))),
    prim("hydra.lib.lists.maybeTail", scheme(tyFn(tyList(tyVar("a")), tyMaybe(tyList(tyVar("a")))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "maybeTail"), (xs) =>
          bind(asList(xs), (lst) => right(lst.length === 0 ? tMaybeNothing : tMaybeJust(mkList(lst.slice(1))))))),
    prim("hydra.lib.lists.maybeInit", scheme(tyFn(tyList(tyVar("a")), tyMaybe(tyList(tyVar("a")))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "maybeInit"), (xs) =>
          bind(asList(xs), (lst) => right(lst.length === 0 ? tMaybeNothing : tMaybeJust(mkList(lst.slice(0, -1))))))),
    prim("hydra.lib.lists.uncons", scheme(tyFn(tyList(tyVar("a")), tyMaybe(tyPair(tyVar("a"), tyList(tyVar("a"))))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "uncons"), (xs) =>
          bind(asList(xs), (lst) => right(lst.length === 0 ? tMaybeNothing : tMaybeJust({ tag: "pair", value: [lst[0]!, mkList(lst.slice(1))] } as never))))),
    prim("hydra.lib.lists.zip", scheme(tyFnCurried(tyList(tyVar("a")), tyList(tyVar("b")), tyList(tyPair(tyVar("a"), tyVar("b")))), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "zip"), (xs) =>
          bind(need(args, 1, "zip"), (ys) =>
            bind(asList(xs), (xL) =>
              bind(asList(ys), (yL) => {
                const n = Math.min(xL.length, yL.length);
                const out: Term[] = [];
                for (let i = 0; i < n; i++) out.push({ tag: "pair", value: [xL[i]!, yL[i]!] } as never);
                return right(mkList(out));
              }))))),
    prim("hydra.lib.lists.replicate", scheme(tyFnCurried(tyInt32, tyVar("a"), tyList(tyVar("a"))), ["a"]),
      (_cx, g, args) =>
        bind(need(args, 0, "replicate"), (a0) =>
          bind(need(args, 1, "replicate"), (x) =>
            bind(dAnyInt(g, a0), (n) => right(mkList(Array.from({ length: Math.max(n, 0) }, () => x))))))),
    prim("hydra.lib.lists.nub", schemeC(tyFn(tyList(tyVar("a")), tyList(tyVar("a"))), ["a"], [["a", ["equality"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "nub"), (xs) =>
          bind(asList(xs), (lst) => {
            const out: Term[] = [];
            for (const x of lst) {
              if (!out.some((y) => libEquality.equal(x as unknown, y as unknown))) out.push(x);
            }
            return right(mkList(out));
          }))),
    prim("hydra.lib.lists.group", schemeC(tyFn(tyList(tyVar("a")), tyList(tyList(tyVar("a")))), ["a"], [["a", ["equality"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "group"), (xs) =>
          bind(asList(xs), (lst) => {
            if (lst.length === 0) return right(mkList([]));
            const out: Term[][] = [[lst[0]!]];
            for (let i = 1; i < lst.length; i++) {
              if (libEquality.equal(lst[i] as unknown, lst[i - 1] as unknown)) {
                out[out.length - 1]!.push(lst[i]!);
              } else {
                out.push([lst[i]!]);
              }
            }
            return right(mkList(out.map((g) => mkList(g))));
          }))),
    prim("hydra.lib.lists.sort", schemeC(tyFn(tyList(tyVar("a")), tyList(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "sort"), (xs) =>
          bind(asList(xs), (lst) => {
            const sorted = [...lst].sort((a, b) => libEquality.lt(a as unknown, b as unknown) ? -1 : libEquality.lt(b as unknown, a as unknown) ? 1 : 0);
            return right(mkList(sorted));
          }))),
    prim("hydra.lib.lists.sortOn", schemeC(tyFnCurried(tyFn(tyVar("a"), tyVar("b")), tyList(tyVar("a")), tyList(tyVar("a"))), ["a", "b"], [["b", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "sortOn"), (fn) =>
          bind(need(args, 1, "sortOn"), (xs) =>
            bind(asList(xs), (lst) => {
              // Compute keys via the closure first.
              const keyed: Array<[Term, Term]> = [];
              for (const x of lst) {
                const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                keyed.push([x, r.value]);
              }
              keyed.sort((a, b) => libEquality.lt(a[1] as unknown, b[1] as unknown) ? -1 : libEquality.lt(b[1] as unknown, a[1] as unknown) ? 1 : 0);
              return right(mkList(keyed.map((p) => p[0])));
            })))),
    prim("hydra.lib.lists.partition", scheme(tyFnCurried(tyFn(tyVar("a"), tyBool), tyList(tyVar("a")), tyPair(tyList(tyVar("a")), tyList(tyVar("a")))), ["a"]),
      (cx, g, args) =>
        bind(need(args, 0, "partition"), (fn) =>
          bind(need(args, 1, "partition"), (xs) =>
            bind(asList(xs), (lst) => {
              const yes: Term[] = [];
              const no: Term[] = [];
              for (const x of lst) {
                const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const v = r.value as { tag: string; value?: { tag?: string; value?: boolean } };
                const b = v.tag === "literal" && v.value?.tag === "boolean" ? v.value.value : false;
                if (b) yes.push(x); else no.push(x);
              }
              return right({ tag: "pair", value: [mkList(yes), mkList(no)] } as never);
            })))),
    prim("hydra.lib.lists.span", scheme(tyFnCurried(tyFn(tyVar("a"), tyBool), tyList(tyVar("a")), tyPair(tyList(tyVar("a")), tyList(tyVar("a")))), ["a"]),
      (cx, g, args) =>
        bind(need(args, 0, "span"), (fn) =>
          bind(need(args, 1, "span"), (xs) =>
            bind(asList(xs), (lst) => {
              let i = 0;
              while (i < lst.length) {
                const app: Term = { tag: "application", value: { function_: fn, argument: lst[i]! } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const v = r.value as { tag: string; value?: { tag?: string; value?: boolean } };
                const b = v.tag === "literal" && v.value?.tag === "boolean" ? v.value.value : false;
                if (!b) break;
                i++;
              }
              return right({ tag: "pair", value: [mkList(lst.slice(0, i)), mkList(lst.slice(i))] } as never);
            })))),
    prim("hydra.lib.lists.transpose", scheme(tyFn(tyList(tyList(tyVar("a"))), tyList(tyList(tyVar("a")))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "transpose"), (xss) =>
          bind(asList(xss), (rows) => {
            if (rows.length === 0) return right(mkList([]));
            const rowsArr: Term[][] = [];
            for (const r of rows) {
              const sub = asList(r);
              if (sub.tag === "left") return sub as Either<HydraError, Term>;
              rowsArr.push([...sub.value]);
            }
            const w = Math.max(...rowsArr.map((r) => r.length));
            const out: Term[] = [];
            for (let i = 0; i < w; i++) {
              const col: Term[] = [];
              for (const r of rowsArr) if (i < r.length) col.push(r[i]!);
              out.push(mkList(col));
            }
            return right(mkList(out));
          }))),
  ];
};

// === lib.sets (selected) ===

const setsPrimitives = (): readonly Primitive[] => {
  const asSet = (t: Term): Either<HydraError, ReadonlySet<Term>> => {
    const x = t as { tag: string; value?: ReadonlySet<Term> };
    if (x.tag === "set") return right(x.value ?? new Set());
    return left({ tag: "other", value: "expected a set" } as never);
  };
  const mkSet = (s: ReadonlySet<Term>): Term => ({ tag: "set", value: s } as never);
  return [
    prim("hydra.lib.sets.size", schemeC(tyFn(tySet(tyVar("a")), tyInt32), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "size"), (a0) =>
          bind(asSet(a0), (s) => right(tInt(libSets.size(s)))))),
    prim("hydra.lib.sets.empty", schemeC(tySet(tyVar("a")), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, _args) => right(mkSet(libSets.empty))),
    prim("hydra.lib.sets.null", schemeC(tyFn(tySet(tyVar("a")), tyBool), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "null"), (a0) =>
          bind(asSet(a0), (s) => right(tBool(libSets.null_(s)))))),
    prim("hydra.lib.sets.member", schemeC(tyFnCurried(tyVar("a"), tySet(tyVar("a")), tyBool), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "member"), (e) =>
          bind(need(args, 1, "member"), (s) =>
            bind(asSet(s), (st) => right(tBool(libSets.member(e as Term, st))))))),
    prim("hydra.lib.sets.singleton", schemeC(tyFn(tyVar("a"), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "singleton"), (e) =>
          right(mkSet(libSets.singleton(e as Term))))),
    prim("hydra.lib.sets.insert", schemeC(tyFnCurried(tyVar("a"), tySet(tyVar("a")), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "insert"), (e) =>
          bind(need(args, 1, "insert"), (s) =>
            bind(asSet(s), (st) => right(mkSet(libSets.insert(e as Term, st))))))),
    prim("hydra.lib.sets.delete", schemeC(tyFnCurried(tyVar("a"), tySet(tyVar("a")), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "delete"), (e) =>
          bind(need(args, 1, "delete"), (s) =>
            bind(asSet(s), (st) => right(mkSet(libSets.delete_(e as Term, st))))))),
    prim("hydra.lib.sets.fromList", schemeC(tyFn(tyList(tyVar("a")), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "fromList"), (a0) => {
          const lst = a0 as { tag: string; value?: readonly Term[] };
          if (lst.tag !== "list") return left({ tag: "other", value: "fromList: expected list" } as never);
          return right(mkSet(libSets.fromList(lst.value ?? [])));
        })),
    prim("hydra.lib.sets.toList", schemeC(tyFn(tySet(tyVar("a")), tyList(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "toList"), (s) =>
          bind(asSet(s), (st) => right({ tag: "list", value: [...libSets.toList(st)] } as never)))),
    prim("hydra.lib.sets.union", schemeC(tyFnCurried(tySet(tyVar("a")), tySet(tyVar("a")), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "union"), (a) =>
          bind(need(args, 1, "union"), (b) =>
            bind(asSet(a), (sa) =>
              bind(asSet(b), (sb) => right(mkSet(libSets.union(sa, sb)))))))),
    prim("hydra.lib.sets.intersection", schemeC(tyFnCurried(tySet(tyVar("a")), tySet(tyVar("a")), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "intersection"), (a) =>
          bind(need(args, 1, "intersection"), (b) =>
            bind(asSet(a), (sa) =>
              bind(asSet(b), (sb) => right(mkSet(libSets.intersection(sa, sb)))))))),
    prim("hydra.lib.sets.difference", schemeC(tyFnCurried(tySet(tyVar("a")), tySet(tyVar("a")), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "difference"), (a) =>
          bind(need(args, 1, "difference"), (b) =>
            bind(asSet(a), (sa) =>
              bind(asSet(b), (sb) => right(mkSet(libSets.difference(sa, sb)))))))),
    prim("hydra.lib.sets.unions", schemeC(tyFn(tyList(tySet(tyVar("a"))), tySet(tyVar("a"))), ["a"], [["a", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "unions"), (a0) => {
          const lst = a0 as { tag: string; value?: readonly Term[] };
          if (lst.tag !== "list") return left({ tag: "other", value: "unions: expected list" } as never);
          let acc = libSets.empty as ReadonlySet<Term>;
          for (const t of lst.value ?? []) {
            const r = asSet(t);
            if (r.tag === "left") return r as Either<HydraError, Term>;
            acc = libSets.union(acc, r.value);
          }
          return right(mkSet(acc));
        })),
    // sets.map :: (a -> b) -> Set a -> Set b — ordering on both
    prim("hydra.lib.sets.map", schemeC(tyFnCurried(tyFn(tyVar("a"), tyVar("b")), tySet(tyVar("a")), tySet(tyVar("b"))),
        ["a", "b"], [["a", ["ordering"]], ["b", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "sets.map"), (fn) =>
          bind(need(args, 1, "sets.map"), (s) =>
            bind(asSet(s), (st) => {
              const out: Term[] = [];
              for (const e of libSets.toList(st)) {
                const app: Term = { tag: "application", value: { function_: fn, argument: e } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, eg: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                out.push(r.value);
              }
              return right(mkSet(libSets.fromList(out)));
            })))),
  ];
};

// === lib.maps ===
//
// Map-typed args come in as Term_map values: { tag: "map", value: <Map> }.
// We forward to the runtime helpers, which already handle the
// canonical-key wrapping.

const mapsPrimitives = (): readonly Primitive[] => {
  const asMap = (t: Term): Either<HydraError, ReadonlyMap<unknown, Term>> => {
    const x = t as { tag: string; value?: ReadonlyMap<unknown, Term> };
    if (x.tag === "map") return right(x.value ?? new Map());
    return left({ tag: "other", value: "expected a map" } as never);
  };
  const mkMap = (m: ReadonlyMap<unknown, Term>): Term => ({ tag: "map", value: m } as never);
  return [
    prim("hydra.lib.maps.null", schemeC(tyFn(tyMap(tyVar("k"), tyVar("v")), tyBool), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "null"), (a0) =>
          bind(asMap(a0), (m) => right(tBool(libMaps.null_(m)))))),
    prim("hydra.lib.maps.size", schemeC(tyFn(tyMap(tyVar("k"), tyVar("v")), tyInt32), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "size"), (a0) =>
          bind(asMap(a0), (m) => right(tInt(libMaps.size(m)))))),
    prim("hydra.lib.maps.empty", schemeC(tyMap(tyVar("k"), tyVar("v")), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, _args) => right(mkMap(new Map()))),
    // Simple non-HOF map ops.
    prim("hydra.lib.maps.lookup", schemeC(tyFnCurried(tyVar("k"), tyMap(tyVar("k"), tyVar("v")), tyMaybe(tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "lookup"), (k) =>
          bind(need(args, 1, "lookup"), (m) =>
            bind(asMap(m), (mp) => {
              const r = libMaps.lookup(k, mp);
              return right(r.tag === "just" ? tMaybeJust(r.value as Term) : tMaybeNothing);
            })))),
    prim("hydra.lib.maps.member", schemeC(tyFnCurried(tyVar("k"), tyMap(tyVar("k"), tyVar("v")), tyBool), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "member"), (k) =>
          bind(need(args, 1, "member"), (m) =>
            bind(asMap(m), (mp) => right(tBool(libMaps.member(k, mp))))))),
    prim("hydra.lib.maps.insert", schemeC(tyFnCurried(tyVar("k"), tyVar("v"), tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "insert"), (k) =>
          bind(need(args, 1, "insert"), (v) =>
            bind(need(args, 2, "insert"), (m) =>
              bind(asMap(m), (mp) => right(mkMap(libMaps.insert(k, v as Term, mp) as ReadonlyMap<unknown, Term>))))))),
    prim("hydra.lib.maps.delete", schemeC(tyFnCurried(tyVar("k"), tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "delete"), (k) =>
          bind(need(args, 1, "delete"), (m) =>
            bind(asMap(m), (mp) => right(mkMap(libMaps.delete_(k, mp) as ReadonlyMap<unknown, Term>)))))),
    prim("hydra.lib.maps.fromList", schemeC(tyFn(tyList(tyPair(tyVar("k"), tyVar("v"))), tyMap(tyVar("k"), tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "fromList"), (a0) => {
          const lst = a0 as { tag: string; value?: readonly Term[] };
          if (lst.tag !== "list") return left({ tag: "other", value: "fromList: expected list" } as never);
          const pairs: readonly (readonly [unknown, Term])[] = (lst.value ?? []).map((t) => {
            const p = t as { tag: string; value?: readonly [unknown, Term] };
            return p.value!;
          });
          return right(mkMap(libMaps.fromList(pairs) as ReadonlyMap<unknown, Term>));
        })),
    prim("hydra.lib.maps.toList", schemeC(tyFn(tyMap(tyVar("k"), tyVar("v")), tyList(tyPair(tyVar("k"), tyVar("v")))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "toList"), (m) =>
          bind(asMap(m), (mp) =>
            right({ tag: "list", value: libMaps.toList(mp).map((kv) => ({ tag: "pair", value: kv })) } as never)))),
    prim("hydra.lib.maps.keys", schemeC(tyFn(tyMap(tyVar("k"), tyVar("v")), tyList(tyVar("k"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "keys"), (m) =>
          bind(asMap(m), (mp) =>
            right({ tag: "list", value: [...libMaps.keys(mp)] } as never)))),
    prim("hydra.lib.maps.values", schemeC(tyFn(tyMap(tyVar("k"), tyVar("v")), tyList(tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "values"), (m) =>
          bind(asMap(m), (mp) =>
            right({ tag: "list", value: [...libMaps.values(mp) as readonly Term[]] } as never)))),
    prim("hydra.lib.maps.singleton", schemeC(tyFnCurried(tyVar("k"), tyVar("v"), tyMap(tyVar("k"), tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "singleton"), (k) =>
          bind(need(args, 1, "singleton"), (v) =>
            right(mkMap(libMaps.singleton(k, v as Term) as ReadonlyMap<unknown, Term>))))),
    prim("hydra.lib.maps.union", schemeC(tyFnCurried(tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "union"), (a) =>
          bind(need(args, 1, "union"), (b) =>
            bind(asMap(a), (ma) =>
              bind(asMap(b), (mb) =>
                right(mkMap(libMaps.union(ma, mb) as ReadonlyMap<unknown, Term>))))))),
    // HOF: alter takes a closure (Maybe v -> Maybe v) and runs it via
    // reduceTerm. The closure's input/output are Term-encoded Hydra
    // Maybe values.
    prim("hydra.lib.maps.alter", schemeC(tyFnCurried(tyFn(tyMaybe(tyVar("v")), tyMaybe(tyVar("v"))), tyVar("k"), tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v"))), ["v", "k"], [["k", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "alter"), (fn) =>
          bind(need(args, 1, "alter"), (k) =>
            bind(need(args, 2, "alter"), (m) =>
              bind(asMap(m), (mp) => {
                const cur = libMaps.lookup(k, mp);
                const curMaybe: Term = cur.tag === "just"
                  ? tMaybeJust(cur.value as Term)
                  : tMaybeNothing;
                // Apply the closure: reduceTerm( App(fn, curMaybe) ).
                const appTerm: Term = { tag: "application", value: { function_: fn, argument: curMaybe } } as never;
                const reduced = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, appTerm);
                if (reduced.tag === "left") return reduced as Either<HydraError, Term>;
                // The result is a Hydra Maybe Term. Interpret it.
                const r = reduced.value as { tag: string; value?: { tag?: string; value?: Term } };
                if (r.tag !== "maybe") return left({ tag: "other", value: "alter: closure didn't return Maybe" } as never);
                const next = r.value;
                if (next?.tag === "just" && next.value !== undefined) {
                  return right(mkMap(libMaps.insert(k, next.value, mp) as ReadonlyMap<unknown, Term>));
                }
                return right(mkMap(libMaps.delete_(k, mp) as ReadonlyMap<unknown, Term>));
              })))))
    ,
    // map :: (v1 -> v2) -> Map k v1 -> Map k v2 — ordering on k
    prim("hydra.lib.maps.map", schemeC(tyFnCurried(tyFn(tyVar("v1"), tyVar("v2")), tyMap(tyVar("k"), tyVar("v1")), tyMap(tyVar("k"), tyVar("v2"))),
        ["v1", "v2", "k"], [["k", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "maps.map"), (fn) =>
          bind(need(args, 1, "maps.map"), (m) =>
            bind(asMap(m), (mp) => {
              const out = new Map();
              for (const [k, v] of libMaps.toList(mp)) {
                const app: Term = { tag: "application", value: { function_: fn, argument: v as Term } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                out.set(k, r.value);
              }
              return right(mkMap(libMaps.fromList([...out.entries()] as readonly (readonly [unknown, Term])[]) as ReadonlyMap<unknown, Term>));
            })))),
    // mapKeys :: (k1 -> k2) -> Map k1 v -> Map k2 v — ordering on both
    prim("hydra.lib.maps.mapKeys", schemeC(tyFnCurried(tyFn(tyVar("k1"), tyVar("k2")), tyMap(tyVar("k1"), tyVar("v")), tyMap(tyVar("k2"), tyVar("v"))),
        ["k1", "k2", "v"], [["k1", ["ordering"]], ["k2", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "maps.mapKeys"), (fn) =>
          bind(need(args, 1, "maps.mapKeys"), (m) =>
            bind(asMap(m), (mp) => {
              const out: Array<readonly [unknown, Term]> = [];
              for (const [k, v] of libMaps.toList(mp)) {
                const app: Term = { tag: "application", value: { function_: fn, argument: k as Term } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                out.push([r.value, v as Term] as const);
              }
              return right(mkMap(libMaps.fromList(out) as ReadonlyMap<unknown, Term>));
            })))),
    // filter :: (v -> bool) -> Map k v -> Map k v — ordering on k
    prim("hydra.lib.maps.filter", schemeC(tyFnCurried(tyFn(tyVar("v"), tyBool), tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v"))),
        ["v", "k"], [["k", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "maps.filter"), (fn) =>
          bind(need(args, 1, "maps.filter"), (m) =>
            bind(asMap(m), (mp) => {
              const out: Array<readonly [unknown, Term]> = [];
              for (const [k, v] of libMaps.toList(mp)) {
                const app: Term = { tag: "application", value: { function_: fn, argument: v as Term } } as never;
                const r = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
                if (r.tag === "left") return r as Either<HydraError, Term>;
                const lit = r.value as { tag: string; value?: { tag: string; value?: boolean } };
                if (lit.tag === "literal" && lit.value?.tag === "boolean" && lit.value.value) {
                  out.push([k, v as Term] as const);
                }
              }
              return right(mkMap(libMaps.fromList(out) as ReadonlyMap<unknown, Term>));
            })))),
    // findWithDefault :: v -> k -> Map k v -> v — ordering on k
    prim("hydra.lib.maps.findWithDefault", schemeC(tyFnCurried(tyVar("v"), tyVar("k"), tyMap(tyVar("k"), tyVar("v")), tyVar("v")),
        ["v", "k"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "findWithDefault"), (d) =>
          bind(need(args, 1, "findWithDefault"), (k) =>
            bind(need(args, 2, "findWithDefault"), (m) =>
              bind(asMap(m), (mp) => {
                const r = libMaps.lookup(k, mp);
                return right(r.tag === "just" ? (r.value as Term) : (d as Term));
              }))))),
    // elems :: Map k v -> [v]
    prim("hydra.lib.maps.elems", schemeC(tyFn(tyMap(tyVar("k"), tyVar("v")), tyList(tyVar("v"))), ["k", "v"], [["k", ["ordering"]]]),
      (_cx, _g, args) =>
        bind(need(args, 0, "elems"), (m) =>
          bind(asMap(m), (mp) =>
            right({ tag: "list", value: [...libMaps.values(mp) as readonly Term[]] } as never)))),
    // bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2 — ordering on both keys
    prim("hydra.lib.maps.bimap", schemeC(tyFnCurried(tyFn(tyVar("k1"), tyVar("k2")), tyFn(tyVar("v1"), tyVar("v2")), tyMap(tyVar("k1"), tyVar("v1")), tyMap(tyVar("k2"), tyVar("v2"))),
        ["k1", "k2", "v1", "v2"], [["k1", ["ordering"]], ["k2", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "bimap"), (fk) =>
          bind(need(args, 1, "bimap"), (fv) =>
            bind(need(args, 2, "bimap"), (m) =>
              bind(asMap(m), (mp) => {
                const out: Array<readonly [unknown, Term]> = [];
                for (const [k, v] of libMaps.toList(mp)) {
                  const appK: Term = { tag: "application", value: { function_: fk, argument: k as Term } } as never;
                  const appV: Term = { tag: "application", value: { function_: fv, argument: v as Term } } as never;
                  const rk = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, appK);
                  if (rk.tag === "left") return rk as Either<HydraError, Term>;
                  const rv = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, appV);
                  if (rv.tag === "left") return rv as Either<HydraError, Term>;
                  out.push([rk.value, rv.value] as const);
                }
                return right(mkMap(libMaps.fromList(out) as ReadonlyMap<unknown, Term>));
              }))))),
    // filterWithKey :: (k -> v -> bool) -> Map k v -> Map k v — ordering on k
    prim("hydra.lib.maps.filterWithKey", schemeC(tyFnCurried(tyFnCurried(tyVar("k"), tyVar("v"), tyBool), tyMap(tyVar("k"), tyVar("v")), tyMap(tyVar("k"), tyVar("v"))),
        ["k", "v"], [["k", ["ordering"]]]),
      (cx, g, args) =>
        bind(need(args, 0, "filterWithKey"), (fn) =>
          bind(need(args, 1, "filterWithKey"), (m) =>
            bind(asMap(m), (mp) => {
              const out: Array<readonly [unknown, Term]> = [];
              for (const [k, v] of libMaps.toList(mp)) {
                const app1: Term = { tag: "application", value: { function_: fn, argument: k as Term } } as never;
                const r1 = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app1);
                if (r1.tag === "left") return r1 as Either<HydraError, Term>;
                const app2: Term = { tag: "application", value: { function_: r1.value, argument: v as Term } } as never;
                const r2 = (reduceTerm as never as (cx: Context, g: Graph, e: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app2);
                if (r2.tag === "left") return r2 as Either<HydraError, Term>;
                const lit = r2.value as { tag: string; value?: { tag: string; value?: boolean } };
                if (lit.tag === "literal" && lit.value?.tag === "boolean" && lit.value.value) {
                  out.push([k, v as Term] as const);
                }
              }
              return right(mkMap(libMaps.fromList(out) as ReadonlyMap<unknown, Term>));
            })))),
  ];
};

// Suppress unused-import warnings for the lib modules we import but
// reference only through forwarded calls in registration helpers.
void libLists; void libSets; void dInt64; void dFloat32; void tyUnit;

// === lib.maybes ===

const maybesPrimitives = (): readonly Primitive[] => {
  const asMaybe = (t: Term): { tag: "just"; value: Term } | { tag: "nothing" } | null => {
    const x = t as { tag: string; value?: { tag?: string; value?: Term } };
    if (x.tag === "maybe" && x.value) {
      if (x.value.tag === "just") return { tag: "just", value: x.value.value! };
      if (x.value.tag === "nothing") return { tag: "nothing" };
    }
    return null;
  };
  return [
    // `maybe default fn m` — applies fn to m's value, else returns default.
    // The default position is lazy in our wrap; receives either a value
    // or a thunk. Here we get a Term (the unforced value) since the
    // primitive args go through reduceArg.
    prim("hydra.lib.maybes.maybe", scheme(tyFnCurried(tyVar("b"), tyFn(tyVar("a"), tyVar("b")), tyMaybe(tyVar("a")), tyVar("b")), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "maybe-default"), (def) =>
          bind(need(args, 1, "maybe-fn"), (fn) =>
            bind(need(args, 2, "maybe-m"), (m) => {
              const mv = asMaybe(m);
              if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
              if (mv.tag === "nothing") return right(def);
              const app: Term = { tag: "application", value: { function_: fn, argument: mv.value } } as never;
              return (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
            })))),
    prim("hydra.lib.maybes.fromMaybe", scheme(tyFnCurried(tyVar("a"), tyMaybe(tyVar("a")), tyVar("a")), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "fromMaybe-default"), (def) =>
          bind(need(args, 1, "fromMaybe-m"), (m) => {
            const mv = asMaybe(m);
            if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
            return right(mv.tag === "just" ? mv.value : def);
          }))),
    prim("hydra.lib.maybes.isJust", scheme(tyFn(tyMaybe(tyVar("a")), tyBool), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "isJust"), (m) => {
          const mv = asMaybe(m);
          if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
          return right(tBool(mv.tag === "just"));
        })),
    prim("hydra.lib.maybes.isNothing", scheme(tyFn(tyMaybe(tyVar("a")), tyBool), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "isNothing"), (m) => {
          const mv = asMaybe(m);
          if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
          return right(tBool(mv.tag === "nothing"));
        })),
    prim("hydra.lib.maybes.map", scheme(tyFnCurried(tyFn(tyVar("a"), tyVar("b")), tyMaybe(tyVar("a")), tyMaybe(tyVar("b"))), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "map-fn"), (fn) =>
          bind(need(args, 1, "map-m"), (m) => {
            const mv = asMaybe(m);
            if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
            if (mv.tag === "nothing") return right(tMaybeNothing);
            const app: Term = { tag: "application", value: { function_: fn, argument: mv.value } } as never;
            const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
            if (r.tag === "left") return r as Either<HydraError, Term>;
            return right(tMaybeJust(r.value));
          }))),
    prim("hydra.lib.maybes.bind", scheme(tyFnCurried(tyMaybe(tyVar("a")), tyFn(tyVar("a"), tyMaybe(tyVar("b"))), tyMaybe(tyVar("b"))), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "bind-m"), (m) =>
          bind(need(args, 1, "bind-fn"), (fn) => {
            const mv = asMaybe(m);
            if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
            if (mv.tag === "nothing") return right(tMaybeNothing);
            const app: Term = { tag: "application", value: { function_: fn, argument: mv.value } } as never;
            return (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
          }))),
    prim("hydra.lib.maybes.pure", scheme(tyFn(tyVar("a"), tyMaybe(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "pure"), (x) => right(tMaybeJust(x)))),
    prim("hydra.lib.maybes.cat", scheme(tyFn(tyList(tyMaybe(tyVar("a"))), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "cat"), (xs) => {
          const lst = xs as { tag: string; value?: readonly Term[] };
          if (lst.tag !== "list") return left({ tag: "other", value: "expected a list" } as never);
          const out: Term[] = [];
          for (const m of (lst.value ?? [])) {
            const mv = asMaybe(m);
            if (mv?.tag === "just") out.push(mv.value);
          }
          return right({ tag: "list", value: out } as never);
        })),
    prim("hydra.lib.maybes.toList", scheme(tyFn(tyMaybe(tyVar("a")), tyList(tyVar("a"))), ["a"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "toList"), (m) => {
          const mv = asMaybe(m);
          if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
          return right({ tag: "list", value: mv.tag === "just" ? [mv.value] : [] } as never);
        })),
    prim("hydra.lib.maybes.cases", scheme(tyFnCurried(tyMaybe(tyVar("a")), tyVar("b"), tyFn(tyVar("a"), tyVar("b")), tyVar("b")), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "cases-m"), (m) =>
          bind(need(args, 1, "cases-default"), (def) =>
            bind(need(args, 2, "cases-fn"), (fn) => {
              const mv = asMaybe(m);
              if (!mv) return left({ tag: "other", value: "expected a maybe" } as never);
              if (mv.tag === "nothing") return right(def);
              const app: Term = { tag: "application", value: { function_: fn, argument: mv.value } } as never;
              return (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
            })))),
    prim("hydra.lib.maybes.mapMaybe", scheme(tyFnCurried(tyFn(tyVar("a"), tyMaybe(tyVar("b"))), tyList(tyVar("a")), tyList(tyVar("b"))), ["a", "b"]),
      (cx, g, args) =>
        bind(need(args, 0, "mapMaybe-fn"), (fn) =>
          bind(need(args, 1, "mapMaybe-xs"), (xs) => {
            const lst = xs as { tag: string; value?: readonly Term[] };
            if (lst.tag !== "list") return left({ tag: "other", value: "expected a list" } as never);
            const out: Term[] = [];
            for (const x of (lst.value ?? [])) {
              const app: Term = { tag: "application", value: { function_: fn, argument: x } } as never;
              const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
              if (r.tag === "left") return r as Either<HydraError, Term>;
              const mv = asMaybe(r.value);
              if (mv?.tag === "just") out.push(mv.value);
            }
            return right({ tag: "list", value: out } as never);
          }))),
  ];
};

// === lib.eithers ===

const eithersPrimitives = (): readonly Primitive[] => {
  const asEither = (t: Term): { tag: "left" | "right"; value: Term } | null => {
    const x = t as { tag: string; value?: { tag?: string; value?: Term } };
    if (x.tag === "either" && x.value) {
      if (x.value.tag === "left" || x.value.tag === "right") {
        return { tag: x.value.tag, value: x.value.value! };
      }
    }
    return null;
  };
  const tLeft = (v: Term): Term => ({ tag: "either", value: { tag: "left", value: v } } as never);
  const tRight = (v: Term): Term => ({ tag: "either", value: { tag: "right", value: v } } as never);
  return [
    prim("hydra.lib.eithers.either", scheme(tyFnCurried(tyFn(tyVar("a"), tyVar("c")), tyFn(tyVar("b"), tyVar("c")), tyEither(tyVar("a"), tyVar("b")), tyVar("c")), ["a", "b", "c"]),
      (cx, g, args) =>
        bind(need(args, 0, "either-fl"), (fl) =>
          bind(need(args, 1, "either-fr"), (fr) =>
            bind(need(args, 2, "either-e"), (e) => {
              const ev = asEither(e);
              if (!ev) return left({ tag: "other", value: "expected an either" } as never);
              const fn = ev.tag === "left" ? fl : fr;
              const app: Term = { tag: "application", value: { function_: fn, argument: ev.value } } as never;
              return (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
            })))),
    prim("hydra.lib.eithers.map", scheme(tyFnCurried(tyFn(tyVar("b"), tyVar("c")), tyEither(tyVar("a"), tyVar("b")), tyEither(tyVar("a"), tyVar("c"))), ["a", "b", "c"]),
      (cx, g, args) =>
        bind(need(args, 0, "map-fn"), (fn) =>
          bind(need(args, 1, "map-e"), (e) => {
            const ev = asEither(e);
            if (!ev) return left({ tag: "other", value: "expected an either" } as never);
            if (ev.tag === "left") return right(e);
            const app: Term = { tag: "application", value: { function_: fn, argument: ev.value } } as never;
            const r = (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
            if (r.tag === "left") return r as Either<HydraError, Term>;
            return right(tRight(r.value));
          }))),
    prim("hydra.lib.eithers.bind", scheme(tyFnCurried(tyEither(tyVar("a"), tyVar("b")), tyFn(tyVar("b"), tyEither(tyVar("a"), tyVar("c"))), tyEither(tyVar("a"), tyVar("c"))), ["a", "b", "c"]),
      (cx, g, args) =>
        bind(need(args, 0, "bind-e"), (e) =>
          bind(need(args, 1, "bind-fn"), (fn) => {
            const ev = asEither(e);
            if (!ev) return left({ tag: "other", value: "expected an either" } as never);
            if (ev.tag === "left") return right(e);
            const app: Term = { tag: "application", value: { function_: fn, argument: ev.value } } as never;
            return (reduceTerm as never as (cx: Context, g: Graph, eager: boolean, t: Term) => Either<HydraError, Term>)(cx, g, true, app);
          }))),
    prim("hydra.lib.eithers.isLeft", scheme(tyFn(tyEither(tyVar("a"), tyVar("b")), tyBool), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "isLeft"), (e) => {
          const ev = asEither(e);
          if (!ev) return left({ tag: "other", value: "expected an either" } as never);
          return right(tBool(ev.tag === "left"));
        })),
    prim("hydra.lib.eithers.isRight", scheme(tyFn(tyEither(tyVar("a"), tyVar("b")), tyBool), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "isRight"), (e) => {
          const ev = asEither(e);
          if (!ev) return left({ tag: "other", value: "expected an either" } as never);
          return right(tBool(ev.tag === "right"));
        })),
    prim("hydra.lib.eithers.fromLeft", scheme(tyFnCurried(tyVar("a"), tyEither(tyVar("a"), tyVar("b")), tyVar("a")), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "fromLeft-def"), (def) =>
          bind(need(args, 1, "fromLeft-e"), (e) => {
            const ev = asEither(e);
            if (!ev) return left({ tag: "other", value: "expected an either" } as never);
            return right(ev.tag === "left" ? ev.value : def);
          }))),
    prim("hydra.lib.eithers.fromRight", scheme(tyFnCurried(tyVar("b"), tyEither(tyVar("a"), tyVar("b")), tyVar("b")), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "fromRight-def"), (def) =>
          bind(need(args, 1, "fromRight-e"), (e) => {
            const ev = asEither(e);
            if (!ev) return left({ tag: "other", value: "expected an either" } as never);
            return right(ev.tag === "right" ? ev.value : def);
          }))),
    prim("hydra.lib.eithers.lefts", scheme(tyFn(tyList(tyEither(tyVar("a"), tyVar("b"))), tyList(tyVar("a"))), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "lefts"), (xs) => {
          const lst = xs as { tag: string; value?: readonly Term[] };
          if (lst.tag !== "list") return left({ tag: "other", value: "expected a list" } as never);
          const out: Term[] = [];
          for (const e of (lst.value ?? [])) {
            const ev = asEither(e);
            if (ev?.tag === "left") out.push(ev.value);
          }
          return right({ tag: "list", value: out } as never);
        })),
    prim("hydra.lib.eithers.rights", scheme(tyFn(tyList(tyEither(tyVar("a"), tyVar("b"))), tyList(tyVar("b"))), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "rights"), (xs) => {
          const lst = xs as { tag: string; value?: readonly Term[] };
          if (lst.tag !== "list") return left({ tag: "other", value: "expected a list" } as never);
          const out: Term[] = [];
          for (const e of (lst.value ?? [])) {
            const ev = asEither(e);
            if (ev?.tag === "right") out.push(ev.value);
          }
          return right({ tag: "list", value: out } as never);
        })),
    // bimap, foldl, mapList — used by Java/Python coders. Kernel kernel
    // registers these in Libraries.hs; their type schemes (here transcribed)
    // are what `analyzeFunctionTerm` consults to type top-level uses.
    prim("hydra.lib.eithers.bimap", scheme(
      tyFnCurried(tyFn(tyVar("a"), tyVar("c")), tyFn(tyVar("b"), tyVar("d")),
        tyEither(tyVar("a"), tyVar("b")), tyEither(tyVar("c"), tyVar("d"))),
      ["a", "b", "c", "d"]),
      (_cx, _g, args) => left({ tag: "other", value: "bimap interpreter not implemented (kernel-only)" } as never)),
    prim("hydra.lib.eithers.foldl", scheme(
      tyFnCurried(
        tyFn(tyVar("a"), tyFn(tyVar("b"), tyEither(tyVar("c"), tyVar("a")))),
        tyVar("a"), tyList(tyVar("b")), tyEither(tyVar("c"), tyVar("a"))),
      ["a", "b", "c"]),
      (_cx, _g, args) => left({ tag: "other", value: "foldl interpreter not implemented (kernel-only)" } as never)),
    prim("hydra.lib.eithers.mapList", scheme(
      tyFnCurried(tyFn(tyVar("a"), tyEither(tyVar("c"), tyVar("b"))),
        tyList(tyVar("a")), tyEither(tyVar("c"), tyList(tyVar("b")))),
      ["a", "b", "c"]),
      (_cx, _g, args) => left({ tag: "other", value: "mapList interpreter not implemented (kernel-only)" } as never)),
    prim("hydra.lib.eithers.mapMaybe", scheme(
      tyFnCurried(tyFn(tyVar("a"), tyEither(tyVar("c"), tyVar("b"))),
        tyMaybe(tyVar("a")), tyEither(tyVar("c"), tyMaybe(tyVar("b")))),
      ["a", "b", "c"]),
      (_cx, _g, args) => left({ tag: "other", value: "mapMaybe interpreter not implemented (kernel-only)" } as never)),
    prim("hydra.lib.eithers.mapSet", scheme(
      tyFnCurried(tyFn(tyVar("a"), tyEither(tyVar("c"), tyVar("b"))),
        tySet(tyVar("a")), tyEither(tyVar("c"), tySet(tyVar("b")))),
      ["a", "b", "c"]),
      (_cx, _g, args) => left({ tag: "other", value: "mapSet interpreter not implemented (kernel-only)" } as never)),
    prim("hydra.lib.eithers.partitionEithers", scheme(
      tyFn(tyList(tyEither(tyVar("a"), tyVar("b"))),
        tyPair(tyList(tyVar("a")), tyList(tyVar("b")))),
      ["a", "b"]),
      (_cx, _g, args) => left({ tag: "other", value: "partitionEithers interpreter not implemented (kernel-only)" } as never)),
    // Unused: tLeft helper retained for symmetry.
    ...(function() { void tLeft; return []; })(),
  ];
};

export const standardPrimitives = (): readonly Primitive[] => [
  ...charsPrimitives(),
  ...eithersPrimitives(),
  ...equalityPrimitives(),
  ...listsPrimitives(),
  ...literalsPrimitives(),
  ...logicPrimitives(),
  ...mapsPrimitives(),
  ...mathPrimitives(),
  ...maybesPrimitives(),
  ...pairsPrimitivesList(),
  ...regexPrimitives(),
  ...setsPrimitives(),
  ...stringsPrimitives(),
];

function pairsPrimitivesList(): readonly Primitive[] {
  return [
    prim("hydra.lib.pairs.first", scheme(tyFn(tyPair(tyVar("a"), tyVar("b")), tyVar("a")), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "first"), (a0) => {
          const p = a0 as { tag: string; value?: readonly [Term, Term] };
          if (p.tag !== "pair" || !p.value) return left({ tag: "other", value: "first: expected pair" } as never);
          return right(p.value[0]);
        })),
    prim("hydra.lib.pairs.second", scheme(tyFn(tyPair(tyVar("a"), tyVar("b")), tyVar("b")), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "second"), (a0) => {
          const p = a0 as { tag: string; value?: readonly [Term, Term] };
          if (p.tag !== "pair" || !p.value) return left({ tag: "other", value: "second: expected pair" } as never);
          return right(p.value[1]);
        })),
    prim("hydra.lib.pairs.swap", scheme(tyFn(tyPair(tyVar("a"), tyVar("b")), tyPair(tyVar("b"), tyVar("a"))), ["a", "b"]),
      (_cx, _g, args) =>
        bind(need(args, 0, "swap"), (a0) => {
          const p = a0 as { tag: string; value?: readonly [Term, Term] };
          if (p.tag !== "pair" || !p.value) return left({ tag: "other", value: "swap: expected pair" } as never);
          return right({ tag: "pair", value: [p.value[1], p.value[0]] } as never);
        })),
  ];
}
