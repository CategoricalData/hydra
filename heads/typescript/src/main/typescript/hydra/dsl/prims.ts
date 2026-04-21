/**
 * Primitive construction utilities for Hydra-TypeScript.
 *
 * Provides prim0/prim1/prim2/prim3 constructors that create Primitive objects
 * wiring term extraction, native computation, and term wrapping.
 */

import type { Either, Term, Type, TypeScheme, Primitive } from "../core.js";
import { Name, left, right } from "../core.js";
import * as types from "./types.js";
import * as terms from "./terms.js";
import * as expect from "./expect.js";

// --- Term coders: encode (Term -> native) and decode (native -> Term) ---

export type TermCoder<T> = {
  readonly type: Type;
  readonly encode: (t: Term) => Either<string, T>;
  readonly decode: (v: T) => Either<string, Term>;
};

// Scalar coders

export function boolean(): TermCoder<boolean> {
  return {
    type: types.boolean(),
    encode: expect.expectBoolean,
    decode: (v) => right(terms.boolean(v)),
  };
}

export function string(): TermCoder<string> {
  return {
    type: types.string_(),
    encode: expect.expectString,
    decode: (v) => right(terms.string_(v)),
  };
}

export function int8(): TermCoder<number> {
  return {
    type: types.int8(),
    encode: expect.expectInt8,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "int8", value: v } } }),
  };
}

export function int16(): TermCoder<number> {
  return {
    type: types.int16(),
    encode: expect.expectInt16,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "int16", value: v } } }),
  };
}

export function int32(): TermCoder<number> {
  return {
    type: types.int32(),
    encode: expect.expectInt32,
    decode: (v) => right(terms.int32(v)),
  };
}

export function int64(): TermCoder<bigint> {
  return {
    type: types.int64(),
    encode: expect.expectInt64,
    decode: (v) => right(terms.int64(v)),
  };
}

export function uint8(): TermCoder<number> {
  return {
    type: types.int8(),
    encode: expect.expectUint8,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "uint8", value: v } } }),
  };
}

export function uint16(): TermCoder<number> {
  return {
    type: types.int16(),
    encode: expect.expectUint16,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "uint16", value: v } } }),
  };
}

export function uint32(): TermCoder<number> {
  return {
    type: types.int32(),
    encode: expect.expectUint32,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "uint32", value: v } } }),
  };
}

export function uint64(): TermCoder<bigint> {
  return {
    type: types.int64(),
    encode: expect.expectUint64,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "uint64", value: v } } }),
  };
}

export function float32(): TermCoder<number> {
  return {
    type: types.float32(),
    encode: expect.expectFloat32,
    decode: (v) => right(terms.float32(v)),
  };
}

export function float64(): TermCoder<number> {
  return {
    type: types.float64(),
    encode: expect.expectFloat64,
    decode: (v) => right(terms.float64(v)),
  };
}

export function bigfloat(): TermCoder<number> {
  return {
    type: { tag: "literal", value: { tag: "float", value: { tag: "bigfloat" } } },
    encode: expect.expectBigfloat,
    decode: (v) => right({ tag: "literal", value: { tag: "float", value: { tag: "bigfloat", value: v } } }),
  };
}

export function bigint_(): TermCoder<bigint> {
  return {
    type: types.bigint_(),
    encode: expect.expectBigint,
    decode: (v) => right({ tag: "literal", value: { tag: "integer", value: { tag: "bigint", value: v } } }),
  };
}

export function binary(): TermCoder<Uint8Array> {
  return {
    type: types.binary(),
    encode: expect.expectBinary,
    decode: (v) => right({ tag: "literal", value: { tag: "binary", value: v } } as Term),
  };
}

// Container coders

export function list_<T>(el: TermCoder<T>): TermCoder<ReadonlyArray<T>> {
  return {
    type: types.list(el.type),
    encode: (t) => {
      const r = expect.expectList(t);
      if (r.tag === "left") return r;
      const result: T[] = [];
      for (const item of r.value) {
        const ir = el.encode(item);
        if (ir.tag === "left") return ir;
        result.push(ir.value);
      }
      return right(result);
    },
    decode: (v) => {
      const items: Term[] = [];
      for (const item of v) {
        const r = el.decode(item);
        if (r.tag === "left") return r;
        items.push(r.value);
      }
      return right(terms.list(items));
    },
  };
}

export function set_<T>(el: TermCoder<T>): TermCoder<ReadonlySet<T>> {
  return {
    type: types.set(el.type),
    encode: (t) => {
      const r = expect.expectSet(t);
      if (r.tag === "left") return r;
      const result = new Set<T>();
      for (const item of r.value) {
        const ir = el.encode(item);
        if (ir.tag === "left") return ir;
        result.add(ir.value);
      }
      return right(result);
    },
    decode: (v) => {
      const items = new Set<Term>();
      for (const item of v) {
        const r = el.decode(item);
        if (r.tag === "left") return r;
        items.add(r.value);
      }
      return right(terms.set(items));
    },
  };
}

export function map_<K, V>(
  keyCoder: TermCoder<K>,
  valCoder: TermCoder<V>,
): TermCoder<ReadonlyMap<K, V>> {
  return {
    type: types.map(keyCoder.type, valCoder.type),
    encode: (t) => {
      const r = expect.expectMap(t);
      if (r.tag === "left") return r;
      const result = new Map<K, V>();
      for (const [kt, vt] of r.value) {
        const kr = keyCoder.encode(kt);
        if (kr.tag === "left") return kr;
        const vr = valCoder.encode(vt);
        if (vr.tag === "left") return vr;
        result.set(kr.value, vr.value);
      }
      return right(result);
    },
    decode: (v) => {
      const entries = new Map<Term, Term>();
      for (const [k, val] of v) {
        const kr = keyCoder.decode(k);
        if (kr.tag === "left") return kr;
        const vr = valCoder.decode(val);
        if (vr.tag === "left") return vr;
        entries.set(kr.value, vr.value);
      }
      return right(terms.map(entries));
    },
  };
}

export function optional<T>(el: TermCoder<T>): TermCoder<import("../core.js").Maybe<T>> {
  return {
    type: types.maybe(el.type),
    encode: (t) => {
      const r = expect.expectMaybe(t);
      if (r.tag === "left") return r;
      if (r.value.tag === "nothing") return right({ tag: "nothing" as const });
      const ir = el.encode(r.value.value);
      if (ir.tag === "left") return ir;
      return right({ tag: "just" as const, value: ir.value });
    },
    decode: (v) => {
      if (v.tag === "nothing") return right(terms.maybe_nothing());
      const r = el.decode(v.value);
      if (r.tag === "left") return r;
      return right(terms.maybe_just(r.value));
    },
  };
}

export function either<L, R>(
  leftCoder: TermCoder<L>,
  rightCoder: TermCoder<R>,
): TermCoder<import("../core.js").Either<L, R>> {
  return {
    type: types.either(leftCoder.type, rightCoder.type),
    encode: (t) => {
      const r = expect.expectEither(t);
      if (r.tag === "left") return r as Either<string, never>;
      const e = r.value;
      if (e.tag === "left") {
        const lr = leftCoder.encode(e.value);
        if (lr.tag === "left") return lr;
        return right({ tag: "left" as const, value: lr.value });
      }
      const rr = rightCoder.encode(e.value);
      if (rr.tag === "left") return rr;
      return right({ tag: "right" as const, value: rr.value });
    },
    decode: (v) => {
      if (v.tag === "left") {
        const r = leftCoder.decode(v.value);
        if (r.tag === "left") return r;
        return right(terms.either_left(r.value));
      }
      const r = rightCoder.decode(v.value);
      if (r.tag === "left") return r;
      return right(terms.either_right(r.value));
    },
  };
}

export function pair<A, B>(
  firstCoder: TermCoder<A>,
  secondCoder: TermCoder<B>,
): TermCoder<readonly [A, B]> {
  return {
    type: types.pair(firstCoder.type, secondCoder.type),
    encode: (t) => {
      const r = expect.expectPair(t);
      if (r.tag === "left") return r;
      const ar = firstCoder.encode(r.value[0]);
      if (ar.tag === "left") return ar;
      const br = secondCoder.encode(r.value[1]);
      if (br.tag === "left") return br;
      return right([ar.value, br.value] as const);
    },
    decode: (v) => {
      const ar = firstCoder.decode(v[0]);
      if (ar.tag === "left") return ar;
      const br = secondCoder.decode(v[1]);
      if (br.tag === "left") return br;
      return right(terms.pair(ar.value, br.value));
    },
  };
}

// Type variable coder — passes Terms through unchanged
export function variable(_name: string): TermCoder<Term> {
  return {
    type: types.variable(_name),
    encode: (t) => right(t),
    decode: (t) => right(t),
  };
}

// Function type — only for building type schemes, not for actual encoding
export function function_<A, B>(dom: TermCoder<A>, cod: TermCoder<B>): TermCoder<(a: A) => B> {
  return {
    type: types.func(dom.type, cod.type),
    encode: (_t) => left("cannot encode term to a function"),
    decode: (_v) => left("cannot decode functions to terms"),
  };
}

// --- Type scheme builder ---

export function typeScheme(variables: ReadonlyArray<string>, type: Type): TypeScheme {
  return {
    variables: variables.map(Name),
    type,
  };
}

// --- Primitive constructors ---

export function prim0<A>(
  name: string,
  compute: () => A,
  variables: ReadonlyArray<string>,
  output: TermCoder<A>,
): Primitive {
  const qname = Name(name);
  return {
    name: qname,
    type: typeScheme(variables, output.type),
    implementation: (...args: ReadonlyArray<Term>): Either<string, Term> => {
      const r = expect.expectNArgs(name, 0, args);
      if (r.tag === "left") return r;
      return output.decode(compute());
    },
  };
}

export function prim1<A, B>(
  name: string,
  compute: (a: A) => B,
  variables: ReadonlyArray<string>,
  input1: TermCoder<A>,
  output: TermCoder<B>,
): Primitive {
  const qname = Name(name);
  return {
    name: qname,
    type: typeScheme(variables, types.func(input1.type, output.type)),
    implementation: (...args: ReadonlyArray<Term>): Either<string, Term> => {
      const r = expect.expectNArgs(name, 1, args);
      if (r.tag === "left") return r;
      const a = input1.encode(args[0]!);
      if (a.tag === "left") return a;
      return output.decode(compute(a.value));
    },
  };
}

export function prim2<A, B, C>(
  name: string,
  compute: (a: A, b: B) => C,
  variables: ReadonlyArray<string>,
  input1: TermCoder<A>,
  input2: TermCoder<B>,
  output: TermCoder<C>,
): Primitive {
  const qname = Name(name);
  return {
    name: qname,
    type: typeScheme(variables, types.func(input1.type, types.func(input2.type, output.type))),
    implementation: (...args: ReadonlyArray<Term>): Either<string, Term> => {
      const r = expect.expectNArgs(name, 2, args);
      if (r.tag === "left") return r;
      const a = input1.encode(args[0]!);
      if (a.tag === "left") return a;
      const b = input2.encode(args[1]!);
      if (b.tag === "left") return b;
      return output.decode(compute(a.value, b.value));
    },
  };
}

export function prim3<A, B, C, D>(
  name: string,
  compute: (a: A, b: B, c: C) => D,
  variables: ReadonlyArray<string>,
  input1: TermCoder<A>,
  input2: TermCoder<B>,
  input3: TermCoder<C>,
  output: TermCoder<D>,
): Primitive {
  const qname = Name(name);
  return {
    name: qname,
    type: typeScheme(
      variables,
      types.func(input1.type, types.func(input2.type, types.func(input3.type, output.type))),
    ),
    implementation: (...args: ReadonlyArray<Term>): Either<string, Term> => {
      const r = expect.expectNArgs(name, 3, args);
      if (r.tag === "left") return r;
      const a = input1.encode(args[0]!);
      if (a.tag === "left") return a;
      const b = input2.encode(args[1]!);
      if (b.tag === "left") return b;
      const c = input3.encode(args[2]!);
      if (c.tag === "left") return c;
      return output.decode(compute(a.value, b.value, c.value));
    },
  };
}
