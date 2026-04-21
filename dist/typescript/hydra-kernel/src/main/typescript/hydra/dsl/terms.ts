/**
 * A DSL for constructing Hydra terms in TypeScript.
 *
 * Mirrors the Haskell DSL in Hydra.Dsl.Terms.
 */

import type {
  Field, Literal, Term, Type, TypeScheme,
} from "../core.js";
import { Name, just, nothing } from "../core.js";

export function apply(func: Term, arg: Term): Term {
  return { tag: "application", value: { function: func, argument: arg } };
}

export function applyAll(func: Term, args: ReadonlyArray<Term>): Term {
  let result = func;
  for (const arg of args) {
    result = apply(result, arg);
  }
  return result;
}

// --- Literals ---

export function literal(lit: Literal): Term {
  return { tag: "literal", value: lit };
}

export function bigfloat(n: number): Term {
  return { tag: "literal", value: { tag: "float", value: { tag: "bigfloat", value: n } } };
}

export function binary(b: Uint8Array): Term {
  return { tag: "literal", value: { tag: "binary", value: b } };
}

export function boolean(b: boolean): Term {
  return { tag: "literal", value: { tag: "boolean", value: b } };
}

export const false_ = boolean(false);
export const true_ = boolean(true);

export function float32(n: number): Term {
  return {
    tag: "literal",
    value: { tag: "float", value: { tag: "float32", value: n } },
  };
}

export function float64(n: number): Term {
  return {
    tag: "literal",
    value: { tag: "float", value: { tag: "float64", value: n } },
  };
}

export function int8(n: number): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "int8", value: n } } };
}

export function int16(n: number): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "int16", value: n } } };
}

export function int32(n: number): Term {
  return {
    tag: "literal",
    value: { tag: "integer", value: { tag: "int32", value: n } },
  };
}

export function int64(n: bigint): Term {
  return {
    tag: "literal",
    value: { tag: "integer", value: { tag: "int64", value: n } },
  };
}

export function uint8(n: number): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "uint8", value: n } } };
}

export function uint16(n: number): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "uint16", value: n } } };
}

export function uint32(n: number): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "uint32", value: n } } };
}

export function uint64(n: bigint): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "uint64", value: n } } };
}

export function bigint_(n: bigint): Term {
  return { tag: "literal", value: { tag: "integer", value: { tag: "bigint", value: n } } };
}

export function string_(s: string): Term {
  return { tag: "literal", value: { tag: "string", value: s } };
}

export function list(elements: ReadonlyArray<Term>): Term {
  return { tag: "list", value: elements };
}

export function set(elements: ReadonlySet<Term>): Term {
  return { tag: "set", value: elements };
}

export function map(entries: ReadonlyMap<Term, Term>): Term {
  return { tag: "map", value: entries };
}

export function record(typeName: string, fields: ReadonlyArray<Field>): Term {
  return {
    tag: "record",
    value: { typeName: Name(typeName), fields },
  };
}

export function field(name: string, term: Term): Field {
  return { name: Name(name), term };
}

export function inject(
  typeName: string,
  fieldName: string,
  value: Term,
): Term {
  return {
    tag: "inject",
    value: {
      typeName: Name(typeName),
      field: { name: Name(fieldName), term: value },
    },
  };
}

export function lambda(parameter: string, body: Term): Term {
  return {
    tag: "lambda",
    value: {
      parameter: Name(parameter),
      domain: nothing(),
      body,
    },
  };
}

export function lambdaTyped(
  parameter: string,
  domain: Type,
  body: Term,
): Term {
  return {
    tag: "lambda",
    value: {
      parameter: Name(parameter),
      domain: just(domain),
      body,
    },
  };
}

export function let_(
  bindings: ReadonlyArray<{
    readonly name: string;
    readonly term: Term;
  }>,
  body: Term,
): Term {
  return {
    tag: "let",
    value: {
      bindings: bindings.map((b) => ({
        name: Name(b.name),
        term: b.term,
        type: nothing<TypeScheme>(),
      })),
      environment: body,
    },
  };
}

export function variable(name: string): Term {
  return { tag: "variable", value: Name(name) };
}

export function unit(): Term {
  return { tag: "unit" };
}

export function project(typeName: string, fieldName: string): Term {
  return {
    tag: "project",
    value: {
      typeName: Name(typeName),
      field: Name(fieldName),
    },
  };
}

export function wrap(typeName: string, inner: Term): Term {
  return {
    tag: "wrap",
    value: {
      typeName: Name(typeName),
      object: inner,
    },
  };
}

export function unwrap(typeName: string): Term {
  return { tag: "unwrap", value: Name(typeName) };
}

export function pair(first: Term, second: Term): Term {
  return { tag: "pair", value: [first, second] as const };
}

export function maybe_nothing(): Term {
  return { tag: "maybe", value: nothing() };
}

export function maybe_just(t: Term): Term {
  return { tag: "maybe", value: just(t) };
}

export function either_left(t: Term): Term {
  return { tag: "either", value: { tag: "left", value: t } };
}

export function either_right(t: Term): Term {
  return { tag: "either", value: { tag: "right", value: t } };
}

// --- Combinators ---

export function identity(): Term {
  return lambda("x", variable("x"));
}

export function constant(value: Term): Term {
  return lambda("_", value);
}

export function compose(f: Term, g: Term): Term {
  return lambda("x", apply(f, apply(g, variable("x"))));
}

export function primitive(name: string): Term {
  return variable(name);
}

// --- Type-level polymorphism ---

export function typeApplication(func: Term, argument: Type): Term {
  return { tag: "typeApplication", value: { function: func, argument } };
}

export function typeLambda(parameter: string, body: Term): Term {
  return { tag: "typeLambda", value: { parameter: Name(parameter), body } };
}

// --- Match / cases ---

export function match(
  typeName: string,
  defaultTerm: Term | undefined,
  cases: ReadonlyArray<Field>,
): Term {
  return {
    tag: "cases",
    value: {
      typeName: Name(typeName),
      default: defaultTerm !== undefined ? just(defaultTerm) : nothing(),
      cases,
    },
  };
}

// --- Injection helpers ---

export function injectUnit(typeName: string, fieldName: string): Term {
  return inject(typeName, fieldName, unit());
}

// --- Let (typed) ---

export function letTyped(
  bindings: ReadonlyArray<{
    readonly name: string;
    readonly term: Term;
    readonly type: TypeScheme;
  }>,
  body: Term,
): Term {
  return {
    tag: "let",
    value: {
      bindings: bindings.map((b) => ({
        name: Name(b.name),
        term: b.term,
        type: just(b.type),
      })),
      environment: body,
    },
  };
}

// --- Tuple convenience ---

export function tuple2(a: Term, b: Term): Term {
  return pair(a, b);
}

export function optional(m: import("../core.js").Maybe<Term>): Term {
  return { tag: "maybe", value: m };
}
