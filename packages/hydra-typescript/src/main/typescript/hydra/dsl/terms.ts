/**
 * A DSL for constructing Hydra terms in TypeScript.
 *
 * Mirrors the Haskell DSL in Hydra.Dsl.Terms.
 */

import type {
  Field, Term, Type, TypeScheme,
} from "../core.js";
import { Name, just, nothing } from "../core.js";

export function apply(func: Term, arg: Term): Term {
  return { tag: "application", value: { function: func, argument: arg } };
}

export function boolean(b: boolean): Term {
  return { tag: "literal", value: { tag: "boolean", value: b } };
}

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
