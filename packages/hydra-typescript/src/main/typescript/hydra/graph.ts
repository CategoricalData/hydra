/**
 * Graph construction and lookup utilities for Hydra-TypeScript.
 */

import type {
  Graph, Name, Primitive, Term, TypeScheme, Either,
} from "./core.js";
import { left, right } from "./core.js";

export function emptyGraph(): Graph {
  return {
    elements: new Map(),
    types: new Map(),
    primitives: new Map(),
  };
}

export function graphWithElements(
  elements: ReadonlyMap<Name, Term>,
): Graph {
  return {
    elements,
    types: new Map(),
    primitives: new Map(),
  };
}

export function lookupPrimitive(
  name: Name,
  graph: Graph,
): Either<string, Primitive> {
  const prim = graph.primitives.get(name);
  if (prim === undefined) {
    return left(`Unknown primitive: ${name}`);
  }
  return right(prim);
}

export function lookupType(
  name: Name,
  graph: Graph,
): Either<string, TypeScheme> {
  const ts = graph.types.get(name);
  if (ts === undefined) {
    return left(`Type not found: ${name}`);
  }
  return right(ts);
}
