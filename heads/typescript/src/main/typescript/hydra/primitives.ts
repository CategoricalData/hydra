// Hand-written primitive-function registry for the Hydra TypeScript runtime.
//
// Hydra primitives are typed, named functions registered against a kernel
// graph. Each generated kernel module that uses a primitive looks it up by
// `Hydra.Core.Name` and applies it directly. The registry stays minimal here
// (a `Map<string, unknown>`); strict typing is provided at use sites by the
// generated code, not by this layer.

import { Name } from "./core.js";

export interface Primitive<T = unknown> {
  readonly name: Name;
  readonly impl: T;
}

export function Primitive<T>(name: Name | string, impl: T): Primitive<T> {
  const n = typeof name === "string" ? Name(name) : name;
  return { name: n, impl };
}

/** A primitive lookup table keyed by the primitive's `Name.value`. */
export class PrimitiveRegistry {
  private readonly entries = new Map<string, Primitive>();

  register<T>(primitive: Primitive<T>): void {
    const key = primitive.name.value;
    if (this.entries.has(key)) {
      throw new Error(`primitive already registered: ${key}`);
    }
    this.entries.set(key, primitive as Primitive);
  }

  lookup(name: Name | string): Primitive | undefined {
    const key = typeof name === "string" ? name : name.value;
    return this.entries.get(key);
  }

  require<T = unknown>(name: Name | string): Primitive<T> {
    const found = this.lookup(name);
    if (!found) {
      const key = typeof name === "string" ? name : name.value;
      throw new Error(`unknown primitive: ${key}`);
    }
    return found as Primitive<T>;
  }

  /** Iterate over registered primitives in insertion order. */
  *all(): IterableIterator<Primitive> {
    yield* this.entries.values();
  }

  get size(): number {
    return this.entries.size;
  }
}

/** The default registry. Generated `hydra/lib/*` modules populate this. */
export const defaultPrimitives = new PrimitiveRegistry();
