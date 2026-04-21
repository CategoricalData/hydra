/**
 * Set primitive functions for Hydra-TypeScript.
 */

export function delete_<T>(x: T): (s: ReadonlySet<T>) => ReadonlySet<T> {
  return (s) => {
    const result = new Set(s);
    result.delete(x);
    return result;
  };
}

export function difference<T>(
  a: ReadonlySet<T>,
): (b: ReadonlySet<T>) => ReadonlySet<T> {
  return (b) => {
    const result = new Set<T>();
    for (const x of a) {
      if (!b.has(x)) result.add(x);
    }
    return result;
  };
}

export function empty<T>(): ReadonlySet<T> {
  return new Set();
}

export function fromList<T>(xs: ReadonlyArray<T>): ReadonlySet<T> {
  return new Set(xs);
}

export function insert<T>(x: T): (s: ReadonlySet<T>) => ReadonlySet<T> {
  return (s) => {
    const result = new Set(s);
    result.add(x);
    return result;
  };
}

export function intersection<T>(
  a: ReadonlySet<T>,
): (b: ReadonlySet<T>) => ReadonlySet<T> {
  return (b) => {
    const result = new Set<T>();
    for (const x of a) {
      if (b.has(x)) result.add(x);
    }
    return result;
  };
}

export function map<A, B>(
  f: (a: A) => B,
): (s: ReadonlySet<A>) => ReadonlySet<B> {
  return (s) => {
    const result = new Set<B>();
    for (const x of s) result.add(f(x));
    return result;
  };
}

export function member<T>(x: T): (s: ReadonlySet<T>) => boolean {
  return (s) => s.has(x);
}

export function null_<T>(s: ReadonlySet<T>): boolean {
  return s.size === 0;
}

export function singleton<T>(x: T): ReadonlySet<T> {
  return new Set([x]);
}

export function size<T>(s: ReadonlySet<T>): number {
  return s.size;
}

export function toList<T>(s: ReadonlySet<T>): ReadonlyArray<T> {
  return [...s];
}

export function union<T>(
  a: ReadonlySet<T>,
): (b: ReadonlySet<T>) => ReadonlySet<T> {
  return (b) => {
    const result = new Set(a);
    for (const x of b) result.add(x);
    return result;
  };
}

export function unions<T>(
  sets: ReadonlyArray<ReadonlySet<T>>,
): ReadonlySet<T> {
  const result = new Set<T>();
  for (const s of sets) {
    for (const x of s) result.add(x);
  }
  return result;
}
