import { describe, it, expect } from "vitest";
import * as lists from "../../../main/typescript/hydra/lib/lists.js";
import * as strings from "../../../main/typescript/hydra/lib/strings.js";
import * as maps from "../../../main/typescript/hydra/lib/maps.js";
import * as maybes from "../../../main/typescript/hydra/lib/maybes.js";
import { just, nothing } from "../../../main/typescript/hydra/core.js";

describe("lists", () => {
  it("cons prepends an element", () => {
    expect(lists.cons(1, [2, 3])).toEqual([1, 2, 3]);
  });

  it("head returns the first element", () => {
    expect(lists.head([1, 2, 3])).toBe(1);
  });

  it("tail returns all but first", () => {
    expect(lists.tail([1, 2, 3])).toEqual([2, 3]);
  });

  it("map applies a function", () => {
    expect(lists.map((x: number) => x * 2, [1, 2, 3])).toEqual([2, 4, 6]);
  });

  it("filter selects matching elements", () => {
    expect(lists.filter((x: number) => x > 1, [1, 2, 3])).toEqual([2, 3]);
  });

  it("foldl accumulates left-to-right", () => {
    expect(lists.foldl((acc: number, x: number) => acc + x, 0, [1, 2, 3])).toBe(6);
  });

  it("concat flattens nested arrays", () => {
    expect(lists.concat([[1, 2], [3], [4, 5]])).toEqual([1, 2, 3, 4, 5]);
  });

  it("length returns the count", () => {
    expect(lists.length([1, 2, 3])).toBe(3);
  });
});

describe("strings", () => {
  it("cat joins strings", () => {
    expect(strings.cat(["a", "b", "c"])).toBe("abc");
  });

  it("intercalate joins with separator", () => {
    expect(strings.intercalate(", ", ["a", "b", "c"])).toBe("a, b, c");
  });

  it("lines splits on newlines", () => {
    expect(strings.lines("a\nb\nc")).toEqual(["a", "b", "c"]);
  });

  it("toLower lowercases", () => {
    expect(strings.toLower("Hello")).toBe("hello");
  });
});

describe("maps", () => {
  it("singleton creates a one-entry map", () => {
    const m = maps.singleton("key", 42);
    expect(m.get("key")).toBe(42);
    expect(m.size).toBe(1);
  });

  it("lookup finds existing keys", () => {
    const m = maps.singleton("a", 1);
    expect(maps.lookup("a", m)).toEqual(just(1));
  });

  it("lookup returns nothing for missing keys", () => {
    const m = maps.empty<string, number>();
    expect(maps.lookup("a", m)).toEqual(nothing());
  });
});

describe("maybes", () => {
  it("fromMaybe returns value when just", () => {
    expect(maybes.fromMaybe(0, just(42))).toBe(42);
  });

  it("fromMaybe returns default when nothing", () => {
    expect(maybes.fromMaybe(0, nothing())).toBe(0);
  });

  it("maybe applies function when just", () => {
    expect(maybes.maybe("none", (n: number) => `got ${n}`, just(42))).toBe("got 42");
  });
});
