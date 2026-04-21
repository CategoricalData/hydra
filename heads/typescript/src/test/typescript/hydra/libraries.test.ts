import { describe, it, expect } from "vitest";
import { standardLibrary } from "../../../main/typescript/hydra/libraries.js";
import { right } from "../../../main/typescript/hydra/core.js";
import * as terms from "../../../main/typescript/hydra/dsl/terms.js";

describe("standardLibrary", () => {
  const lib = standardLibrary();

  it("registers a non-trivial number of primitives", () => {
    expect(lib.size).toBeGreaterThan(100);
  });

  it("contains expected primitive categories", () => {
    const names = [...lib.keys()];
    expect(names.some((n) => n.startsWith("hydra.lib.chars."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.eithers."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.equality."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.lists."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.literals."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.logic."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.maps."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.math."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.maybes."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.pairs."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.regex."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.sets."))).toBe(true);
    expect(names.some((n) => n.startsWith("hydra.lib.strings."))).toBe(true);
  });

  it("can invoke hydra.lib.math.add", () => {
    const prim = lib.get("hydra.lib.math.add");
    expect(prim).toBeDefined();
    const result = prim!.implementation(terms.int32(3), terms.int32(4));
    expect(result).toEqual(right(terms.int32(7)));
  });

  it("can invoke hydra.lib.strings.cat", () => {
    const prim = lib.get("hydra.lib.strings.cat");
    expect(prim).toBeDefined();
    const result = prim!.implementation(
      terms.list([terms.string_("hello"), terms.string_(" "), terms.string_("world")]),
    );
    expect(result).toEqual(right(terms.string_("hello world")));
  });

  it("can invoke hydra.lib.logic.not", () => {
    const prim = lib.get("hydra.lib.logic.not");
    expect(prim).toBeDefined();
    expect(prim!.implementation(terms.boolean(true))).toEqual(right(terms.boolean(false)));
    expect(prim!.implementation(terms.boolean(false))).toEqual(right(terms.boolean(true)));
  });

  it("can invoke hydra.lib.lists.length", () => {
    const prim = lib.get("hydra.lib.lists.length");
    expect(prim).toBeDefined();
    const result = prim!.implementation(
      terms.list([terms.int32(1), terms.int32(2), terms.int32(3)]),
    );
    expect(result).toEqual(right(terms.int32(3)));
  });

  it("can invoke hydra.lib.math.pi (prim0)", () => {
    const prim = lib.get("hydra.lib.math.pi");
    expect(prim).toBeDefined();
    const result = prim!.implementation();
    expect(result).toEqual(right(terms.float64(Math.PI)));
  });

  it("returns error for wrong argument count", () => {
    const prim = lib.get("hydra.lib.math.add");
    expect(prim).toBeDefined();
    const result = prim!.implementation(terms.int32(1));
    expect(result.tag).toBe("left");
  });

  it("returns error for wrong argument type", () => {
    const prim = lib.get("hydra.lib.math.add");
    expect(prim).toBeDefined();
    const result = prim!.implementation(terms.string_("not a number"), terms.int32(1));
    expect(result.tag).toBe("left");
  });

  it("primitives have valid type schemes", () => {
    for (const [name, prim] of lib) {
      expect(prim.type).toBeDefined();
      expect(prim.type.type).toBeDefined();
      expect(typeof prim.name).toBe("string");
    }
  });
});
