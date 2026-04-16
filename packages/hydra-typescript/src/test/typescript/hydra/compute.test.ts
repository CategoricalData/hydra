import { describe, it, expect } from "vitest";
import { right, left } from "../../../main/typescript/hydra/core.js";
import {
  mapEither, flatMapEither, pureEither, failEither,
} from "../../../main/typescript/hydra/compute.js";

describe("compute", () => {
  it("mapEither transforms right values", () => {
    const result = mapEither((x: number) => x * 2, right<string, number>(21));
    expect(result).toEqual(right(42));
  });

  it("mapEither passes through left values", () => {
    const result = mapEither((x: number) => x * 2, left<string, number>("err"));
    expect(result).toEqual(left("err"));
  });

  it("flatMapEither chains operations", () => {
    const safeDivide = (n: number) =>
      n === 0 ? left<string, number>("div by zero") : right<string, number>(100 / n);
    expect(flatMapEither(safeDivide, right<string, number>(10))).toEqual(right(10));
    expect(flatMapEither(safeDivide, right<string, number>(0))).toEqual(left("div by zero"));
  });

  it("pureEither wraps a value", () => {
    expect(pureEither<string, number>(42)).toEqual(right(42));
  });

  it("failEither creates a left", () => {
    expect(failEither("oops")).toEqual(left("oops"));
  });
});
