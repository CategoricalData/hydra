import { describe, expect, it } from "vitest";

import { isGiven, None } from "../../../../../../dist/typescript/hydra-kernel/src/main/typescript/hydra/runtime.js";
import {
  div,
  mod,
} from "../../../../../../dist/typescript/hydra-kernel/src/main/typescript/hydra/overlay/typescript/lib/math.js";

// Regression test for #677: exercises the *emitted* flat-function surface
// (hydra/overlay/typescript/lib/math.ts) directly, not the registry/interpreter
// path (libraries.ts) that the generated `primCase` suite (Test/Lib/Math.hs) runs
// through. The two surfaces previously diverged: the registry was already correct,
// but coder-emitted code calls this file's `div`/`mod` directly, and those used to
// do float division / JS `%` instead of floor-division + Knuth-mod. A negative
// dividend is required to catch it -- every non-negative case agrees with both
// the correct and the previously-broken implementation.
describe("overlay/typescript/lib/math (emitted path)", () => {
  it("div is floor division, not truncation or float division", () => {
    expect(div(7, 2)).toEqual({ tag: "given", value: 3 });
    expect(div(-7, 2)).toEqual({ tag: "given", value: -4 });
    expect(div(7, -2)).toEqual({ tag: "given", value: -4 });
    expect(div(-7, -2)).toEqual({ tag: "given", value: 3 });
    expect(div(-6, 2)).toEqual({ tag: "given", value: -3 });
  });

  it("div guards the zero divisor", () => {
    expect(div(7, 0)).toBe(None);
    expect(isGiven(div(7, 0))).toBe(false);
  });

  it("div wraps the (minBound, -1) boundary instead of overflowing", () => {
    expect(div(-2147483648, -1)).toEqual({ tag: "given", value: -2147483648 });
  });

  it("mod is Knuth floor-mod: sign follows the divisor", () => {
    expect(mod(7, 2)).toEqual({ tag: "given", value: 1 });
    expect(mod(-7, 2)).toEqual({ tag: "given", value: 1 });
    expect(mod(7, -2)).toEqual({ tag: "given", value: -1 });
    expect(mod(-7, -2)).toEqual({ tag: "given", value: -1 });
    expect(mod(-6, 2)).toEqual({ tag: "given", value: 0 });
  });

  it("mod guards the zero divisor", () => {
    expect(mod(7, 0)).toBe(None);
  });
});
