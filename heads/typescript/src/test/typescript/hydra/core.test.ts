import { describe, expect, it } from "vitest";

import {
  Either,
  Given,
  Left,
  Name,
  Namespace,
  None,
  Optional,
  Pair,
  Right,
  Unit,
  fromOptional,
  isGiven,
  isLeft,
  isRight,
} from "../../../main/typescript/hydra/runtime.js";

describe("core", () => {
  it("wraps Name and Namespace", () => {
    // `Name`/`Namespace` are now plain `{value: string}` records so the
    // shape matches the generated kernel `core.Name`. Branded variants
    // were dropped in the runtime.ts split because the brand mismatch
    // produced canonical-map-key drift between hand-written and
    // generated code paths.
    const n: Name = Name("hydra.core.Term");
    expect(n.value).toBe("hydra.core.Term");

    const ns: Namespace = Namespace("hydra.core");
    expect(ns.value).toBe("hydra.core");
  });

  it("constructs and inspects Optional", () => {
    const m: Optional<number> = Given(42);
    expect(isGiven(m)).toBe(true);
    expect(fromOptional(0, m)).toBe(42);

    const n: Optional<number> = None;
    expect(isGiven(n)).toBe(false);
    expect(fromOptional(0, n)).toBe(0);
  });

  it("constructs and inspects Either", () => {
    const l: Either<string, number> = Left("oops");
    expect(isLeft(l)).toBe(true);
    expect(isRight(l)).toBe(false);

    const r: Either<string, number> = Right(7);
    expect(isLeft(r)).toBe(false);
    expect(isRight(r)).toBe(true);
  });

  it("constructs Pair", () => {
    const p = Pair("k", 3);
    expect(p.fst).toBe("k");
    expect(p.snd).toBe(3);
  });

  it("has a frozen Unit", () => {
    expect(Object.isFrozen(Unit)).toBe(true);
    expect(Unit._brand).toBe("Unit");
  });
});
