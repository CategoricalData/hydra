import { describe, expect, it } from "vitest";

import {
  Either,
  Just,
  Left,
  Maybe,
  Name,
  Namespace,
  Nothing,
  Pair,
  Right,
  Unit,
  fromMaybe,
  isJust,
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

  it("constructs and inspects Maybe", () => {
    const m: Maybe<number> = Just(42);
    expect(isJust(m)).toBe(true);
    expect(fromMaybe(0, m)).toBe(42);

    const n: Maybe<number> = Nothing;
    expect(isJust(n)).toBe(false);
    expect(fromMaybe(0, n)).toBe(0);
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
