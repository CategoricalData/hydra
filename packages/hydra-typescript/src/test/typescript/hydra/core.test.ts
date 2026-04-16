import { describe, it, expect } from "vitest";
import {
  Name, just, nothing, left, right,
} from "../../../main/typescript/hydra/core.js";

describe("Name", () => {
  it("creates a branded string", () => {
    const n = Name("hydra.core.Term");
    expect(n).toBe("hydra.core.Term");
  });
});

describe("Maybe", () => {
  it("creates just values", () => {
    const j = just(42);
    expect(j.tag).toBe("just");
    if (j.tag === "just") {
      expect(j.value).toBe(42);
    }
  });

  it("creates nothing values", () => {
    const n = nothing<number>();
    expect(n.tag).toBe("nothing");
  });
});

describe("Either", () => {
  it("creates left values", () => {
    const l = left<string, number>("error");
    expect(l.tag).toBe("left");
    if (l.tag === "left") {
      expect(l.value).toBe("error");
    }
  });

  it("creates right values", () => {
    const r = right<string, number>(42);
    expect(r.tag).toBe("right");
    if (r.tag === "right") {
      expect(r.value).toBe(42);
    }
  });
});
