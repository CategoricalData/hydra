import { describe, it, expect } from "vitest";
import * as types from "../../../main/typescript/hydra/dsl/types.js";
import * as terms from "../../../main/typescript/hydra/dsl/terms.js";

describe("types DSL", () => {
  it("constructs a string type", () => {
    const t = types.string_();
    expect(t.tag).toBe("literal");
    if (t.tag === "literal") {
      expect(t.value.tag).toBe("string");
    }
  });

  it("constructs a function type", () => {
    const t = types.func(types.string_(), types.int32());
    expect(t.tag).toBe("function");
  });

  it("constructs a list type", () => {
    const t = types.list(types.boolean());
    expect(t.tag).toBe("list");
  });

  it("constructs a record type", () => {
    const t = types.record([
      types.field("name", types.string_()),
      types.field("age", types.int32()),
    ]);
    expect(t.tag).toBe("record");
    if (t.tag === "record") {
      expect(t.value.length).toBe(2);
    }
  });

  it("constructs a union type", () => {
    const t = types.union([
      types.field("left", types.string_()),
      types.field("right", types.int32()),
    ]);
    expect(t.tag).toBe("union");
  });

  it("constructs a forall type", () => {
    const t = types.forall("a", types.list(types.variable("a")));
    expect(t.tag).toBe("forall");
  });
});

describe("terms DSL", () => {
  it("constructs a string literal", () => {
    const t = terms.string_("hello");
    expect(t.tag).toBe("literal");
    if (t.tag === "literal") {
      expect(t.value.tag).toBe("string");
    }
  });

  it("constructs an int32 literal", () => {
    const t = terms.int32(42);
    expect(t.tag).toBe("literal");
  });

  it("constructs a lambda", () => {
    const t = terms.lambda("x", terms.variable("x"));
    expect(t.tag).toBe("lambda");
  });

  it("constructs an application", () => {
    const t = terms.apply(
      terms.lambda("x", terms.variable("x")),
      terms.int32(42),
    );
    expect(t.tag).toBe("application");
  });

  it("constructs a record", () => {
    const t = terms.record("Person", [
      terms.field("name", terms.string_("Alice")),
      terms.field("age", terms.int32(30)),
    ]);
    expect(t.tag).toBe("record");
  });

  it("constructs a let expression", () => {
    const t = terms.let_(
      [{ name: "x", term: terms.int32(1) }],
      terms.variable("x"),
    );
    expect(t.tag).toBe("let");
  });
});
