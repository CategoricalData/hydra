import { describe, expect, it } from "vitest";

import { loadAll } from "../../../../../../../dist/typescript/hydra-kernel/src/test/typescript/hydra/test/jsonBindings.js";

// bug_564: docs/json-format.md documents a "compact string form" for union
// variants with Unit payload — e.g. Type.unit / Type.void encode as the
// bare JSON string "unit"/"void" instead of {"unit": {}}. jsonBindings.ts's
// convert() must normalize that bare string to {tag: "unit"}/{tag: "void"},
// not pass it through unmodified (which produced a raw string that failed
// every `.tag`-based switch downstream, e.g. rewriting.ts's Type-strip
// switch). hydra.core's own Type union definition, loaded via
// loadAll()'s TYPE_NAMESPACES, exercises this path directly: its `unit`
// and `void` field types are each themselves annotated(unit) in the
// kernel JSON.
describe("jsonBindings compact string form (bug_564)", () => {
  it("normalizes Type.unit's compact JSON form to {tag: unit}, not a bare string", () => {
    const { types } = loadAll();
    const typeScheme = types.find(([name]) => name.value === "hydra.core.Type");
    expect(typeScheme).toBeDefined();

    const body = typeScheme![1].body as unknown;
    const unwrapAnnotated = (t: any): any => (t?.tag === "annotated" ? unwrapAnnotated(t.value.body) : t);
    const union = unwrapAnnotated(body);
    expect(union.tag).toBe("union");

    const fields = union.value as ReadonlyArray<{ name: { value: string }; type_: unknown }>;
    const unitField = fields.find((f) => f.name.value === "unit");
    const voidField = fields.find((f) => f.name.value === "void");
    expect(unitField).toBeDefined();
    expect(voidField).toBeDefined();

    // Each field's type_ is `annotated(unit)` in the kernel JSON, where the
    // annotated body is the compact-string form "unit". Before the fix,
    // convert() passed that bare string through unchanged; the assertion
    // below fails against the pre-fix behavior (body === "unit", a string).
    const unitFieldBody = unwrapAnnotated(unitField!.type_);
    const voidFieldBody = unwrapAnnotated(voidField!.type_);
    expect(unitFieldBody).toEqual({ tag: "unit" });
    expect(voidFieldBody).toEqual({ tag: "unit" });
  });
});
