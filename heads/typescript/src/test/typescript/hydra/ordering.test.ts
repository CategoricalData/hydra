import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

import { compareValues } from "../../../../../../dist/typescript/hydra-kernel/src/main/typescript/hydra/core/overlay/typescript/lib/ordering.js";

// Regression test for #827: ordering.ts keeps a hand-authored table of declared
// variant order for the hydra.core.model unions, which had drifted from the kernel
// (Term's inject/typeApplication/typeLambda positions). This test derives the
// declared order from the kernel's model.json, so the table cannot drift again.

const MODEL_JSON = new URL(
  "../../../../../../dist/json/hydra-kernel/src/main/json/hydra/core/model.json",
  import.meta.url,
);

const unwrap = (t: any): any => {
  for (;;) {
    if (t.annotated) t = t.annotated.body;
    else if (t.forall) t = t.forall.body;
    else if (t.variables && t.body) t = t.body;
    else return t;
  }
};

const declaredVariants = (): Record<string, string[]> => {
  const model = JSON.parse(readFileSync(MODEL_JSON, "utf8"));
  const out: Record<string, string[]> = {};
  for (const def of model.definitions) {
    const short = def.type.name.split(".").pop() as string;
    const family = { Term: "term", Type: "type", Literal: "literal", IntegerType: "integer", FloatType: "float" }[short];
    if (family) out[family] = unwrap(def.type.body).union.map((f: any) => f.name);
  }
  return out;
};

// A representative value of every variant, tagged as the TypeScript coder emits it.
const sample = (tag: string) => ({ tag, value: 0 });

describe("compareValues: declared variant order (#827)", () => {
  const families = declaredVariants();

  for (const [family, variants] of Object.entries(families)) {
    it(`orders every ${family} variant by its declared position`, () => {
      expect(variants.length).toBeGreaterThan(1);
      for (let i = 0; i < variants.length; i++) {
        for (let j = 0; j < variants.length; j++) {
          const c = compareValues(sample(variants[i]), sample(variants[j]));
          expect(Math.sign(c), `${family}: ${variants[i]} vs ${variants[j]}`).toBe(Math.sign(i - j));
        }
      }
    });
  }

  it("sorts a shuffled set of terms spanning every variant into declared order", () => {
    const declared = families.term;
    const shuffled = [...declared].reverse().map(sample);
    shuffled.sort(compareValues);
    expect(shuffled.map((t) => t.tag)).toEqual(declared);
  });
});
