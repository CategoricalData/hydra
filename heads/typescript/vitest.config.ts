import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: [
      "src/test/**/*.test.ts",
      "src/gen-test/**/*.test.ts",
    ],
  },
});
