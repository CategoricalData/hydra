import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["src/test/typescript/**/*.test.ts"],
    globals: false,
    testTimeout: 30000,
  },
});
