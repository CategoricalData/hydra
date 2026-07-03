// Hand-written runtime: hydra.lib.hashing primitives.
//
// SHA-256 over raw bytes (binary). In the TS runtime `binary` values are represented
// as base64-encoded strings (the convention from literals.ts / files.ts / text.ts), so
// these helpers decode the base64 input, hash it, and (for sha256) re-encode the digest
// as base64. Pure and total. Pairs with hydra.lib.files.readFile to hash file contents.
// Mirrors overlay/python/.../lib/hashing.py and Hydra.Lib.Hashing. For #524.

import { createHash } from "node:crypto";

// Compute the SHA-256 digest of binary (a base64-encoded string), returned as a
// base64-encoded string (binary).
export const sha256 = (data: string): string =>
  createHash("sha256").update(Buffer.from(data, "base64")).digest("base64");

// Compute the SHA-256 digest of binary (a base64-encoded string) as a 64-character
// lowercase hexadecimal string.
export const sha256Hex = (data: string): string =>
  createHash("sha256").update(Buffer.from(data, "base64")).digest("hex");
