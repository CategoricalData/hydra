// Hand-written runtime: hydra.lib.text primitives.
//
// UTF-8 codec bridging Hydra strings and raw bytes. In the TypeScript runtime,
// `binary` values are base64-encoded strings (matching the literals.ts convention).
// Pairs with hydra.lib.files.readFile / writeFile, which are byte-oriented.

import type { Either } from "../runtime.js";
import { Left, Right } from "../runtime.js";

// decodeUtf8 : binary -> either<string, string>
// Accepts a base64-encoded string (the TS binary representation), decodes it to
// raw bytes, then decodes those bytes as UTF-8. Returns Right(text) on success,
// or Left(message) if the bytes are not valid UTF-8.
export const decodeUtf8 = (data: string): Either<string, string> => {
  try {
    const raw = typeof atob !== "undefined"
      ? atob(data)
      : Buffer.from(data, "base64").toString("binary");
    const bytes = new Uint8Array(raw.length);
    for (let i = 0; i < raw.length; i++) bytes[i] = raw.charCodeAt(i);
    const decoder = new TextDecoder("utf-8", { fatal: true });
    return Right(decoder.decode(bytes));
  } catch (e) {
    return Left(e instanceof Error ? e.message : String(e));
  }
};

// encodeUtf8 : string -> binary
// Total: every Hydra string is valid Unicode and always encodes.
// Returns a base64-encoded string (the TS binary representation).
export const encodeUtf8 = (text: string): string => {
  const encoder = new TextEncoder();
  const bytes = encoder.encode(text);
  let raw = "";
  for (const b of bytes) raw += String.fromCharCode(b);
  return typeof btoa !== "undefined"
    ? btoa(raw)
    : Buffer.from(raw, "binary").toString("base64");
};
