// Hand-written runtime: hydra.lib.text primitives.
//
// UTF-8 codec primitives bridging Hydra strings and raw bytes (binary). These pair
// with hydra.lib.files.readFile / writeFile and hydra.lib.system (ProcessResult
// stdout/stderr), which are byte-oriented. In the TS runtime `binary` values are
// represented as base64-encoded strings (the convention from literals.ts / files.ts /
// system.ts), so decodeUtf8 takes the base64 form. Mirrors heads/python/.../lib/text.py
// and Hydra.Lib.Text. For #494/#507.

import type { Either } from "../../../runtime.js";
import { Left, Right } from "../../../runtime.js";

// Decode binary (a base64-encoded string) as UTF-8 text. Returns Right(text) on
// success, or Left(message) if the underlying bytes are not valid UTF-8. Node's
// TextDecoder with `fatal: true` throws on malformed input, mirroring Python's
// UnicodeDecodeError.
export const decodeUtf8 = (data: string): Either<string, string> => {
  try {
    const bytes = Buffer.from(data, "base64");
    const decoder = new TextDecoder("utf-8", { fatal: true });
    return Right(decoder.decode(bytes));
  } catch (e) {
    return Left(e instanceof Error ? e.message : String(e));
  }
};

// Encode text as binary (a base64-encoded string of its UTF-8 bytes). Total: every
// Hydra string is valid Unicode.
export const encodeUtf8 = (text: string): string =>
  Buffer.from(text, "utf-8").toString("base64");
