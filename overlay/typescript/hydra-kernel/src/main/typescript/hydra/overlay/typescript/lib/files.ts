// Hand-written runtime: hydra.lib.files primitives.
//
// Real file-system primitives via synchronous Node.js APIs. The effect type is
// transparent in TypeScript (effect<t> = t), so each primitive performs its I/O
// eagerly and returns an Either<FileError, T>: a recoverable failure is Left(error),
// success is Right(value). Mirrors heads/python/.../lib/files.py and Hydra.Lib.Files,
// including the classify() mapping of OS errors to FileError variants. In the TS
// runtime `binary` values (file contents) are base64-encoded strings (the convention
// from literals.ts / system.ts), pairing with hydra.lib.text.decodeUtf8. `FileError`,
// `FilePath` are generated types; we use `any` to stay decoupled from the generated dist.
// For #494/#507.

import * as fs from "node:fs";
import type { Either } from "../../../runtime.js";
import { Left, Right, Unit } from "../../../runtime.js";

// Map a Node filesystem error to a FileError tagged variant by its `code`, mirroring
// the Python errno classification in files.py:_classify.
function classify(path: any, e: unknown): any {
  const code = (e as NodeJS.ErrnoException).code;
  if (code === "EEXIST")                                      return { tag: "alreadyExists", value: path };
  if (code === "ENOENT" || code === "ENOTDIR")               return { tag: "notFound", value: path };
  if (code === "EACCES" || code === "EPERM")                 return { tag: "permissionDenied", value: path };
  if (code === "ENAMETOOLONG" || code === "EINVAL" || code === "EISDIR")
    return { tag: "invalidPath", value: e instanceof Error ? e.message : String(e) };
  return { tag: "other", value: e instanceof Error ? e.message : String(e) };
}

// Run an I/O action, returning Right on success or Left(classified error) on failure.
function withFileError<A>(path: any, action: () => A): Either<any, A> {
  try {
    return Right(action());
  } catch (e) {
    return Left(classify(path, e));
  }
}

// contents is binary (a base64-encoded string); decode to raw bytes before writing.
export const appendFile = (path: any, contents: string): Either<any, any> =>
  withFileError(path, () => {
    fs.appendFileSync(path.value, Buffer.from(contents, "base64"));
    return Unit;
  });

export const createDirectory = (recursive: boolean, path: any): Either<any, any> =>
  withFileError(path, () => {
    fs.mkdirSync(path.value, { recursive });
    return Unit;
  });

export const exists = (path: any): Either<any, boolean> =>
  withFileError(path, () => fs.existsSync(path.value));

export const listDirectory = (path: any): Either<any, readonly any[]> =>
  withFileError(path, () => fs.readdirSync(path.value).map((name) => ({ value: name })));

// readFile returns binary as a base64-encoded string, pairing with text.decodeUtf8.
export const readFile = (path: any): Either<any, string> =>
  withFileError(path, () => fs.readFileSync(path.value).toString("base64"));

export const removeFile = (path: any): Either<any, any> =>
  withFileError(path, () => {
    fs.rmSync(path.value);
    return Unit;
  });

export const rename = (source: any, destination: any): Either<any, any> =>
  withFileError(source, () => {
    fs.renameSync(source.value, destination.value);
    return Unit;
  });

// contents is binary (a base64-encoded string); decode to raw bytes before writing.
export const writeFile = (path: any, contents: string): Either<any, any> =>
  withFileError(path, () => {
    fs.writeFileSync(path.value, Buffer.from(contents, "base64"));
    return Unit;
  });
