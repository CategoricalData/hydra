// Hand-written runtime: hydra.lib.files primitives.
//
// Node.js file I/O using synchronous `node:fs` APIs. The effect type is
// transparent in TypeScript (effect<t> = t), so these helpers perform real
// I/O eagerly and return Either<FileError, T>. A recoverable file-system
// failure becomes Left(classified FileError); success becomes Right(value).
//
// `binary` values are base64-encoded strings (the TS runtime convention from
// literals.ts). `FilePath` is a wrapped `{ value: string }` record.
// `FileError` and `FilePath` types are produced by code generation; we use
// `any` here to stay decoupled from the generated dist.

import {
  appendFileSync,
  copyFileSync,
  cpSync,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  statSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import type { Either } from "../runtime.js";
import { Left, Right, Unit } from "../runtime.js";

// ---- Helpers ---------------------------------------------------------------

function classify(path: string, e: unknown): any {
  const code = (e as NodeJS.ErrnoException).code;
  if (code === "EEXIST")                               return { _type: "alreadyExists", value: { value: path } };
  if (code === "ENOENT" || code === "ENOTDIR")        return { _type: "notFound",       value: { value: path } };
  if (code === "EACCES" || code === "EPERM")          return { _type: "permissionDenied", value: { value: path } };
  if (code === "ENAMETOOLONG" || code === "EINVAL")   return { _type: "invalidPath",    value: message(e) };
  return { _type: "other", value: message(e) };
}

function message(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}

function withFileError<T>(path: string, action: () => T): Either<any, T> {
  try {
    return Right(action());
  } catch (e) {
    return Left(classify(path, e));
  }
}

function base64ToBuffer(b64: string): Buffer {
  return Buffer.from(b64, "base64");
}

function bufferToBase64(buf: Buffer): string {
  return buf.toString("base64");
}

// ---- Primitives ------------------------------------------------------------

// appendFile : FilePath -> binary -> effect<either<FileError, unit>>
export const appendFile = (path: any, contents: string): Either<any, typeof Unit> =>
  withFileError(path.value, () => {
    appendFileSync(path.value, base64ToBuffer(contents));
    return Unit;
  });

// copy : boolean -> FilePath -> FilePath -> effect<either<FileError, unit>>
export const copy = (recursive: boolean, source: any, destination: any): Either<any, typeof Unit> =>
  withFileError(source.value, () => {
    if (recursive) {
      cpSync(source.value, destination.value, { recursive: true });
    } else {
      copyFileSync(source.value, destination.value);
    }
    return Unit;
  });

// createDirectory : boolean -> FilePath -> effect<either<FileError, unit>>
export const createDirectory = (recursive: boolean, path: any): Either<any, typeof Unit> =>
  withFileError(path.value, () => {
    mkdirSync(path.value, { recursive });
    return Unit;
  });

// exists : FilePath -> effect<either<FileError, boolean>>
export const exists = (path: any): Either<any, boolean> =>
  withFileError(path.value, () => existsSync(path.value));

// listDirectory : FilePath -> effect<either<FileError, list<FilePath>>>
export const listDirectory = (path: any): Either<any, readonly any[]> =>
  withFileError(path.value, () =>
    readdirSync(path.value).map((name: string) => ({ value: name })));

// readFile : FilePath -> effect<either<FileError, binary>>
export const readFile = (path: any): Either<any, string> =>
  withFileError(path.value, () => bufferToBase64(readFileSync(path.value)));

// removeDirectory : boolean -> FilePath -> effect<either<FileError, unit>>
export const removeDirectory = (recursive: boolean, path: any): Either<any, typeof Unit> =>
  withFileError(path.value, () => {
    rmSync(path.value, { recursive, force: false });
    return Unit;
  });

// removeFile : FilePath -> effect<either<FileError, unit>>
export const removeFile = (path: any): Either<any, typeof Unit> =>
  withFileError(path.value, () => {
    unlinkSync(path.value);
    return Unit;
  });

// rename : FilePath -> FilePath -> effect<either<FileError, unit>>
export const rename = (source: any, destination: any): Either<any, typeof Unit> =>
  withFileError(source.value, () => {
    renameSync(source.value, destination.value);
    return Unit;
  });

// status : FilePath -> effect<either<FileError, FileStatus>>
// FileStatus is a generated type; we construct the tagged shape directly.
export const status = (path: any): Either<any, any> =>
  withFileError(path.value, () => {
    const s = statSync(path.value);
    const fileType = s.isDirectory()         ? { _type: "directory" }
                   : s.isFile()              ? { _type: "regular" }
                   : s.isSymbolicLink()      ? { _type: "symbolicLink" }
                   : s.isBlockDevice()       ? { _type: "blockSpecial" }
                   : s.isCharacterDevice()   ? { _type: "characterSpecial" }
                   : s.isFIFO()              ? { _type: "fifo" }
                   : s.isSocket()            ? { _type: "socket" }
                   :                          { _type: "other" };
    return {
      type: fileType,
      size: s.size,
      modificationTime: s.mtimeMs / 1000,
    };
  });

// writeFile : FilePath -> binary -> effect<either<FileError, unit>>
export const writeFile = (path: any, contents: string): Either<any, typeof Unit> =>
  withFileError(path.value, () => {
    writeFileSync(path.value, base64ToBuffer(contents));
    return Unit;
  });
