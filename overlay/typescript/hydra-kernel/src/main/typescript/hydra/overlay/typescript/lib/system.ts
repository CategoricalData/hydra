// Hand-written runtime: hydra.lib.system primitives.
//
// Real system-interface primitives via synchronous Node.js APIs. The effect
// type is transparent in TypeScript (effect<t> = t), so these perform their
// effect eagerly. Fallible primitives return Either<SystemError, T>: a failure
// to launch (or perform the call) is Left(classified SystemError); success is
// Right(value). Infallible primitives (getEnvironment, getEnvironmentVariable,
// getTime) return their value directly. Mirrors the Python reference in
// overlay/python/hydra-kernel/.../hydra/python/lib/system.py and the Haskell
// reference Hydra.Haskell.Lib.System. For #498.
//
// `binary` values (ProcessResult.stdout/stderr) are base64-encoded strings (the
// TS runtime convention from literals.ts / files.ts), so they pair with
// hydra.lib.text.decodeUtf8. `SystemError`, `Command`, `ProcessResult`, etc. are
// generated types; we use `any` here to stay decoupled from the generated dist.

import { spawnSync } from "node:child_process";
import process from "node:process";
import type { Either, Optional } from "../../../runtime.js";
import { Left, Right, Given, None } from "../../../runtime.js";

// ---- Helpers ---------------------------------------------------------------

function bufferToBase64(buf: Buffer | Uint8Array | null | undefined): string {
  return Buffer.from(buf ?? Buffer.alloc(0)).toString("base64");
}

// Classify a spawn launch failure (errno) into a SystemError tagged variant.
function classifyLaunch(program: any, e: unknown): any {
  const code = (e as NodeJS.ErrnoException).code;
  if (code === "ENOENT")                       return { tag: "commandNotFound", value: program };
  if (code === "EACCES" || code === "EPERM")   return { tag: "permissionDenied", value: program };
  if (code === "ENOTDIR")                      return { tag: "invalidWorkingDirectory", value: program };
  if (code === "EINTR")                        return { tag: "interrupted" };
  return { tag: "other", value: e instanceof Error ? e.message : String(e) };
}

// ---- Primitives ------------------------------------------------------------

// execute : Command -> effect<either<SystemError, ProcessResult>>
// Runs a program to completion and captures its result. A child that runs and
// exits non-zero is Right(result) with that exitCode; only a launch failure is
// Left(error). No shell is invoked; the program is executed directly.
export const execute = (command: any): Either<any, any> => {
  const program: string = command.program.value;
  const args: string[] = Array.from(command.arguments_ ?? []);
  const cwd = command.workingDirectory?.tag === "given"
    ? command.workingDirectory.value.value
    : undefined;
  let env: Record<string, string> | undefined = undefined;
  if (command.environment?.tag === "given") {
    env = {};
    for (const [k, v] of command.environment.value) env[k.value] = v;
  }
  const r = spawnSync(program, args, {
    cwd,
    env,
    shell: false,
    windowsHide: true,
  });
  if (r.error) {
    return Left(classifyLaunch(command.program, r.error));
  }
  return Right({
    exitCode: { value: r.status ?? 0 },
    stdout: bufferToBase64(r.stdout),
    stderr: bufferToBase64(r.stderr),
  });
};

// exit : StatusCode -> effect<unit>
// Terminate the current process with the given status code. Does not return.
export const exit = (code: any): never => {
  process.exit(code.value);
};

// getEnvironment : effect<map<EnvironmentVariable, string>>
// The full environment, as a map from variable name to value.
export const getEnvironment = (): ReadonlyMap<any, string> => {
  const m = new Map<any, string>();
  for (const [k, v] of Object.entries(process.env)) {
    if (v !== undefined) m.set({ value: k }, v);
  }
  return m;
};

// getEnvironmentVariable : EnvironmentVariable -> effect<optional<string>>
// Look up a single environment variable by name; none if it is not set.
export const getEnvironmentVariable = (name: any): Optional<string> => {
  const value = process.env[name.value];
  return value === undefined ? None : Given(value);
};

// getTime : effect<Timespec>
// The current wall-clock time as a Timespec (seconds and nanoseconds since the
// Unix epoch). Both seconds and nanoseconds are bigint, matching the generated
// Timespec type (int64 and uint32 both encode as bigint in TypeScript).
export const getTime = (): any => {
  const nanosTotal = BigInt(Math.round(Date.now())) * 1_000_000n;
  // Date.now() gives wall-clock ms since epoch; convert to seconds + nanoseconds.
  const seconds = nanosTotal / 1_000_000_000n;
  const nanoseconds = nanosTotal % 1_000_000_000n;
  return { seconds, nanoseconds };
};

// getWorkingDirectory : effect<either<SystemError, FilePath>>
// The current working directory as a FilePath.
export const getWorkingDirectory = (): Either<any, any> => {
  try {
    return Right({ value: process.cwd() });
  } catch (e) {
    return Left({ tag: "other", value: e instanceof Error ? e.message : String(e) });
  }
};
